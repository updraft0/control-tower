package org.updraft0.controltower.server.db

import io.getquill.*
import io.getquill.extras.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.query.*
import zio.*

import javax.sql.DataSource
import java.util.UUID

case class CharacterMapRole(characterId: CharacterId, mapId: MapId, role: model.MapRole)

/** Queries for user/auth related operations
  */
object AuthQueries:
  import ctx.*
  import auth.given
  import auth.schema.*

  // note: this ignores session expiry
  private inline def getSessionById(sessionId: UUID) =
    userSession.filter(_.sessionId == lift(sessionId))

  def getAllCharacterIds(): Result[List[CharacterId]] =
    run(quote(userCharacter.map(_.characterId)))

  def getCharacterByName(name: String): Result[Option[model.AuthCharacter]] =
    run(quote(character.filter(_.name == lift(name)))).map(_.headOption)

  def getUserCharacterByIdAndCharId(
      userId: UserId,
      characterId: CharacterId
  ): Result[Option[(model.AuthUser, model.AuthCharacter)]] =
    getUserCharactersById(userId).map(_.flatMap((u, chars) => chars.find(_.id == characterId).map(c => (u -> c))))

  def getUserCharactersById(
      userId: UserId
  ): Result[Option[(model.AuthUser, List[model.AuthCharacter])]] =
    run(quote {
      for
        user           <- user.filter(_.id == lift(userId))
        userCharacters <- userCharacter.join(_.userId == user.id)
        characters     <- character.join(_.id == userCharacters.characterId)
      yield (user, characters)
    }).map {
      case Nil => None
      case xs  => Some(xs.head._1, xs.map(_._2))
    }

  def getUserCharactersBySessionId(
      sessionId: UUID
  ): Result[Option[(model.UserSession, model.AuthUser, List[model.AuthCharacter])]] =
    run(quote {
      for
        sess           <- getSessionById(sessionId)
        user           <- user.join(_.id == sess.userId)
        userCharacters <- userCharacter.join(_.userId == user.id)
        characters     <- character.join(_.id == userCharacters.characterId)
      yield (sess, user, characters)
    }).map {
      case Nil => None
      case xs  => Some(xs.head._1, xs.head._2, xs.map(_._3))
    }

  def getUserByCharacterId(characterId: CharacterId): Result[Option[(model.AuthUser, model.AuthCharacter)]] =
    run(quote {
      (for
        charJoin <- userCharacter.filter(_.characterId == lift(characterId))
        user     <- user.join(_.id == charJoin.userId)
        char     <- character.join(_.id == charJoin.characterId)
      yield (user, char)).groupByMap((u, c) => (u, c))((u, c) => (u, c))
    }).map {
      case Nil      => None
      case h :: Nil => Some(h)
      case _        => None
    }

  def getMapPoliciesForCharacter(
      characterIds: List[CharacterId]
  ): Result[Map[CharacterId, List[model.MapPolicyMember]]] =
    inline def userRoles(inline cids: Query[CharacterId]) =
      for
        char <- character.filter(c => cids.contains(c.id))
        roles <- mapPolicyMember
          .join(m => infix"${m.memberId} = ${char.id}".asCondition)
          // .join(_.memberId == char.id) FIXME workaround against opaque type vs non-opaque join
          .filter(_.memberType == lift(model.PolicyMemberType.Character))
      yield char.id -> roles

    inline def userCorporationRoles(inline cids: Query[CharacterId]) =
      for
        char <- character.filter(c => cids.contains(c.id))
        roles <- mapPolicyMember
          .join(m => infix"${m.memberId} = ${char.corporationId}".asCondition) // see comment above
          .filter(_.memberType == lift(model.PolicyMemberType.Corporation))
      yield char.id -> roles

    inline def userAllianceRoles(inline cids: Query[CharacterId]) =
      for
        char <- character.filter(c => cids.contains(c.id))
        roles <- mapPolicyMember
          .join(m => infix"${m.memberId} = ${char.allianceId}".asCondition) // see comment above
          .filter(_.memberType == lift(model.PolicyMemberType.Alliance))
      yield char.id -> roles

    run(quote {
      val cids = liftQuery(characterIds)
      userRoles(cids).distinct
        .union(userCorporationRoles(cids))
        .union(userAllianceRoles(cids))
    }).map(_.groupBy(_._1).view.mapValues(_.map(_._2)).toMap)

  def getMapAllowedCharactersRaw(mapId: MapId): Result[List[(model.PolicyMemberType, Long, Boolean, model.MapRole)]] =
    inline def characters(inline mid: MapId) = quote(
      mapPolicyMember
        .filter(mp => mp.mapId == lift(mid) && mp.memberType == lift(model.PolicyMemberType.Character))
        .map(mp => (mp.memberType, mp.memberId, mp.isDeny, mp.role))
    )
    inline def charactersInCorps(inline mid: MapId) = quote(
      mapPolicyMember
        .filter(mp => mp.mapId == lift(mid) && mp.memberType == lift(model.PolicyMemberType.Corporation))
        .join(character)
        .on((m, c) => infix"${m.memberId} = ${c.corporationId}".asCondition)
        .map((mp, c) => (mp.memberType, infix"${c.id}".as[Long], mp.isDeny, mp.role))
    )
    inline def charactersInAlliances(inline mid: MapId) = quote(
      mapPolicyMember
        .filter(mp => mp.mapId == lift(mid) && mp.memberType == lift(model.PolicyMemberType.Alliance))
        .join(character)
        .on((m, c) => c.allianceId.exists(allianceId => infix"${m.memberId} = $allianceId".asCondition))
        .map((mp, c) => (mp.memberType, infix"${c.id}".as[Long], mp.isDeny, mp.role))
    )

    run(
      quote(
        characters(mapId)
          .union(charactersInCorps(mapId))
          .union(charactersInAlliances(mapId))
      )
    )

  def getMapPolicy(mapId: MapId): Result[Option[model.MapPolicy]] =
    run(quote(mapPolicy.filter(_.mapId == lift(mapId)))).map(_.headOption)

  def getMapPolicyMembers(mapId: MapId): Result[List[model.MapPolicyMember]] =
    run(quote(mapPolicyMember.filter(_.mapId == lift(mapId))))

  // creates

  def createMapPolicy(mapId: MapId, userId: UserId): Result[Unit] =
    run(quote {
      mapPolicy.insert(_.mapId -> lift(mapId), _.createdByUserId -> lift(userId))
    }).unit

  def createMapPolicyMembers(policyMembers: List[model.MapPolicyMember]): Result[Int] =
    run(quote {
      liftQuery(policyMembers).foreach(pm => mapPolicyMember.insertValue(pm))
    }).map(_.size)

  // updates
  def updateMapPolicyMembers(mapId: MapId, policyMembers: List[model.MapPolicyMember]): Result[Long] =
    run(
      quote(
        liftQuery(policyMembers).foreach(mpm =>
          mapPolicyMember
            .filter(m => m.mapId == lift(mapId) && m.memberId == mpm.memberId && m.memberType == mpm.memberType)
            .update(
              _.isDeny          -> mpm.isDeny,
              _.role            -> mpm.role,
              _.updatedAt       -> mpm.updatedAt,
              _.updatedByUserId -> mpm.updatedByUserId
            )
        )
      )
    ).map(_.sum)

  def updateCharacterAffiliations(xs: List[(CharacterId, CorporationId, Option[AllianceId])]): Result[Long] =
    run(
      quote(
        liftQuery(xs).foreach((charId, corpId, allianceId) =>
          character
            .filter(_.id == charId)
            .update(_.allianceId -> allianceId, _.corporationId -> corpId, _.updatedAt -> Some(unixepoch))
        )
      )
    ).map(_.sum)

  // deletes
  def deleteMapPolicyMembers(mapId: MapId): Result[Long] =
    run(quote {
      mapPolicyMember.filter(_.mapId == lift(mapId)).delete
    })

  def deleteMapPolicyMembersByMemberIds(mapId: MapId, ids: List[(Long, model.PolicyMemberType)]): Result[Long] =
    run(
      quote(
        liftQuery(ids).foreach((id, mt) =>
          mapPolicyMember.filter(mpm => mpm.mapId == lift(mapId) && mpm.memberId == id && mpm.memberType == mt).delete
        )
      )
    ).map(_.sum)

  def deleteMapPolicy(mapId: MapId): Result[Long] =
    run(quote(mapPolicy.filter(_.mapId == lift(mapId)).delete))
