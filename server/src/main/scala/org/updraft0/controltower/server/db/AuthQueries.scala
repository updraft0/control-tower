package org.updraft0.controltower.server.db

import io.getquill.*
import io.getquill.extras.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.query.*
import zio.*

import java.util.UUID

/** Queries for user/auth related operations
  */
object AuthQueries:
  import ctx.*
  import auth.given
  import auth.schema.*

  @scala.annotation.unused // ??
  private inline val ThirtyDaysInSeconds = 30 * 24 * 60

  // note: this ignores session expiry
  private inline def getSessionById(sessionId: UUID) =
    userSession.filter(_.sessionId == lift(sessionId))

  def getAllCharacterIds(): Result[List[CharacterId]] =
    run(quote(userCharacter.map(_.characterId)))

  def getCharacterByName(name: String): Result[Option[model.AuthCharacter]] =
    run(quote(character.filter(_.name == lift(name)))).map(_.headOption)

  def getUserCharactersById(
      userId: UserId
  ): Result[Option[(model.AuthUser, List[model.AuthCharacter], List[CharacterId])]] =
    run(quote {
      for
        user                <- user.filter(_.id == lift(userId))
        userCharacters      <- userCharacter.join(_.userId == user.id)
        characters          <- character.join(_.id == userCharacters.characterId)
        authTokenCharacters <- characterAuthToken
          .filter(_.updatedAt.exists(_ > unixepochMinusSeconds(ThirtyDaysInSeconds)))
          .leftJoin(_.characterId == userCharacters.characterId)
          .map(_.map(_.characterId))
      yield (user, characters, authTokenCharacters)
    }).map {
      case Nil => None
      case xs  => Some(xs.head._1, xs.map(_._2), xs.flatMap(_._3))
    }

  def getUserCharactersBySessionId(
      sessionId: UUID
  ): Result[Option[(model.UserSession, model.AuthUser, Option[model.UserPreference], List[model.AuthCharacter])]] =
    run(quote {
      for
        sess           <- getSessionById(sessionId)
        user           <- user.join(_.id == sess.userId)
        userCharacters <- userCharacter.join(_.userId == user.id)
        characters     <- character.join(_.id == userCharacters.characterId)
        pref           <- userPreference.leftJoin(_.userId == user.id)
      yield (sess, user, pref, characters)
    }).map {
      case Nil => None
      case xs  => Some(xs.head._1, xs.head._2, xs.head._3, xs.map(_._4))
    }

  def getUserByCharacterId(
      characterId: CharacterId
  ): Result[Option[(model.AuthUser, Option[model.UserPreference], model.AuthCharacter)]] =
    run(quote {
      (for
        charJoin <- userCharacter.filter(_.characterId == lift(characterId))
        user     <- user.join(_.id == charJoin.userId)
        char     <- character.join(_.id == charJoin.characterId)
        pref     <- userPreference.leftJoin(_.userId == charJoin.userId)
      yield (user, pref, char)).groupByMap((u, p, c) => (u, p, c))((u, p, c) => (u, p, c))
    }).map {
      case Nil      => None
      case h :: Nil => Some(h)
      case _        => None
    }

  def getSomeCharacterAuthToken(userId: UserId): Result[Option[(model.AuthCharacter, model.CharacterAuthToken)]] =
    run(quote {
      for
        uc  <- userCharacter.filter(uc => uc.userId == lift(userId))
        ac  <- character.join(ac => uc.characterId == ac.id)
        cat <- characterAuthToken
          .filter(_.expiresAt > unixepoch)
          .join(cat => cat.characterId == uc.characterId)
      yield (ac, cat)
    }).map(_.headOption)

  def getUserPreference(userId: UserId): Result[Option[model.UserPreference]] =
    run(quote { userPreference.filter(_.userId == lift(userId)) }).map {
      case Nil      => None
      case h :: Nil => Some(h)
      case _        => None
    }

  def getMapPoliciesForCharacter(
      characterIds: List[CharacterId]
  ): Result[Map[CharacterId, List[model.MapPolicyMember]]] =
    inline def userRoles(inline cids: Query[CharacterId]) =
      for
        char  <- character.filter(c => cids.contains(c.id))
        roles <- mapPolicyMember
          .join(m => infix"${m.memberId} = ${char.id}".asCondition)
          // .join(_.memberId == char.id) FIXME workaround against opaque type vs non-opaque join
          .filter(_.memberType == lift(model.PolicyMemberType.Character))
      yield char.id -> roles

    inline def userCorporationRoles(inline cids: Query[CharacterId]) =
      for
        char  <- character.filter(c => cids.contains(c.id))
        roles <- mapPolicyMember
          .join(m => infix"${m.memberId} = ${char.corporationId}".asCondition) // see comment above
          .filter(_.memberType == lift(model.PolicyMemberType.Corporation))
      yield char.id -> roles

    inline def userAllianceRoles(inline cids: Query[CharacterId]) =
      for
        char  <- character.filter(c => cids.contains(c.id))
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

  def updateUserPreference(pref: model.UserPreference): Result[Long] =
    run(
      quote(
        userPreference
          .insertValue(lift(pref))
          .onConflictUpdate(_.userId)((t, e) => t.preferenceJson -> e.preferenceJson)
      )
    )

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
