package org.updraft0.controltower.server.db

import io.getquill.*
import io.getquill.extras.*
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.query.*
import zio.*

import java.nio.file.Paths
import javax.sql.DataSource
import java.util.UUID

case class CharacterMapRole(characterId: Long, mapId: Long, role: model.MapRole)

/** Queries for user/auth related operations
  */
object AuthQueries:
  import ctx.{*, given}
  import auth.given
  import auth.schema.*

  private inline def getSessionById(sessionId: UUID) =
    quote {
      userSession
        .filter(_.sessionId == lift(sessionId))
        .filter(us => sql"${us.expiresAt} > (unixepoch('subsec') * 1000)".asCondition)
    }

  def getUserCharacterByIdAndCharId(
      userId: Long,
      characterId: Long
  ): Result[Option[(model.AuthUser, model.AuthCharacter)]] =
    getUserCharactersById(userId).map(_.flatMap((u, chars) => chars.find(_.id == characterId).map(c => (u -> c))))

  def getUserCharactersById(
      userId: Long
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

  def getUserByCharacterId(characterId: Long): Result[Option[(model.AuthUser, model.AuthCharacter)]] =
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

  def getMapPoliciesForCharacter(characterIds: List[Long]): Result[Map[Long, List[model.MapPolicyMember]]] =
    inline def userRoles(inline cids: List[Long]) = quote {
      for
        char <- character.filter(c => liftQuery(cids).contains(c.id))
        roles <- mapPolicyMember
          .join(_.memberId == char.id)
          .filter(_.memberType == lift(model.PolicyMemberType.Character))
      yield char.id -> roles
    }

    inline def userCorporationRoles(inline cids: List[Long]) = quote {
      for
        char <- character.filter(c => liftQuery(cids).contains(c.id))
        roles <- mapPolicyMember
          .join(_.memberId == char.corporationId)
          .filter(_.memberType == lift(model.PolicyMemberType.Corporation))
      yield char.id -> roles
    }

    inline def userAllianceRoles(inline cids: List[Long]) = quote {
      for
        char <- character.filter(c => liftQuery(cids).contains(c.id))
        roles <- mapPolicyMember
          .join(_.memberId === char.allianceId)
          .filter(_.memberType == lift(model.PolicyMemberType.Alliance))
      yield char.id -> roles
    }

    run(quote {
      userRoles(characterIds).distinct
        .union(userCorporationRoles(characterIds))
        .union(userAllianceRoles(characterIds))
    }).map(_.groupBy(_._1).view.mapValues(_.map(_._2)).toMap)

  def createMapPolicy(mapId: Long, userId: Long): Result[Unit] =
    run(quote {
      mapPolicy.insert(_.mapId -> lift(mapId), _.createdBy -> lift(userId))
    }).unit

  def createMapPolicyMembers(policyMembers: List[model.MapPolicyMember]): Result[Int] =
    run(quote {
      liftQuery(policyMembers).foreach(pm => mapPolicyMember.insertValue(pm))
    }).map(_.size)
