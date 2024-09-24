package org.updraft0.controltower.server.endpoints

import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.db.model
import org.updraft0.controltower.protocol
import org.updraft0.controltower.protocol.Endpoints
import org.updraft0.controltower.server.Server.EndpointEnv
import org.updraft0.controltower.server.auth.*
import org.updraft0.controltower.server.db.AuthQueries
import org.updraft0.controltower.server.tracking.CharacterAuthTracker
import sttp.tapir.ztapir.*
import zio.{Config as _, *}

enum UserSessionError derives CanEqual:
  case InvalidCookie, DbQueryFailure, SessionNotFound, SessionExpired

def validateSessionString(
    cookie: protocol.SessionCookie
): ZIO[SessionCrypto & javax.sql.DataSource & UserSession, String, LoggedInUser] =
  validateSession(cookie).mapError:
    case UserSessionError.InvalidCookie   => "invalid cookie"
    case UserSessionError.SessionNotFound => "session not found"
    case UserSessionError.SessionExpired  => "session expired"
    case UserSessionError.DbQueryFailure  => "internal error"

def validateSession(
    cookie: protocol.SessionCookie
): ZIO[SessionCrypto & javax.sql.DataSource & UserSession, UserSessionError, LoggedInUser] =
  for
    now           <- ZIO.clockWith(_.instant)
    sessionCookie <- validateCookie(cookie)
    authResult <- AuthQueries
      .getUserCharactersBySessionId(sessionCookie.id)
      .tapErrorCause(ZIO.logWarningCause("failed query", _))
      .orElseFail(UserSessionError.DbQueryFailure)
    _ <- ZIO.fail(UserSessionError.SessionNotFound).when(authResult.isEmpty)
    _ <- ZIO.fail(UserSessionError.SessionExpired).when(authResult.exists(_._1.expiresAt.isBefore(now)))
    (_, user, _) = authResult.get
    user <- UserSession.setUser(LoggedInUser(user.id, sessionCookie.id))
  yield user

private inline def validateCookie(cookie: protocol.SessionCookie) =
  ZIO
    .fromEither(SessionCookie.from(cookie))
    .tapError(msg => ZIO.logWarning(s"Invalid session cookie: $msg"))
    .mapError(_ => UserSessionError.InvalidCookie)

def getUserInfo = Endpoints.getUserInfo
  .zServerSecurityLogic(validateSessionString)
  .serverLogic(user =>
    _ =>
      AuthQueries
        .getUserCharactersById(user.userId)
        .tapErrorCause(ZIO.logWarningCause("failed query", _))
        .orElseFail("Failure while trying to find user")
        .flatMap {
          case None => ZIO.fail("No user found")
          case Some(user, characters) =>
            MapPolicy
              .getMapsForCharacters(characters.map(_.id))
              .tapErrorCause(ZIO.logWarningCause("failed map policy lookup", _))
              .mapBoth(_ => "Failure while trying to find map policies", toUserInfo(user, characters, _))
        }
  )

def logoutUserCharacter = Endpoints.logoutUserCharacter
  .zServerSecurityLogic(validateSessionString)
  .serverLogic(user =>
    characterId =>
      Users
        .logoutCharacterFromUser(user.sessionId, characterId)
        .zipLeft(ZIO.serviceWithZIO[CharacterAuthTracker](_.logout(characterId)))
        .tapErrorCause(ZIO.logWarningCause("failed query", _))
        .orElseFail("Failure while trying to logout user character")
        .flatMap {
          case true  => ZIO.unit
          case false => ZIO.fail("Unable to remove")
        }
  )

def allUserEndpoints: List[ZServerEndpoint[EndpointEnv, Any]] =
  List(
    getUserInfo.widen[EndpointEnv],
    logoutUserCharacter.widen[EndpointEnv]
  )

private def toUserInfo(
    user: model.AuthUser,
    characters: List[model.AuthCharacter],
    mapsPerCharacter: Map[CharacterId, List[(model.MapModel, model.MapRole)]]
): protocol.UserInfo =
  protocol.UserInfo(
    userId = user.id,
    displayName = user.displayName,
    characters = characters.map { ac =>
      protocol.UserCharacter(
        name = ac.name,
        characterId = ac.id,
        corporationId = ac.corporationId,
        allianceId = ac.allianceId
      )
    },
    maps = toUserCharacterMaps(mapsPerCharacter)
  )

private def toUserCharacterMaps(mapsPerCharacter: Map[CharacterId, List[(model.MapModel, model.MapRole)]]) =
  mapsPerCharacter
    .flatMap((characterId, mapModels) =>
      mapModels.map((map, role) => protocol.UserCharacterMap(characterId, map.id, map.name, toProtocol(role)))
    )
    .toList

private def toProtocol(role: model.MapRole) =
  role match
    case model.MapRole.Editor => protocol.MapRole.Editor
    case model.MapRole.Viewer => protocol.MapRole.Viewer
    case model.MapRole.Admin  => protocol.MapRole.Admin
