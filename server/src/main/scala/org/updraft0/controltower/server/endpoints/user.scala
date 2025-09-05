package org.updraft0.controltower.server.endpoints

import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.db.model
import org.updraft0.controltower.protocol
import org.updraft0.controltower.protocol.{Endpoints, UserPreferences}
import org.updraft0.controltower.server.Server.EndpointEnv
import org.updraft0.controltower.server.auth.*
import org.updraft0.controltower.server.db.AuthQueries
import org.updraft0.controltower.server.tracking.CharacterAuthTracker
import sttp.tapir.ztapir.*
import zio.{Config as _, *}
import com.github.plokhotnyuk.jsoniter_scala

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
    authResult    <- AuthQueries
      .getUserCharactersBySessionId(sessionCookie.id)
      .tapErrorCause(ZIO.logWarningCause("failed query", _))
      .orElseFail(UserSessionError.DbQueryFailure)
    _ <- ZIO.fail(UserSessionError.SessionNotFound).when(authResult.isEmpty)
    _ <- ZIO.fail(UserSessionError.SessionExpired).when(authResult.exists(_._1.expiresAt.isBefore(now)))
    (_, user, _, _) = authResult.get
    user <- UserSession.setUser(LoggedInUser(user.id, sessionCookie.id))
  yield user

private inline def validateCookie(cookie: protocol.SessionCookie) =
  ZIO
    .fromEither(SessionCookie.from(cookie))
    .tapError(msg => ZIO.logWarning(s"Invalid session cookie: $msg"))
    .orElseFail(UserSessionError.InvalidCookie)

def getUserInfo = Endpoints.getUserInfo
  .zServerSecurityLogic(validateSessionString)
  .serverLogic(user =>
    _ =>
      for
        usersById <- AuthQueries
          .getUserCharactersById(user.userId)
          .tapErrorCause(ZIO.logWarningCause("failed query", _))
          .orElseFail("Failure while trying to find user")
          .someOrFail("No user found")
        (user, characters, withAuthTokens) = usersById
        res <- MapPolicy
          .getMapsForCharacters(characters.map(_.id))
          .tapErrorCause(ZIO.logWarningCause("failed map policy lookup", _))
          .orElseFail("Failure while trying to find map policies")
        prefs <- AuthQueries
          .getUserPreference(user.id)
          .tapErrorCause(ZIO.logWarningCause("failed map policy lookup", _))
          .orElseFail("Failure while trying to load user preferences")
        preferences <- loadPreferences(prefs)
      yield toUserInfo(user, characters, withAuthTokens, res, preferences)
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

def updatePreferences = Endpoints.updatePreferences
  .zServerSecurityLogic(validateSessionString)
  .serverLogic(user =>
    prefs =>
      import protocol.jsoncodec.given
      val prefsJson = jsoniter_scala.core.writeToString(prefs)
      AuthQueries
        .updateUserPreference(model.UserPreference(user.userId, prefsJson))
        .tapErrorCause(ZIO.logWarningCause("failed query", _))
        .orElseFail("Failure while trying to update user preferences")
        .unit
  )

def allUserEndpoints: List[ZServerEndpoint[EndpointEnv, Any]] =
  List(
    getUserInfo.widen[EndpointEnv],
    updatePreferences.widen[EndpointEnv],
    logoutUserCharacter.widen[EndpointEnv]
  )

private def toUserInfo(
    user: model.AuthUser,
    characters: List[model.AuthCharacter],
    withAuthTokens: List[CharacterId],
    mapsPerCharacter: Map[CharacterId, List[(model.MapModel, model.MapRole)]],
    preferences: UserPreferences
): protocol.UserInfo =
  protocol.UserInfo(
    userId = user.id,
    displayName = user.displayName,
    characters = characters.map { ac =>
      protocol.UserCharacter(
        name = ac.name,
        characterId = ac.id,
        corporationId = ac.corporationId,
        allianceId = ac.allianceId,
        authTokenFresh = withAuthTokens.contains(ac.id)
      )
    },
    maps = toUserCharacterMaps(mapsPerCharacter),
    preferences = preferences
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

private def loadPreferences(prefsOpt: Option[model.UserPreference]): UIO[UserPreferences] =
  import protocol.jsoncodec.given
  prefsOpt match
    case None        => ZIO.succeed(UserPreferences.Default)
    case Some(prefs) =>
      ZIO
        .attempt(jsoniter_scala.core.readFromString[UserPreferences](prefs.preferenceJson))
        .logError("unable to deserialize preferences, falling back to defaults")
        .orElseSucceed(UserPreferences.Default)
