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

def validateSession(
    cookie: protocol.SessionCookie
): ZIO[SessionCrypto & javax.sql.DataSource & UserSession, String, LoggedInUser] =
  ZIO
    .fromEither(SessionCookie.from(cookie))
    .tapError(msg => ZIO.logWarning(s"Invalid session cookie: $msg"))
    .foldZIO(
      _ => ZIO.fail("Cookie is not valid"),
      sc =>
        AuthQueries
          .getUserCharactersBySessionId(sc.id)
          .tapErrorCause(ZIO.logWarningCause("failed query", _))
          .orElseFail("Failure while trying to find session")
          .flatMap {
            case None               => ZIO.fail("Session is not valid")
            case Some((_, user, _)) => UserSession.setUser(LoggedInUser(user.id, sc.id))
          }
    )

def getUserInfo = Endpoints.getUserInfo
  .zServerSecurityLogic(validateSession)
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
  .zServerSecurityLogic(validateSession)
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
