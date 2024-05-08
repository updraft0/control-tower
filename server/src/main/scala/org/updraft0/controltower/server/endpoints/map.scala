package org.updraft0.controltower.server.endpoints

import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.db.{model, query as dbquery}
import org.updraft0.controltower.protocol
import org.updraft0.controltower.protocol.{SessionCookie as _, *}
import org.updraft0.controltower.server.Server.EndpointEnv
import org.updraft0.controltower.server.auth.*
import org.updraft0.controltower.server.db.{AuthQueries, MapQueries}
import org.updraft0.controltower.server.map.{MapSessionManager, MapSession}
import sttp.capabilities.zio.ZioStreams
import sttp.tapir.ztapir.*
import zio.{Config as _, *}

case class ValidationError(msg: String) extends RuntimeException(msg)

def createMap = Endpoints.createMap
  .zServerSecurityLogic(validateSession)
  .serverLogic(user =>
    newMap =>
      dbquery
        .transaction(
          for
            _   <- checkNewMapCharacterMatches(user.userId, newMap)
            map <- insertNewMap(user.userId, newMap)
          yield toMapInfo(map) // cannot determine map role here because we have no character id associated
        )
        .tapErrorCause(ZIO.logWarningCause("failed while creating map", _))
        .mapError(toUserError)
  )

// NOTE: See comments in MapSession for why this doesn't use sttp's websocket interop
def mapWebSocket: zio.http.HttpApp[EndpointEnv] =
  import zio.http.*

  def validCookie(req: Request): ZIO[EndpointEnv, Response, LoggedInUser] =
    req
      .cookieWithOrFail(Endpoints.SessionCookieName)(
        Response.text("Missing session cookie").status(Status.BadRequest)
      )(cookie =>
        validateSession(protocol.SessionCookie(cookie.content)).mapError(msg =>
          Response.text(msg).status(Status.Unauthorized)
        )
      )

  Routes(
    Method.GET / "api" / "map" / string("mapName") / string("character") / "ws" ->
      handler { (mapName: String, character: String, req: Request) =>
        ZIO.scoped:
          for
            user         <- validCookie(req)
            characterOpt <- lookupCharacter(character)
            character <- characterOpt
              .map(ZIO.succeed)
              .getOrElse(ZIO.fail(Response.text("No character found").status(Status.NotFound)))
            sessionMsgs <- ZIO.serviceWithZIO[MapSessionManager](_.messages)
            mapTup <- checkUserCanAccessMap(user, character.id, mapName)
              .mapError(Response.text(_).status(Status.Unauthorized))
            (mapId, mapRole) = mapTup
            resp <- MapSession(mapId, character.id, user.userId, mapRole, sessionMsgs).toResponse
          yield resp
      }
  ).toHttpApp

def allMapEndpoints
    : List[ZServerEndpoint[EndpointEnv, sttp.capabilities.zio.ZioStreams & sttp.capabilities.WebSockets]] =
  List(
    createMap.widen[EndpointEnv]
  )

private def lookupCharacter(character: String) =
  AuthQueries
    .getCharacterByName(character)
    .tapError(ex => ZIO.logErrorCause("failed to get character by name", Cause.fail(ex)))
    .orElseFail(zio.http.Response.text("Database error").status(zio.http.Status.InternalServerError))

private def checkUserCanAccessMap(
    user: LoggedInUser,
    characterId: CharacterId,
    mapName: String
): ZIO[javax.sql.DataSource, String, (model.MapId, model.MapRole)] =
  dbquery
    .transaction {
      for
        maps     <- MapPolicy.allowedMapIdsForUser(user.userId).map(_.filter(_._1 == characterId))
        mapNames <- MapQueries.getMapNamesByIds(maps.map(_._2)).map(_.filter(_._2 == mapName))
        _        <- ZIO.when(mapNames.isEmpty)(ZIO.fail(ValidationError("No maps found")))
        _        <- ZIO.when(mapNames.size > 1)(ZIO.fail(ValidationError("Map name is ambiguous")))
      yield maps.find((c, m, _) => c == characterId && m == mapNames.head._1).map((_, m, r) => m -> r).head
    }
    .tapErrorCause(ZIO.logWarningCause("failed while checking user access", _))
    .mapError(toUserError)

private def checkNewMapCharacterMatches(userId: Long, newMap: NewMap) =
  for
    // note: we stopped checking whether a character matches because there is no need to link a character to a map (except through policy)
    mapAlreadyExists <- MapQueries.getMapByCreatorUserAndName(userId, newMap.name).map(_.isDefined)
    _                <- ZIO.when(mapAlreadyExists)(ZIO.fail(ValidationError("Map name not unique")))
  yield ()

private def insertNewMap(userId: model.UserId, newMap: NewMap) =
  for
    map <- MapQueries
      .createMap(userId, newMap.name, toModelMapDisplayType(newMap.displayType))
      .flatMap(MapQueries.getMap)
      .map(_.head)
    _ <- AuthQueries.createMapPolicy(map.id, userId)
    _ <- AuthQueries.createMapPolicyMembers(newMap.policyMembers.map(pm => toModelPolicyMember(map, userId, pm)).toList)
  yield map

def toMapInfo(map: model.MapModel): MapInfo = /* FIXME store settings */
  MapInfo(map.id, map.name, toMapDisplayType(map.displayType), MapSettings(12.hours), map.createdAt)

def toModelPolicyMember(map: model.MapModel, userId: model.UserId, m: MapPolicyMember): model.MapPolicyMember =
  model.MapPolicyMember(
    mapId = map.id,
    memberId = m.memberId,
    memberType = toMemberType(m.memberType),
    isDeny = m.isDeny,
    role = toMapRole(m.role),
    createdByUserId = userId,
    createdAt = map.createdAt,
    updatedByUserId = userId,
    updatedAt = map.createdAt
  )

def toMemberType(m: PolicyMemberType): model.PolicyMemberType = m match
  case PolicyMemberType.Corporation => model.PolicyMemberType.Corporation
  case PolicyMemberType.Alliance    => model.PolicyMemberType.Alliance
  case PolicyMemberType.Character   => model.PolicyMemberType.Character

def toMapDisplayType(dt: model.MapDisplayType): MapDisplayType = dt match
  case model.MapDisplayType.Manual => MapDisplayType.Manual

def toModelMapDisplayType(dt: MapDisplayType): model.MapDisplayType = dt match
  case MapDisplayType.Manual => model.MapDisplayType.Manual

def toMapRole(m: MapRole): model.MapRole = m match
  case MapRole.Viewer => model.MapRole.Viewer
  case MapRole.Editor => model.MapRole.Editor
  case MapRole.Admin  => model.MapRole.Admin

def toProtocolRole(m: model.MapRole): MapRole = m match
  case model.MapRole.Admin  => MapRole.Admin
  case model.MapRole.Editor => MapRole.Editor
  case model.MapRole.Viewer => MapRole.Viewer

def toUserError(t: Throwable) = t match
  case ValidationError(msg) => msg
  case ex                   => "Unknown error occurred"
