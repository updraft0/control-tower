package org.updraft0.controltower.server.endpoints

import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.{model, query as dbquery}
import org.updraft0.controltower.protocol
import org.updraft0.controltower.protocol.{SessionCookie as _, *}
import org.updraft0.controltower.server.Server.EndpointEnv
import org.updraft0.controltower.server.auth.*
import org.updraft0.controltower.server.db.{AuthQueries, MapQueries}
import org.updraft0.controltower.server.map.{MapPermissionTracker, MapSession}
import sttp.tapir.ztapir.*
import zio.{Config as _, *}
import java.time.Instant

case class ValidationError(msg: String)     extends RuntimeException(msg)
case class NoRolesToDeleteMap(mapId: MapId) extends RuntimeException("Insufficient roles to delete map")
case class NoRolesToGetMap(mapId: MapId)    extends RuntimeException("Insufficient roles to get map")
case class NoRolesToUpdateMap(mapId: MapId) extends RuntimeException("Insufficient roles to update map")
case class MapNotFound(mapId: MapId)        extends RuntimeException("Map was not found")

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

def getMap = Endpoints.getMap
  .zServerSecurityLogic(validateSession)
  .serverLogic(user =>
    userMapId =>
      dbquery
        .transaction(
          for
            allMaps <- MapPolicy.allowedMapIdsForUser(user.userId)
            withAdmin = allMaps.find((_, id, role) => id == userMapId && role == model.MapRole.Admin)
            _ <- ZIO.fail(NoRolesToGetMap(userMapId)).when(withAdmin.isEmpty)
            mapId = withAdmin.head._2
            mapModelOpt    <- MapQueries.getMap(mapId)
            _              <- ZIO.fail(MapNotFound(mapId)).when(mapModelOpt.isEmpty)
            memberPolicies <- AuthQueries.getMapPolicyMembers(mapId)
          yield MapInfoWithPermissions(toMapInfo(mapModelOpt.get), memberPolicies.map(toProtoPolicyMember).toArray)
        )
        .tapErrorCause(ZIO.logWarningCause("failed while getting map", _))
        .mapError(toUserError)
  )

def updateMap = Endpoints.updateMap
  .zServerSecurityLogic(validateSession)
  .serverLogic(user =>
    (userMapId, mapUpdate) =>
      dbquery
        .transaction(
          for
            allMaps <- MapPolicy.allowedMapIdsForUser(user.userId)
            withAdmin = allMaps.find((_, id, role) => id == userMapId && role == model.MapRole.Admin)
            _ <- ZIO.fail(NoRolesToUpdateMap(userMapId)).when(withAdmin.isEmpty)
            mapId = withAdmin.head._2
            _ <- MapQueries.updateMap(mapId, mapUpdate.map.name, toModelMapDisplayType(mapUpdate.map.displayType))
            updatedAt            <- ZIO.clockWith(_.instant)
            currentPolicyMembers <- AuthQueries.getMapPolicyMembers(mapId)
            _ <- updatePolicyMembers(
              mapId,
              user.userId,
              currentPolicyMembers,
              mapUpdate.policyMembers.map(toModelPolicyMember(mapId, _)).toList,
              updatedAt
            )
            mapModelOpt    <- MapQueries.getMap(mapId)
            _              <- ZIO.fail(MapNotFound(mapId)).when(mapModelOpt.isEmpty)
            _              <- ZIO.serviceWithZIO[MapPermissionTracker](_.reloadPermissions(mapId))
            memberPolicies <- AuthQueries.getMapPolicyMembers(mapId)
          yield MapInfoWithPermissions(toMapInfo(mapModelOpt.get), memberPolicies.map(toProtoPolicyMember).toArray)
        )
        .tapErrorCause(ZIO.logWarningCause("failed while updating map", _))
        .mapError(toUserError)
  )

def deleteMap = Endpoints.deleteMap
  .zServerSecurityLogic(validateSession)
  .serverLogic(user =>
    userMapId =>
      dbquery
        .transaction(
          for
            allMaps <- MapPolicy.allowedMapIdsForUser(user.userId)
            withAdmin = allMaps.find((_, id, role) => id == userMapId && role == model.MapRole.Admin)
            _ <- ZIO.fail(NoRolesToDeleteMap(userMapId)).when(withAdmin.isEmpty)
            mapId = withAdmin.head._2
            _ <- AuthQueries.deleteMapPolicyMembers(mapId)
            _ <- AuthQueries.deleteMapPolicy(mapId)
            _ <- MapQueries.deleteMap(mapId, user.userId)
          // TODO : notify the permission tracker that the map is gone!
          yield ()
        )
        .tapErrorCause(ZIO.logWarningCause("failed while deleting map", _))
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
            mapTup <- checkUserCanAccessMap(user, character.id, mapName)
              .mapError(Response.text(_).status(Status.Unauthorized))
            (mapId, mapRole) = mapTup
            sessionMsgs <- ZIO.serviceWithZIO[MapPermissionTracker](_.subscribeSession(mapId, character.id))
            resp        <- MapSession(mapId, character.id, user.userId, mapRole, sessionMsgs).toResponse
          yield resp
      }
  ).toHttpApp

def allMapEndpoints
    : List[ZServerEndpoint[EndpointEnv, sttp.capabilities.zio.ZioStreams & sttp.capabilities.WebSockets]] =
  List(
    createMap.widen[EndpointEnv],
    deleteMap.widen[EndpointEnv],
    getMap.widen[EndpointEnv],
    updateMap.widen[EndpointEnv]
  )

private def lookupCharacter(character: String) =
  AuthQueries
    .getCharacterByName(character)
    .tapErrorCause(ZIO.logWarningCause("failed while getting character by name", _))
    .orElseFail(zio.http.Response.text("Database error").status(zio.http.Status.InternalServerError))

private def checkUserCanAccessMap(
    user: LoggedInUser,
    characterId: CharacterId,
    mapName: String
): ZIO[javax.sql.DataSource, String, (MapId, model.MapRole)] =
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

private def checkNewMapCharacterMatches(userId: UserId, newMap: NewMap) =
  for
    // note: we stopped checking whether a character matches because there is no need to link a character to a map (except through policy)
    mapAlreadyExists <- MapQueries.getMapByCreatorUserAndName(userId, newMap.name).map(_.isDefined)
    _                <- ZIO.when(mapAlreadyExists)(ZIO.fail(ValidationError("Map name not unique")))
  yield ()

private def updatePolicyMembers(
    mapId: MapId,
    userId: UserId,
    prev: List[model.MapPolicyMember],
    next: List[model.MapPolicyMember],
    updatedAt: Instant
) =
  val prevMap = prev.map(mpm => (mpm.memberId, mpm.memberType) -> mpm).toMap
  val nextMap =
    next.map(mpm => (mpm.memberId, mpm.memberType) -> mpm.copy(updatedAt = updatedAt, updatedByUserId = userId)).toMap
  val toCreate = nextMap.keySet -- prevMap.keySet
  val toDelete = prevMap.keySet -- nextMap.keySet
  val toUpdate = prevMap.keySet & nextMap.keySet
  for
    _ <- AuthQueries.deleteMapPolicyMembersByMemberIds(mapId, toDelete.toList)
    _ <- AuthQueries.createMapPolicyMembers(
      toCreate.map(k => nextMap(k).copy(createdAt = updatedAt, createdByUserId = userId)).toList
    )
    _ <- AuthQueries.updateMapPolicyMembers(mapId, toUpdate.map(k => nextMap(k)).toList)
  yield ()

private def insertNewMap(userId: UserId, newMap: NewMap) =
  for
    map <- MapQueries
      .createMap(userId, newMap.name, toModelMapDisplayType(newMap.displayType))
      .flatMap(MapQueries.getMap)
      .map(_.head)
    _ <- AuthQueries.createMapPolicy(map.id, userId)
    _ <- AuthQueries.createMapPolicyMembers(
      newMap.policyMembers.map(pm => toModelPolicyMemberForCreate(map, userId, pm)).toList
    )
  yield map

def toMapInfo(map: model.MapModel): MapInfo = /* FIXME store settings */
  MapInfo(map.id, map.name, toMapDisplayType(map.displayType), MapSettings(12.hours), map.createdAt)

def toModelPolicyMemberForCreate(map: model.MapModel, userId: UserId, m: MapPolicyMember): model.MapPolicyMember =
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

def toModelPolicyMember(mapId: MapId, m: MapPolicyMember): model.MapPolicyMember =
  model.MapPolicyMember(
    mapId = mapId,
    memberId = m.memberId,
    memberType = toMemberType(m.memberType),
    isDeny = m.isDeny,
    role = toMapRole(m.role),
    createdByUserId = m.createdBy.getOrElse(UserId.Invalid),
    createdAt = m.createdAt.getOrElse(Instant.EPOCH),
    updatedByUserId = m.updatedBy.getOrElse(UserId.Invalid),
    updatedAt = m.updatedAt.getOrElse(Instant.EPOCH)
  )

def toProtoPolicyMember(value: model.MapPolicyMember): MapPolicyMember =
  MapPolicyMember(
    memberId = value.memberId,
    memberType = toProtoMemberType(value.memberType),
    isDeny = value.isDeny,
    role = toProtocolRole(value.role),
    createdBy = Some(value.createdByUserId),
    createdAt = Some(value.createdAt),
    updatedBy = Some(value.updatedByUserId),
    updatedAt = Some(value.updatedAt)
  )

def toMemberType(m: PolicyMemberType): model.PolicyMemberType = m match
  case PolicyMemberType.Corporation => model.PolicyMemberType.Corporation
  case PolicyMemberType.Alliance    => model.PolicyMemberType.Alliance
  case PolicyMemberType.Character   => model.PolicyMemberType.Character

def toProtoMemberType(m: model.PolicyMemberType) = m match
  case model.PolicyMemberType.Corporation => PolicyMemberType.Corporation
  case model.PolicyMemberType.Alliance    => PolicyMemberType.Alliance
  case model.PolicyMemberType.Character   => PolicyMemberType.Character

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
  case ValidationError(msg)      => msg
  case NoRolesToDeleteMap(mapId) => s"Insufficient roles to delete map with id ${mapId}"
  case ex                        => "Unknown error occurred"
