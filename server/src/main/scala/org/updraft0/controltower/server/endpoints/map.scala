package org.updraft0.controltower.server.endpoints

import org.updraft0.controltower.protocol.{SessionCookie as _, *}
import org.updraft0.controltower.server.Config
import org.updraft0.controltower.server.Server.EndpointEnv
import org.updraft0.controltower.server.auth.*
import org.updraft0.esi.client.EsiClient
import sttp.client3.UriContext
import sttp.model.{Uri, StatusCode}
import sttp.model.headers.{Cookie, CookieValueWithMeta}
import sttp.tapir.ztapir.*
import zio.{Config as _, *}
import java.util.UUID
import org.updraft0.controltower.server.db.AuthQueries
import org.updraft0.controltower.server.db.MapQueries
import org.updraft0.controltower.db.model

def createMap = Endpoints.createMap
  .zServerSecurityLogic(validateSession)
  .serverLogic(user =>
    newMap => (checkNewMapCharacterMatches(user.userId, newMap) *> insertNewMap(user.userId, newMap)).map(toMapInfo)
  )

private def checkNewMapCharacterMatches(userId: Long, newMap: NewMap) =
  for
    charAndExists <- (AuthQueries.getUserCharacterByIdAndCharId(userId, newMap.createdByCharacterId)
      <*> MapQueries.getMapByUserAndName(userId, newMap.name).map(_.isDefined))
      .tapErrorCause(ZIO.logWarningCause("failed query", _))
      .orElseFail("Failure while trying to find user/map info")
    _ <- ZIO.when(charAndExists._2)(ZIO.fail("Map name not unique"))
    _ <- ZIO.when(charAndExists._1.isEmpty)(ZIO.fail("No user/character combination found"))
  yield ()

private def insertNewMap(userId: Long, newMap: NewMap) =
  (for
    map <- MapQueries.createMap(userId, newMap.name).flatMap(MapQueries.getMap).map(_.head)
    _   <- AuthQueries.createMapPolicy(map.id, userId)
    _ <- AuthQueries.createMapPolicyMembers(newMap.policyMembers.map(pm => toModelPolicyMember(map, userId, pm)).toList)
  yield map)
    .tapErrorCause(ZIO.logWarningCause("failed query", _))
    .orElseFail("Failure while trying to create map")

def allMapEndpoints: List[ZServerEndpoint[EndpointEnv, Any]] =
  List(
    createMap.widen[EndpointEnv]
  )

def toMapInfo(map: model.MapModel): MapInfo = MapInfo(map.id, map.name, map.creatorUserId, map.createdAt)

def toModelPolicyMember(map: model.MapModel, userId: Long, m: MapPolicyMember): model.MapPolicyMember =
  model.MapPolicyMember(
    mapId = map.id,
    memberId = m.memberId,
    memberType = toMemberType(m.memberType),
    isDeny = m.isDeny,
    role = toMapRole(m.role),
    createdBy = userId,
    createdAt = map.createdAt,
    updatedBy = userId,
    updatedAt = map.createdAt
  )

def toMemberType(m: PolicyMemberType): model.PolicyMemberType = m match
  case PolicyMemberType.Corporation => model.PolicyMemberType.Corporation
  case PolicyMemberType.Alliance    => model.PolicyMemberType.Alliance
  case PolicyMemberType.Character   => model.PolicyMemberType.Character

def toMapRole(m: MapRole): model.MapRole = m match
  case MapRole.Viewer => model.MapRole.Viewer
  case MapRole.Editor => model.MapRole.Editor
  case MapRole.Admin  => model.MapRole.Admin
