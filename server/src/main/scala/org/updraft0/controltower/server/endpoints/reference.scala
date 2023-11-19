package org.updraft0.controltower.server.endpoints

import org.updraft0.controltower.protocol.*
import org.updraft0.controltower.server.Server.EndpointEnv
import org.updraft0.controltower.server.db.ReferenceQueries
import sttp.model.StatusCode
import sttp.tapir.ztapir.*
import zio.*

def getSolarSystem = Endpoints.getSolarSystem.zServerLogic { name =>
  ReferenceQueries.getSolarSystemByName(name).mapError(dbError).flatMap {
    case Nil       => ZIO.fail(StatusCode.NotFound -> "")
    case ss :: Nil => ZIO.succeed(ss)
    case _         => ZIO.fail(StatusCode.InternalServerError -> "BUG: non-unique solar system")
  }
}

def getVersion = Endpoints.getVersion.zServerLogic(_ => ReferenceQueries.getVersion.mapBoth(dbError, _.id))

def getAllReference = Endpoints.getAllReference.zServerLogic(_ =>
  (ReferenceQueries.getVersion <&> ReferenceQueries.getFactions <&> ReferenceQueries.getShipTypes <&> ReferenceQueries.getStarTypes <&> ReferenceQueries.getStationOperations <&> ReferenceQueries.getWormholeTypes)
    .mapBoth(
      dbError,
      (version, factions, shipTypes, starTypes, stationOperations, wormholeTypes) =>
        Reference(version.id, factions, shipTypes, starTypes, stationOperations, wormholeTypes)
    )
)

def getAllSolarSystems = Endpoints.getAllSolarSystems.zServerLogic(_ =>
  (ReferenceQueries.getVersion <&> ReferenceQueries.getSolarSystemsByName(None))
    .mapBoth(
      dbError,
      (version, solarSystems) => ReferenceSolarSystems(version.id, solarSystems)
    )
)

def getFactions  = Endpoints.getFactions.zServerLogic(_ => ReferenceQueries.getFactions.mapError(dbError))
def getShipTypes = Endpoints.getShipTypes.zServerLogic(_ => ReferenceQueries.getShipTypes.mapError(dbError))
def getStarTypes = Endpoints.getStarTypes.zServerLogic(_ => ReferenceQueries.getStarTypes.mapError(dbError))
def getStationOperations =
  Endpoints.getStationOperations.zServerLogic(_ => ReferenceQueries.getStationOperations.mapError(dbError))
def getWormholeTypes = Endpoints.getWormholeTypes.zServerLogic(_ => ReferenceQueries.getWormholeTypes.mapError(dbError))

def allReferenceEndpoints: List[ZServerEndpoint[EndpointEnv, Any]] =
  List(
    getSolarSystem.widen[EndpointEnv],
    getVersion.widen[EndpointEnv],
    getAllReference.widen[EndpointEnv],
    getAllSolarSystems.widen[EndpointEnv],
    getFactions.widen[EndpointEnv],
    getShipTypes.widen[EndpointEnv],
    getStarTypes.widen[EndpointEnv],
    getStationOperations.widen[EndpointEnv],
    getWormholeTypes.widen[EndpointEnv]
  )
