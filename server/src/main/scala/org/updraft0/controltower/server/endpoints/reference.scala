package org.updraft0.controltower.server.endpoints

import org.updraft0.controltower.protocol.*
import org.updraft0.controltower.server.db.ReferenceQueries
import sttp.model.StatusCode
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import sttp.tapir.ztapir.*
import zio.*

import javax.sql.DataSource

def getSolarSystem = Endpoints.getSolarSystem.zServerLogic { name =>
  ReferenceQueries.getSolarSystemByName(name).mapError(dbError).flatMap {
    case Nil       => ZIO.fail(StatusCode.NotFound -> "")
    case ss :: Nil => ZIO.succeed(ss)
    case _         => ZIO.fail(StatusCode.InternalServerError -> "BUG: non-unique solar system")
  }
}

def getVersion = Endpoints.getVersion.zServerLogic(_ => ZIO.succeed(0L /* TODO */ ))

def getAllReference = Endpoints.getAll.zServerLogic(_ =>
  (ReferenceQueries.getFactions <&> ReferenceQueries.getShipTypes <&> ReferenceQueries.getStarTypes <&> ReferenceQueries.getStationOperations <&> ReferenceQueries.getWormholeTypes)
    .mapBoth(
      dbError,
      { case (factions, shipTypes, starTypes, stationOperations, wormholeTypes) =>
        Reference(0L /* TODO */, factions, shipTypes, starTypes, stationOperations, wormholeTypes)
      }
    )
)

def getFactions  = Endpoints.getFactions.zServerLogic(_ => ReferenceQueries.getFactions.mapError(dbError))
def getShipTypes = Endpoints.getShipTypes.zServerLogic(_ => ReferenceQueries.getShipTypes.mapError(dbError))
def getStarTypes = Endpoints.getStarTypes.zServerLogic(_ => ReferenceQueries.getStarTypes.mapError(dbError))
def getStationOperations =
  Endpoints.getStationOperations.zServerLogic(_ => ReferenceQueries.getStationOperations.mapError(dbError))
def getWormholeTypes = Endpoints.getWormholeTypes.zServerLogic(_ => ReferenceQueries.getWormholeTypes.mapError(dbError))

def allReferenceEndpoints: List[ZServerEndpoint[DataSource, Any]] =
  List(
    getSolarSystem,
    getVersion.asInstanceOf[ZServerEndpoint[DataSource, Any]],
    getAllReference,
    getFactions,
    getShipTypes,
    getStarTypes,
    getStationOperations,
    getWormholeTypes
  )
