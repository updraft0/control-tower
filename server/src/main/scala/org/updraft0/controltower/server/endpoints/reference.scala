package org.updraft0.controltower.server.endpoints

import org.updraft0.controltower.build.BuildInfo
import org.updraft0.controltower.constant.SystemId
import org.updraft0.controltower.protocol.*
import org.updraft0.controltower.server.Server.EndpointEnv
import org.updraft0.controltower.server.db.ReferenceQueries
import org.updraft0.controltower.db.query.sde.getLatestVersion
import sttp.model.StatusCode
import sttp.tapir.ztapir.*
import zio.*

private val NoSdeVersion =
  ZIO.logError("No SDE version loaded").as(StatusCode.InternalServerError -> "BUG: No SDE version found").flip

def getSolarSystem = Endpoints.getSolarSystem.zServerLogic { name =>
  ReferenceQueries.getSolarSystemByName(name).flatMapError(logDbError).flatMap {
    case Nil       => ZIO.fail(StatusCode.NotFound -> "")
    case ss :: Nil => ZIO.succeed(ss)
    case _         => ZIO.fail(StatusCode.InternalServerError -> "BUG: non-unique solar system")
  }
}

def getSolarSystemById = Endpoints.getSolarSystemById.zServerLogic { id =>
  ReferenceQueries.getSolarSystemById(SystemId(id)).flatMapError(logDbError).flatMap {
    case Nil       => ZIO.fail(StatusCode.NotFound -> "")
    case ss :: Nil => ZIO.succeed(ss)
    case _         => ZIO.fail(StatusCode.InternalServerError -> "BUG: non-unique solar system")
  }
}

def getVersion = Endpoints.getVersion.zServerLogic(_ =>
  getLatestVersion
    .flatMapError(logDbError)
    .flatMap(_.map(ZIO.succeed).getOrElse(NoSdeVersion))
    .map(v => ReferenceVersion(v.id, BuildInfo.gitHash)) // TODO: switch to semantic versions once past 0.x
)

def getAllReference = Endpoints.getAllReference.zServerLogic(_ =>
  (getLatestVersion <&> ReferenceQueries.getFactions <&> ReferenceQueries.getSignaturesInGroup <&> ReferenceQueries.getShipTypes <&> ReferenceQueries.getStarTypes <&> ReferenceQueries.getStationOperations <&> ReferenceQueries.getWormholeTypes)
    .flatMapError(logDbError)
    .flatMap((versionOpt, factions, signaturesInGroup, shipTypes, starTypes, stationOperations, wormholeTypes) =>
      versionOpt
        .map(version =>
          ZIO.succeed(
            Reference(
              version = version.id,
              factions = factions.toArray,
              signaturesInGroup = signaturesInGroup.toArray,
              shipTypes = shipTypes.toArray,
              starTypes = starTypes.toArray,
              stationOperations = stationOperations.toArray,
              wormholeTypes = wormholeTypes.toArray
            )
          )
        )
        .getOrElse(NoSdeVersion)
    )
)

def getAllSolarSystems = Endpoints.getAllSolarSystems.zServerLogic(_ =>
  (getLatestVersion <&> ReferenceQueries.getSolarSystemsByName(None, None))
    .flatMapError(logDbError)
    .flatMap((versionOpt, solarSystems) =>
      versionOpt
        .map(v => ZIO.succeed(ReferenceSolarSystems(v.id, solarSystems.toArray)))
        .getOrElse(NoSdeVersion)
    )
)

def getFactions =
  Endpoints.getFactions.zServerLogic(_ => ReferenceQueries.getFactions.flatMapError(logDbError).map(_.toArray))
def getShipTypes =
  Endpoints.getShipTypes.zServerLogic(_ => ReferenceQueries.getShipTypes.flatMapError(logDbError).map(_.toArray))
def getStarTypes =
  Endpoints.getStarTypes.zServerLogic(_ => ReferenceQueries.getStarTypes.flatMapError(logDbError).map(_.toArray))
def getStationOperations =
  Endpoints.getStationOperations.zServerLogic(_ =>
    ReferenceQueries.getStationOperations.flatMapError(logDbError).map(_.toArray)
  )
def getWormholeTypes =
  Endpoints.getWormholeTypes.zServerLogic(_ =>
    ReferenceQueries.getWormholeTypes.flatMapError(logDbError).map(_.toArray)
  )
def getSignaturesInGroup =
  Endpoints.getSignaturesInGroup.zServerLogic(_ =>
    ReferenceQueries.getSignaturesInGroup.flatMapError(logDbError).map(_.toArray)
  )

def allReferenceEndpoints: List[ZServerEndpoint[EndpointEnv, Any]] =
  List(
    getSolarSystem.widen[EndpointEnv],
    getSolarSystemById.widen[EndpointEnv],
    getVersion.widen[EndpointEnv],
    getAllReference.widen[EndpointEnv],
    getAllSolarSystems.widen[EndpointEnv],
    getFactions.widen[EndpointEnv],
    getShipTypes.widen[EndpointEnv],
    getStarTypes.widen[EndpointEnv],
    getStationOperations.widen[EndpointEnv],
    getWormholeTypes.widen[EndpointEnv],
    getSignaturesInGroup.widen[EndpointEnv]
  )
