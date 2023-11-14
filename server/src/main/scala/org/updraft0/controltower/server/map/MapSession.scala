package org.updraft0.controltower.server.map

import org.updraft0.controltower.db.model
import org.updraft0.controltower.protocol
import org.updraft0.controltower.protocol.displayType
import org.updraft0.controltower.server.db.{MapQueries, MapSystemWithAll}
import zio.stream.{Stream, ZSink, ZStream}
import zio.{Exit, RIO, Scope, ZIO, ZLayer}

/** Loosely, a map "session" is an open WebSocket for a single (character, map)
  */
object MapSession:
  type Env = MapReactive.Service

  def apply(
      mapId: MapId,
      characterId: Long,
      userId: Long
  ): RIO[Env, Stream[Throwable, protocol.MapRequest] => Stream[Throwable, protocol.MapMessage]] =
    // make a manual scope that will be released when the stream finishes (rather than when the outer effect completes)
    Scope.make.flatMap { scope =>
      for
        streamId <- ZIO.randomWith(_.nextUUID)
        mapE     <- ZIO.service[MapReactive.Service]
        mapQ     <- mapE.enqueue(mapId)
        resQ     <- mapE.subscribe(mapId).provideSome(ZLayer.succeed(scope))
        _        <- ZIO.logInfo(s"started subscription stream for $mapId/$characterId/$userId")
      yield { (reqStream: Stream[Throwable, protocol.MapRequest]) =>

        val requestInStream = reqStream.mapZIO {
          case protocol.MapRequest.GetSnapshot =>
            mapQ.offer(Identified(Some(characterId), MapRequest.MapSnapshot))
          case add: protocol.MapRequest.AddSystem =>
            mapQ.offer(Identified(Some(characterId), toAddSystem(add)))
          case upd: protocol.MapRequest.UpdateSystemDisplay =>
            mapQ.offer(Identified(Some(characterId), toUpdateSystemDisplay(upd)))
          case protocol.MapRequest.RemoveSystem(systemId) =>
            mapQ.offer(Identified(Some(characterId), MapRequest.RemoveSystem(systemId)))
        }.drain

        val mapEntitySubStream = ZStream.fromQueue(resQ).map(filterToProto(characterId))

        ZStream
          .mergeAllUnbounded(outputBuffer = 1)(requestInStream, mapEntitySubStream)
          .filter(_.isDefined)
          .map(_.get)
          .onError(cause => scope.close(Exit.failCause(cause))) ++ ZStream.fromZIO(scope.close(Exit.succeed(()))).drain
      }
    }

private def filterToProto(characterId: Long)(msg: Identified[MapResponse]): Option[protocol.MapMessage] =
  msg match
    case Identified(Some(`characterId`), msg) => toProto(msg)
    case Identified(Some(_), _)               => None
    case Identified(None, msg)                => toProto(msg)

private def toProto(msg: MapResponse): Option[protocol.MapMessage] = msg match
  case MapResponse.MapSnapshot(all) =>
    Some(protocol.MapMessage.MapSnapshot(toProtoSystems(all), toProtoConnections(all)))
  case MapResponse.SystemSnapshot(systemId, sys) =>
    Some(
      protocol.MapMessage
        .SystemSnapshot(systemId, toProtoSystemSnapshot(sys), toProtoConnections(Map(systemId -> sys)))
    )
  case MapResponse.SystemDisplayUpdate(systemId, displayData) =>
    Some(protocol.MapMessage.SystemDisplayUpdate(systemId, toProtoDisplay(displayData)))
  case MapResponse.SystemRemoved(systemId) =>
    Some(protocol.MapMessage.SystemRemoved(systemId))

private def toAddSystem(msg: protocol.MapRequest.AddSystem) =
  MapRequest.AddSystem(
    systemId = msg.systemId,
    name = msg.name,
    isPinned = msg.isPinned,
    displayData = toDisplayData(msg.displayData),
    stance = toIntelStance(msg.stance)
  )

private def toUpdateSystemDisplay(msg: protocol.MapRequest.UpdateSystemDisplay) =
  MapRequest.UpdateSystemDisplay(
    systemId = msg.systemId,
    displayData = toDisplayData(msg.displayData)
  )

private def toDisplayType(dt: protocol.MapDisplayType) = dt match
  case protocol.MapDisplayType.Manual => model.MapDisplayType.Manual

private def toDisplayData(sd: protocol.SystemDisplayData) = sd match
  case protocol.SystemDisplayData.Manual(x, y) => model.SystemDisplayData.Manual(x, y)

private def toIntelStance(is: protocol.IntelStance) = is match
  case protocol.IntelStance.Unknown  => model.IntelStance.Unknown
  case protocol.IntelStance.Hostile  => model.IntelStance.Hostile
  case protocol.IntelStance.Friendly => model.IntelStance.Friendly

private def toProtoSystems(all: Map[SystemId, MapSystemWithAll]): Vector[protocol.MapSystemSnapshot] =
  all.values.map(toProtoSystemSnapshot).toVector

private def toProtoSystemSnapshot(value: MapSystemWithAll): protocol.MapSystemSnapshot =
  protocol.MapSystemSnapshot(
    system = toProtoSystem(value.sys, value.display),
    signatures = value.signatures.map(toProtoSignature),
    notes = value.notes.map(toProtoNote),
    structures = value.structures.map(toProtoStructure)
  )

private def toProtoSystem(value: model.MapSystem, displayData: Option[model.SystemDisplayData]): protocol.MapSystem =
  protocol.MapSystem(
    systemId = value.systemId,
    name = value.name,
    isPinned = value.isPinned,
    chainNamingStrategy = Some(value.chainNamingStrategy.ordinal()),
    description = value.description,
    stance = toProtoStance(value.stance),
    display = displayData.map(toProtoDisplay),
    updatedAt = value.updatedAt,
    updatedByCharacterId = value.updatedByCharacterId
  )

private def toProtoSignature(value: model.MapSystemSignature): protocol.MapSystemSignature =
  value.signatureGroup match
    case model.SignatureGroup.Unknown =>
      protocol.MapSystemSignature.Unknown(
        id = value.signatureId,
        createdAt = value.createdAt,
        createdByCharacterId = value.createdByCharacterId,
        updatedAt = value.updatedAt,
        updatedByCharacterId = value.updatedByCharacterId
      )
    case model.SignatureGroup.Wormhole =>
      protocol.MapSystemSignature.Wormhole(
        id = value.signatureId,
        isEol = value.wormholeIsEol,
        eolAt = value.wormholeEolAt,
        connectionType = toProtoConnectionType(value),
        massStatus =
          value.wormholeMassStatus.map(toProtoWormholeMassStatus).getOrElse(protocol.WormholeMassStatus.Unknown),
        massSize = value.wormholeMassSize.map(toProtoWormholeMassSize).getOrElse(protocol.WormholeMassSize.Unknown),
        connectionId = value.wormholeConnectionId,
        createdAt = value.createdAt,
        createdByCharacterId = value.createdByCharacterId,
        updatedAt = value.updatedAt,
        updatedByCharacterId = value.updatedByCharacterId
      )
    case group =>
      protocol.MapSystemSignature.Site(
        id = value.signatureId,
        group = toProtoGroup(group),
        name = value.signatureTypeName,
        createdAt = value.createdAt,
        createdByCharacterId = value.createdByCharacterId,
        updatedAt = value.updatedAt,
        updatedByCharacterId = value.updatedByCharacterId
      )

private def toProtoConnectionType(value: model.MapSystemSignature): protocol.WormholeConnectionType =
  (value.wormholeK162Type, value.wormholeTypeId) match
    case (Some(k162Type), _) =>
      protocol.WormholeConnectionType.K162(k162Type match
        case model.WormholeK162Type.Unknown   => protocol.WormholeK162Type.Unknown
        case model.WormholeK162Type.Dangerous => protocol.WormholeK162Type.Dangerous
        case model.WormholeK162Type.Deadly    => protocol.WormholeK162Type.Deadly
      )
    case (None, Some(typeId)) => protocol.WormholeConnectionType.Known(typeId)
    case _                    => protocol.WormholeConnectionType.Unknown

private def toProtoStance(value: model.IntelStance): protocol.IntelStance =
  value match
    case model.IntelStance.Unknown  => protocol.IntelStance.Unknown
    case model.IntelStance.Hostile  => protocol.IntelStance.Hostile
    case model.IntelStance.Friendly => protocol.IntelStance.Friendly

private def toProtoDisplay(value: model.SystemDisplayData): protocol.SystemDisplayData =
  value match
    case model.SystemDisplayData.Manual(x, y) => protocol.SystemDisplayData.Manual(x, y)

private def toProtoWormholeMassStatus(value: model.WormholeMassStatus): protocol.WormholeMassStatus =
  value match
    case model.WormholeMassStatus.Unknown  => protocol.WormholeMassStatus.Unknown
    case model.WormholeMassStatus.Fresh    => protocol.WormholeMassStatus.Fresh
    case model.WormholeMassStatus.Reduced  => protocol.WormholeMassStatus.Reduced
    case model.WormholeMassStatus.Critical => protocol.WormholeMassStatus.Critical

private def toProtoWormholeMassSize(value: model.WormholeMassSize): protocol.WormholeMassSize =
  value match
    case model.WormholeMassSize.Unknown => protocol.WormholeMassSize.Unknown
    case model.WormholeMassSize.S       => protocol.WormholeMassSize.S
    case model.WormholeMassSize.M       => protocol.WormholeMassSize.M
    case model.WormholeMassSize.L       => protocol.WormholeMassSize.L
    case model.WormholeMassSize.XL      => protocol.WormholeMassSize.XL

private def toProtoGroup(value: model.SignatureGroup): protocol.SignatureGroup =
  value match
    case model.SignatureGroup.Unknown  => protocol.SignatureGroup.Unknown
    case model.SignatureGroup.Combat   => protocol.SignatureGroup.Combat
    case model.SignatureGroup.Data     => protocol.SignatureGroup.Data
    case model.SignatureGroup.Gas      => protocol.SignatureGroup.Gas
    case model.SignatureGroup.Ghost    => protocol.SignatureGroup.Ghost
    case model.SignatureGroup.Ore      => protocol.SignatureGroup.Ore
    case model.SignatureGroup.Relic    => protocol.SignatureGroup.Relic
    case model.SignatureGroup.Wormhole => protocol.SignatureGroup.Wormhole

private def toProtoNote(value: model.MapSystemNote): protocol.MapSystemNote =
  protocol.MapSystemNote(
    id = value.id,
    note = value.note,
    createdAt = value.createdAt,
    createdByCharacterId = value.createdByCharacterId,
    updatedAt = value.updatedAt,
    updatedByCharacterId = value.updatedByCharacterId
  )

private def toProtoStructure(value: model.MapSystemStructure): protocol.MapSystemStructure =
  protocol.MapSystemStructure(
    name = value.name,
    structureType = value.structureType,
    owner = None /* TODO */,
    location = value.location,
    createdAt = value.createdAt,
    createdByCharacterId = value.createdByCharacterId,
    updatedAt = value.updatedAt,
    updatedByCharacterId = value.updatedByCharacterId
  )

private def toProtoConnections(all: Map[SystemId, MapSystemWithAll]): Map[Long, protocol.MapWormholeConnection] =
  all.values
    .flatMap(_.connections)
    .map(c => c.id -> c)
    .toMap
    .view
    .mapValues(toProtoConnection)
    .toMap

private def toProtoConnection(value: model.MapWormholeConnection): protocol.MapWormholeConnection =
  protocol.MapWormholeConnection(
    id = value.id,
    fromSystemId = value.fromSystemId,
    toSystemId = value.toSystemId,
    createdAt = value.createdAt,
    createdByCharacterId = value.createdByCharacterId,
    updatedAt = value.updatedAt,
    updatedByCharacterId = value.updatedByCharacterId
  )
