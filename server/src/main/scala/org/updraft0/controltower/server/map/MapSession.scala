package org.updraft0.controltower.server.map

import org.updraft0.controltower.db.model
import org.updraft0.controltower.protocol
import org.updraft0.controltower.protocol.jsoncodec.given
import org.updraft0.controltower.server.Log
import org.updraft0.controltower.server.db.MapSystemWithAll
import zio.*
import zio.http.{ChannelEvent, Handler, WebSocketChannelEvent, WebSocketFrame}
import zio.json.*

/** Loosely, a map "session" is an open WebSocket for a single (character, map)
  *
  * @note
  *   Using zio-http directly here because of some shutdown issues encountered with the sttp/zio bridge. Basically, with
  *   the internals of `ZioHttpInterpreter` the incoming WS messages are put on a queue, and there's no handler for
  *   socket closure (here we do `Channel.awaitShutdown` to close the manually-created scope and release the resources)
  */
object MapSession:
  type Env = MapReactive.Service

  private case class Context(
      mapId: MapId,
      sessionId: MapSessionId,
      userId: Long,
      mapQ: Enqueue[Identified[MapRequest]],
      resQ: Dequeue[Identified[MapResponse]]
  )

  def apply(
      mapId: MapId,
      characterId: Long,
      userId: Long
  ) = Handler.webSocket: chan =>
    inContext(mapId, characterId, userId)(
      for
        sid  <- ZIO.service[MapSessionId]
        _    <- ZIO.logDebug("started map session")
        mapE <- ZIO.service[MapReactive.Service]
        mapQ <- mapE.enqueue(mapId)
        resQ <- mapE.subscribe(mapId)
        ctx = Context(mapId, sid, userId, mapQ, resQ)
        // close the scope (and the subscription) if the websocket is closed
        _ <- ZIO
          .serviceWithZIO[Scope.Closeable](scope =>
            chan.awaitShutdown
              .zipRight(ZIO.logDebug("finished map session due to socket closure"))
              .zipRight(scope.close(Exit.succeed(())))
          )
          .forkDaemon
        // run the receive from websocket -> queue of inbox and receive from outbox --> websocket in parallel, in scope
        recv <- ZIO
          .whileLoop(true)(chan.receive.flatMap(decodeMessage(_)).flatMap {
            case Some(msg) => processMessage(ctx, msg).unit
            case None      => ZIO.unit
          })(identity)
          .forkScoped
        send <- ZIO
          .whileLoop(true)(resQ.take.map(filterToProto(sid)(_)).flatMap {
            case Some(msg) => chan.send(ChannelEvent.Read(WebSocketFrame.Text(msg.toJson)))
            case None      => ZIO.unit
          })(identity)
          .forkScoped
        _ <- recv.join
        _ <- send.join
      yield ()
    )

  private def inContext[R: Tag](mapId: MapId, characterId: Long, userId: Long)(
      f: ZIO[R & Scope.Closeable & MapSessionId, Throwable, Any]
  ): ZIO[R, Throwable, Any] =
    for
      sessionId <- ZIO.randomWith(_.nextUUID).map(MapSessionId(characterId, _))
      scope     <- Scope.make
      res <- f.provideSome[R](ZLayer.succeed(scope), ZLayer.succeed(sessionId)) @@ Log.SessionId(
        sessionId.sessionId
      ) @@ Log.MapId(mapId) @@ Log.UserId(userId) @@ Log.CharacterId(sessionId.characterId)
    yield res

  private inline def decodeMessage(ev: WebSocketChannelEvent): Task[Option[protocol.MapRequest]] = ev match
    case ChannelEvent.Read(WebSocketFrame.Text(msgText)) =>
      msgText.fromJson[protocol.MapRequest] match
        case Left(error) => ZIO.logError(s"Unable to decode json: $error").as(None)
        case Right(msg)  => ZIO.succeed(Some(msg))
    case ChannelEvent.ExceptionCaught(ex) => ZIO.logErrorCause("Received exception, logging", Cause.fail(ex)).as(None)
    case ChannelEvent.Unregistered        => ZIO.none
    case ChannelEvent.UserEventTriggered(ChannelEvent.UserEvent.HandshakeComplete) => ZIO.none
    case other => ZIO.logError(s"BUG - don't know what to do with message $other").as(None)

  private inline def processMessage(ctx: Context, msg: protocol.MapRequest) = msg match
    case protocol.MapRequest.GetSnapshot =>
      ctx.mapQ.offer(Identified(Some(ctx.sessionId), MapRequest.MapSnapshot))
    case add: protocol.MapRequest.AddSystem =>
      ctx.mapQ.offer(Identified(Some(ctx.sessionId), toAddSystem(add)))
    case upd: protocol.MapRequest.UpdateSystemDisplay =>
      ctx.mapQ.offer(Identified(Some(ctx.sessionId), toUpdateSystemDisplay(upd)))
    case protocol.MapRequest.RemoveSystem(systemId) =>
      ctx.mapQ.offer(Identified(Some(ctx.sessionId), MapRequest.RemoveSystem(systemId)))

private def filterToProto(sessionId: MapSessionId)(msg: Identified[MapResponse]): Option[protocol.MapMessage] =
  if (msg.sessionId.forall(_ == sessionId)) toProto(msg.value) else None

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
    display = value.display.map(toProtoDisplay),
    structures = value.structures.map(toProtoStructure),
    notes = value.notes.map(toProtoNote),
    signatures = value.signatures.map(toProtoSignature),
    connections = value.connections.map(toProtoConnection)
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
