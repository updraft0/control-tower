package org.updraft0.controltower.server.map

import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.{model, query}
import org.updraft0.controltower.protocol
import org.updraft0.controltower.protocol.jsoncodec.given
import org.updraft0.controltower.server.Log
import org.updraft0.controltower.server.db.{
  MapQueries,
  MapSystemWithAll,
  MapWormholeConnectionRank,
  MapWormholeConnectionWithSigs
}
import org.updraft0.controltower.server.endpoints.{toMapInfo, toProtocolRole}
import org.updraft0.controltower.server.tracking.{
  CharacterLocationState,
  LocationTracker,
  LocationTrackingRequest,
  ServerStatusTracker
}
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import org.updraft0.controltower.protocol.UserPreferences

import scala.util.Try
import zio.*
import zio.http.ChannelEvent.UserEvent
import zio.http.{ChannelEvent, Handler, WebSocketChannelEvent, WebSocketFrame}
import zio.logging.LogAnnotation

import java.util.UUID

given CanEqual[WebSocketChannelEvent, WebSocketChannelEvent]   = CanEqual.derived
given CanEqual[ChannelEvent.UserEvent, ChannelEvent.UserEvent] = CanEqual.derived

enum MapSessionMessage:
  // TODO: add Map deleted message
  case MapCharacters(mapId: MapId, all: Map[CharacterId, model.MapRole])
  // TODO: currently largely unused
  case RoleChanged(characterId: CharacterId, role: Option[model.MapRole])

/** Loosely, a map "session" is an open WebSocket for a single (character, map)
  *
  * @note
  *   Using zio-http directly here because of some shutdown issues encountered with the sttp/zio bridge. Basically, with
  *   the internals of `ZioHttpInterpreter` the incoming WS messages are put on a queue, and there's no handler for
  *   socket closure (here we do `Channel.awaitShutdown` to close the manually-created scope and release the resources)
  */
object MapSession:
  type Env = MapReactive.Service & javax.sql.DataSource & LocationTracker & ServerStatusTracker

  private val jsonContent  = LogAnnotation[String]("json", (_, b) => b, identity)
  private val errorMessage = LogAnnotation[String]("error", (_, b) => b, identity)

  /** queue size for messages that are generated internally
    */
  private val OurQueueSize = 16

  /** Proxies etc. close websocket connections if there are no messages
    */
  private val PingInterval = 1.minute

  private val ServerStatusInterval = 1.minute

  // TODO: refresh user preferences when they are altered without creating a new session

  /** Context for a map session
    */
  private case class Context(
      mapId: MapId,
      character: model.AuthCharacter,
      prefs: UserPreferences,
      sessionId: MapSessionId,
      userId: UserId,
      mapRole: Ref[model.MapRole],
      mapQ: Enqueue[Identified[MapRequest]],
      resQ: Dequeue[Identified[MapResponse]],
      ourQ: Queue[protocol.MapMessage],
      metaQ: Dequeue[MapSessionMessage]
  )

  def apply(
      mapId: MapId,
      character: model.AuthCharacter,
      userId: UserId,
      initialRole: model.MapRole,
      sessionMessages: Dequeue[MapSessionMessage],
      prefs: UserPreferences
  ) = Handler.webSocket: chan =>
    inContext(mapId, character.id, userId)(
      for
        sid     <- ZIO.service[MapSessionId]
        _       <- ZIO.logDebug("started map session")
        mapE    <- ZIO.service[MapReactive.Service]
        mapQ    <- mapE.enqueue(mapId)
        resQ    <- mapE.subscribe(mapId)
        ourQ    <- Queue.bounded[protocol.MapMessage](OurQueueSize)
        mapRole <- Ref.make(initialRole)
        // add character id to location tracking (strictly not entirely necessary because currently all map characters
        //    should get tracked automatically)
        _ <- ZIO
          .serviceWith[LocationTracker](_.inbound)
          .flatMap(_.offer(LocationTrackingRequest.AddCharacters(Chunk(character.id))))
        ctx = Context(mapId, character, prefs, sid, userId, mapRole, mapQ, resQ, ourQ, sessionMessages)
        // close the scope (and the subscription) if the websocket is closed
        close <- ZIO.serviceWith[Scope.Closeable](scope => scope.close(Exit.succeed(())))
        _ <- chan.awaitShutdown
          .zipRight(ZIO.logDebug("finished map session due to socket closure"))
          .zipRight(close)
          .forkDaemon
        // run the receive from websocket -> queue of inbox and receive from outbox --> websocket in parallel, in scope
        recv <- (chan.receive.flatMap(decodeMessage(_)) <*> mapRole.get)
          .flatMap {
            case (Right(msg), mapRole) if isAllowed(msg, mapRole) => processMessage(ctx, msg).unit
            case (Right(msg), mapRole) => ourQ.offer(protocol.MapMessage.Error("Permission denied")).ignoreLogged
            case (Left(error), _)      => ourQ.offer(protocol.MapMessage.Error(error)).ignoreLogged
          }
          .forever
          .forkScoped
        send <- resQ.take
          .map(filterToProto(sid)(_))
          .flatMap {
            case Some(msg) => chan.send(ChannelEvent.Read(WebSocketFrame.Text(writeToString(msg))))
            case None      => ZIO.unit
          }
          .forever
          .forkScoped
        sendOurs <- ourQ.take
          .flatMap(msg => chan.send(ChannelEvent.Read(WebSocketFrame.Text(writeToString(msg)))))
          .forever
          .forkScoped
        // process any session messages
        _ <- sessionMessages.take.flatMap(handleMapSessionMessage(character.id, mapRole, close, _)).forever.forkScoped
        // ping out every ping interval to keep connection open
        _ <- chan.send(ChannelEvent.Read(WebSocketFrame.Ping)).schedule(Schedule.fixed(PingInterval)).ignore.forkDaemon
        // listen for server status
        _ <- sendServerStatus(ourQ)
          .repeat(Schedule.fixed(ServerStatusInterval))
          .ignore
          .forkDaemon
        // join on the remaining loops
        _ <- recv.join
        _ <- send.join
        _ <- sendOurs.join
      yield ()
    )

  private def handleMapSessionMessage(
      characterId: CharacterId,
      mapRole: Ref[model.MapRole],
      close: UIO[Unit],
      msg: MapSessionMessage
  ) =
    msg match
      case MapSessionMessage.RoleChanged(charId, Some(newRole)) =>
        mapRole.set(newRole).when(charId == characterId)
      case MapSessionMessage.RoleChanged(charId, None) =>
        // this should close the map
        ZIO.logInfo(s"character no longer has any roles") *> close
      case _ => ZIO.unit // no-op

  private def inContext[R](mapId: MapId, characterId: CharacterId, userId: UserId)(
      f: ZIO[R & Scope.Closeable & MapSessionId, Throwable, Any]
  ): ZIO[R, Throwable, Any] =
    for
      sessionId <- ZIO.attempt(UUID.randomUUID()).map(MapSessionId(characterId, _))
      scope     <- Scope.make
      res <- f
        .tapError(ex => ZIO.logErrorCause("Map session failed unexpectedly", Cause.fail(ex)))
        .provideSome[R](ZLayer.succeed(scope), ZLayer.succeed(sessionId)) @@ Log.SessionId(
        sessionId.sessionId
      ) @@ Log.MapId(mapId) @@ Log.UserId(userId) @@ Log.CharacterId(sessionId.characterId)
    yield res

  private inline def decodeMessage(ev: WebSocketChannelEvent): Task[Either[String, protocol.MapRequest]] = ev match
    case ChannelEvent.UserEventTriggered(UserEvent.HandshakeComplete) =>
      ZIO.right(protocol.MapRequest.GetMapInfo)
    case ChannelEvent.Read(WebSocketFrame.Text(msgText)) =>
      Try(readFromString[protocol.MapRequest](msgText)).toEither match
        case Left(ex) =>
          ZIO.logError(s"Unable to decode json content").as(Left("Unable to decode json content")) @@
            jsonContent(msgText) @@ errorMessage(ex.getMessage)
        case Right(msg) => ZIO.right(msg)
    case ChannelEvent.ExceptionCaught(ex) =>
      ZIO.logErrorCause("Received exception, logging", Cause.fail(ex)).as(Left("Unknown error"))
    case ChannelEvent.Unregistered => ZIO.left("Channel closed")
    case other => ZIO.logError(s"BUG - don't know what to do with message $other").as(Left("Bug: Unexpected message"))

  private inline def processMessage(ctx: Context, msg: protocol.MapRequest) = msg match
    case protocol.MapRequest.GetSnapshot =>
      ctx.mapQ.offer(Identified(Some(ctx.sessionId), MapRequest.MapSnapshot))
    case protocol.MapRequest.GetMapInfo =>
      // TODO: not sure if it's right to propagate this or not
      (query.transaction(MapQueries.getMap(ctx.mapId)) <&> ctx.mapRole.get).flatMap {
        case (Some(map: model.MapModel), role) =>
          ctx.ourQ.offer(
            protocol.MapMessage
              .MapMeta(
                toProtoCharacter(ctx.character, authTokenFresh = true /* lie through our teeth */ ),
                toMapInfo(map),
                toProtocolRole(role),
                ctx.prefs
              )
          )
        case _ => ZIO.logError("BUG: map not set")
      }
    // connection
    case addConn: protocol.MapRequest.AddSystemConnection =>
      ctx.mapQ.offer(Identified(Some(ctx.sessionId), toAddSystemConnection(addConn)))
    case removeConn: protocol.MapRequest.RemoveSystemConnection =>
      ctx.mapQ.offer(Identified(Some(ctx.sessionId), toRemoveConnection(removeConn)))
    // system
    case add: protocol.MapRequest.AddSystem =>
      ctx.mapQ.offer(Identified(Some(ctx.sessionId), toAddSystem(add)))
    case upd: protocol.MapRequest.UpdateSystem =>
      ZIO.when(upd.name.nonEmpty)(
        ctx.mapQ.offer(
          Identified(
            Some(ctx.sessionId),
            MapRequest.RenameSystem(
              systemId = SystemId(upd.systemId),
              name = upd.name.get
            )
          )
        )
      ) *> ZIO.when(upd.displayData.nonEmpty)(
        ctx.mapQ.offer(
          Identified(
            Some(ctx.sessionId),
            MapRequest.UpdateSystemDisplay(
              systemId = SystemId(upd.systemId),
              displayData = toDisplayData(upd.displayData.get)
            )
          )
        )
      ) *> ZIO.when(upd.isPinned.nonEmpty || upd.stance.nonEmpty)(
        ctx.mapQ.offer(
          Identified(
            Some(ctx.sessionId),
            MapRequest.UpdateSystemAttribute(
              systemId = SystemId(upd.systemId),
              pinned = upd.isPinned,
              intelStance = upd.stance.map(toIntelStance)
            )
          )
        )
      )
    case protocol.MapRequest.RemoveSystem(systemId) =>
      ctx.mapQ.offer(Identified(Some(ctx.sessionId), MapRequest.RemoveSystem(SystemId(systemId))))
    case protocol.MapRequest.RemoveSystems(systemIds) =>
      NonEmptyChunk
        .fromChunk(Chunk.fromArray(systemIds))
        .fold(ZIO.unit)(systemIds =>
          ctx.mapQ.offer(Identified(Some(ctx.sessionId), MapRequest.RemoveSystems(systemIds.map(SystemId(_)))))
        )
    // signatures
    case addSig: protocol.MapRequest.AddSystemSignature =>
      ctx.mapQ.offer(
        Identified(
          Some(ctx.sessionId),
          MapRequest.AddSystemSignature(SystemId(addSig.systemId), toNewMapSystemSignature(addSig.sig))
        )
      )
    case protocol.MapRequest.UpdateSystemSignatures(systemId, replaceAll, scanned) =>
      ctx.mapQ.offer(
        Identified(
          Some(ctx.sessionId),
          MapRequest.UpdateSystemSignatures(SystemId(systemId), replaceAll, scanned.map(toNewMapSystemSignature).toList)
        )
      )
    case protocol.MapRequest.RemoveSystemSignatures(systemId, sigIds) =>
      ctx.mapQ.offer(
        Identified(
          Some(ctx.sessionId),
          MapRequest.RemoveSystemSignatures(SystemId(systemId), Chunk.from(sigIds).nonEmptyOrElse(None)(Some(_)))
        )
      )
    case protocol.MapRequest.RemoveAllSystemSignatures(systemId) =>
      ctx.mapQ.offer(
        Identified(
          Some(ctx.sessionId),
          MapRequest.RemoveSystemSignatures(SystemId(systemId), None)
        )
      )

private def sendServerStatus(ourQ: Enqueue[protocol.MapMessage]) =
  ZIO
    .serviceWithZIO[ServerStatusTracker](_.status)
    .flatMap:
      case Left(_) => ourQ.offer(protocol.MapMessage.ServerStatus(protocol.MapServerStatus.Error))
      case Right(s) =>
        ourQ.offer(
          protocol.MapMessage.ServerStatus(
            protocol.MapServerStatus.Online(s.players, s.serverVersion, s.startTime, s.vip.contains(true))
          )
        )

private def isAllowed(msg: protocol.MapRequest, role: model.MapRole): Boolean = (msg, role) match
  case (protocol.MapRequest.GetMapInfo | protocol.MapRequest.GetSnapshot, _) => true
  case (_, model.MapRole.Viewer)                                             => false
  case (_, _)                                                                => true

private def filterToProto(sessionId: MapSessionId)(msg: Identified[MapResponse]): Option[protocol.MapMessage] =
  if (msg.sessionId.forall(_ == sessionId)) toProto(msg.value) else None

private def toProto(msg: MapResponse): Option[protocol.MapMessage] = msg match
  case MapResponse.ConnectionSnapshot(whc, rank) =>
    Some(protocol.MapMessage.ConnectionSnapshot(toProtoConnectionWithSigs(whc, rank)))
  case MapResponse.ConnectionsRemoved(whcs) =>
    Some(protocol.MapMessage.ConnectionsRemoved(whcs.map(toProtoConnection).toArray))
  case MapResponse.ConnectionJumped(jump) => Some(protocol.MapMessage.ConnectionJumped(toProtoConnectionJump(jump)))
  case MapResponse.Error(message)         => Some(protocol.MapMessage.Error(message))
  case MapResponse.MapSnapshot(systems, connections, connectionRanks) =>
    Some(
      protocol.MapMessage.MapSnapshot(
        systems = systems.map((sysId, mss) => (sysId, toProtoSystemSnapshot(mss))),
        connections =
          connections.view.mapValues(c => toProtoConnectionWithSigs(c, connectionRanks(c.connection.id))).toMap
      )
    )
  case MapResponse.SystemSnapshot(systemId, sys, connections, connectionRanks) =>
    Some(
      protocol.MapMessage.SystemSnapshot(
        systemId = systemId,
        system = toProtoSystemSnapshot(sys),
        connections =
          connections.view.mapValues(c => toProtoConnectionWithSigs(c, connectionRanks(c.connection.id))).toMap
      )
    )
  case MapResponse.SystemDisplayUpdate(systemId, name, displayData) =>
    Some(protocol.MapMessage.SystemDisplayUpdate(systemId, name, toProtoDisplay(displayData)))
  case MapResponse.SystemsRemoved(
        removedSystemIds,
        removedConnectionIds,
        updatedSystems,
        updatedConnections,
        updatedConnectionRanks
      ) =>
    Some(
      protocol.MapMessage.SystemsRemoved(
        removedSystemIds = removedSystemIds.toArray,
        removedConnectionIds = removedConnectionIds.toArray,
        updatedSystems = updatedSystems.map(toProtoSystemSnapshot).toArray,
        updatedConnections = updatedConnections.view
          .mapValues(c => toProtoConnectionWithSigs(c, updatedConnectionRanks(c.connection.id)))
          .toMap
      )
    )
  case MapResponse.CharacterLocations(locationMap) =>
    Some(
      protocol.MapMessage.CharacterLocations(
        locationMap
          .groupMap(_._2.system)((cId, inS) => toProtoCharacterLocation(cId, inS))
          .transform((_, i) => i.toArray)
      )
    )

private def toProtoCharacter(char: model.AuthCharacter, authTokenFresh: Boolean) =
  protocol.UserCharacter(
    name = char.name,
    characterId = char.id,
    corporationId = char.corporationId,
    allianceId = char.allianceId,
    authTokenFresh = authTokenFresh
  )

private def toAddSystem(msg: protocol.MapRequest.AddSystem) =
  MapRequest.AddSystem(
    systemId = SystemId(msg.systemId),
    name = msg.name,
    isPinned = msg.isPinned,
    displayData = toDisplayData(msg.displayData),
    stance = msg.stance.map(toIntelStance)
  )

private def toAddSystemConnection(msg: protocol.MapRequest.AddSystemConnection) =
  MapRequest.AddSystemConnection(
    fromSystemId = msg.fromSystemId,
    toSystemId = msg.toSystemId
  )

private def toRemoveConnection(msg: protocol.MapRequest.RemoveSystemConnection) =
  MapRequest.RemoveSystemConnection(msg.connectionId)

private def toNewMapSystemSignature(sig: protocol.NewSystemSignature) = sig match
  case protocol.NewSystemSignature.Unknown(signatureId, _) =>
    NewMapSystemSignature(signatureId, model.SignatureGroup.Unknown)
  case protocol.NewSystemSignature.Site(signatureId, _, group, name) =>
    NewMapSystemSignature(signatureId, toSignatureGroup(group), name)
  case wh: protocol.NewSystemSignature.Wormhole =>
    NewMapSystemSignature(
      signatureId = wh.id,
      signatureGroup = model.SignatureGroup.Wormhole,
      signatureTypeName = None,
      wormholeIsEol = Some(wh.isEol),
      wormholeTypeId = wh.connectionType match
        case protocol.WormholeConnectionType.Known(typeId) => Some(typeId)
        case _                                             => None
      ,
      wormholeMassSize = toWhMassSize(wh.massSize),
      wormholeMassStatus = toWhMassStatus(wh.massStatus),
      wormholeK162Type = wh.connectionType match
        case protocol.WormholeConnectionType.K162(typ) => Some(toWhK162Type(typ))
        case _                                         => None
      ,
      wormholeConnectionId = wh.connectionId
    )

private def toDisplayType(dt: protocol.MapDisplayType) = dt match
  case protocol.MapDisplayType.Manual => model.MapDisplayType.Manual

private def toDisplayData(sd: protocol.SystemDisplayData) = sd match
  case protocol.SystemDisplayData.Manual(x, y) => model.SystemDisplayData.Manual(x, y)

private def toIntelStance(is: protocol.IntelStance) = is match
  case protocol.IntelStance.Unknown  => model.IntelStance.Unknown
  case protocol.IntelStance.Hostile  => model.IntelStance.Hostile
  case protocol.IntelStance.Friendly => model.IntelStance.Friendly

private def toSignatureGroup(sg: protocol.SignatureGroup) = sg match
  case protocol.SignatureGroup.Unknown  => model.SignatureGroup.Unknown
  case protocol.SignatureGroup.Gas      => model.SignatureGroup.Gas
  case protocol.SignatureGroup.Ghost    => model.SignatureGroup.Ghost
  case protocol.SignatureGroup.Data     => model.SignatureGroup.Data
  case protocol.SignatureGroup.Combat   => model.SignatureGroup.Combat
  case protocol.SignatureGroup.Relic    => model.SignatureGroup.Relic
  case protocol.SignatureGroup.Ore      => model.SignatureGroup.Ore
  case protocol.SignatureGroup.Wormhole => model.SignatureGroup.Wormhole

private def toWhMassStatus(ms: protocol.WormholeMassStatus) = ms match
  case protocol.WormholeMassStatus.Unknown  => model.WormholeMassStatus.Unknown
  case protocol.WormholeMassStatus.Critical => model.WormholeMassStatus.Critical
  case protocol.WormholeMassStatus.Fresh    => model.WormholeMassStatus.Fresh
  case protocol.WormholeMassStatus.Reduced  => model.WormholeMassStatus.Reduced

private def toWhMassSize(size: protocol.WormholeMassSize) = size match
  case protocol.WormholeMassSize.Unknown => model.WormholeMassSize.Unknown
  case protocol.WormholeMassSize.S       => model.WormholeMassSize.S
  case protocol.WormholeMassSize.M       => model.WormholeMassSize.M
  case protocol.WormholeMassSize.L       => model.WormholeMassSize.L
  case protocol.WormholeMassSize.XL      => model.WormholeMassSize.XL

private def toWhK162Type(tpe: protocol.WormholeK162Type) = tpe match
  case protocol.WormholeK162Type.Dangerous => model.WormholeK162Type.Dangerous
  case protocol.WormholeK162Type.Deadly    => model.WormholeK162Type.Deadly
  case protocol.WormholeK162Type.Unknown   => model.WormholeK162Type.Unknown
  case protocol.WormholeK162Type.Hisec     => model.WormholeK162Type.Hisec
  case protocol.WormholeK162Type.Losec     => model.WormholeK162Type.Losec
  case protocol.WormholeK162Type.Nullsec   => model.WormholeK162Type.Nullsec
  case protocol.WormholeK162Type.Thera     => model.WormholeK162Type.Thera

private def toProtoSystemSnapshot(value: MapSystemWithAll): protocol.MapSystemSnapshot =
  protocol.MapSystemSnapshot(
    system = toProtoSystem(value.sys, value.display),
    display = value.display.map(toProtoDisplay),
    structures = value.structures.map(toProtoStructure).toArray,
    notes = value.notes.map(toProtoNote).toArray,
    signatures = value.signatures.map(toProtoSignature(value.sys.systemId, _)).toArray,
    connections = value.connections.map(toProtoConnection).toArray
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

private def toProtoSignature(systemId: SystemId, value: model.MapSystemSignature): protocol.MapSystemSignature =
  value.signatureGroup match
    case model.SignatureGroup.Unknown =>
      protocol.MapSystemSignature.Unknown(
        systemId = systemId,
        id = value.signatureId,
        createdAt = value.createdAt,
        createdByCharacterId = value.createdByCharacterId,
        updatedAt = value.updatedAt,
        updatedByCharacterId = value.updatedByCharacterId
      )
    case model.SignatureGroup.Wormhole =>
      protocol.MapSystemSignature.Wormhole(
        systemId = systemId,
        id = value.signatureId,
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
        systemId = systemId,
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
        case model.WormholeK162Type.Hisec     => protocol.WormholeK162Type.Hisec
        case model.WormholeK162Type.Losec     => protocol.WormholeK162Type.Losec
        case model.WormholeK162Type.Nullsec   => protocol.WormholeK162Type.Nullsec
        case model.WormholeK162Type.Thera     => protocol.WormholeK162Type.Thera)
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

private def toProtoConnectionJump(value: model.MapWormholeConnectionJump): protocol.MapWormholeConnectionJump =
  protocol.MapWormholeConnectionJump(
    connectionId = value.connectionId,
    characterId = value.characterId,
    shipTypeId = value.shipTypeId,
    massOverride = value.massOverride,
    createdAt = value.createdAt
  )

private def toProtoConnectionRank(value: MapWormholeConnectionRank): protocol.MapWormholeConnectionRank =
  protocol.MapWormholeConnectionRank(
    fromSystemIdx = value.fromSystemIdx,
    fromSystemCount = value.fromSystemCount,
    toSystemIdx = value.toSystemIdx,
    toSystemCount = value.toSystemCount
  )

private def toProtoConnectionWithSigs(
    value: MapWormholeConnectionWithSigs,
    rank: MapWormholeConnectionRank
): protocol.MapWormholeConnectionWithSigs =
  protocol.MapWormholeConnectionWithSigs(
    connection = toProtoConnection(value.connection),
    jumps = value.jumps.map(toProtoConnectionJump).toArray,
    fromSignature = value.fromSignature.flatMap(toProtoSignatureWormhole(value.connection.fromSystemId, _)),
    toSignature = value.toSignature.flatMap(toProtoSignatureWormhole(value.connection.toSystemId, _)),
    rank = toProtoConnectionRank(rank)
  )

private inline def toProtoSignatureWormhole(
    systemId: SystemId,
    value: model.MapSystemSignature
): Option[protocol.MapSystemSignature.Wormhole] =
  toProtoSignature(systemId, value) match
    case ws: protocol.MapSystemSignature.Wormhole => Some(ws)
    case _                                        => None

private def toProtoCharacterLocation(
    characterId: CharacterId,
    inSystem: CharacterLocationState.InSystem
): protocol.CharacterLocation =
  protocol.CharacterLocation(
    characterId = characterId,
    characterName = inSystem.characterName,
    shipTypeId = inSystem.shipTypeId,
    shipName = inSystem.shipName,
    structureId = inSystem.structureId,
    stationId = inSystem.stationId,
    updatedAt = inSystem.updatedAt
  )
