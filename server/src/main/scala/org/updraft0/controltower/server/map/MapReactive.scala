package org.updraft0.controltower.server.map

import org.updraft0.controltower.db.model.{MapSystemSignature, MapWormholeConnection, displayType}
import org.updraft0.controltower.db.{model, query}
import org.updraft0.controltower.server.db.{
  MapQueries,
  MapSystemWithAll,
  MapWormholeConnectionRank,
  MapWormholeConnectionWithSigs
}
import org.updraft0.minireactive.*
import zio.*

import java.util.UUID
import org.updraft0.controltower.server.Log

import java.time.Instant
import scala.annotation.unused

type MapId        = Long
type MapEnv       = javax.sql.DataSource
type SystemId     = Long
type ConnectionId = Long

private[map] case class MapState(
    systems: Map[SystemId, MapSystemWithAll],
    connections: Map[ConnectionId, MapWormholeConnectionWithSigs],
    connectionRanks: Map[ConnectionId, MapWormholeConnectionRank]
):
  def getSystem(id: SystemId): Option[MapSystemWithAll] = systems.get(id)
  def hasSystem(id: SystemId): Boolean                  = systems.contains(id)

  def connectionsForSystem(id: SystemId): Map[ConnectionId, MapWormholeConnectionWithSigs] =
    systems(id).connections.map(c => c.id -> connections(c.id)).toMap
  def connectionRanksForSystem(id: SystemId): Map[ConnectionId, MapWormholeConnectionRank] =
    systems(id).connections.map(c => c.id -> connectionRanks(c.id)).toMap

  def updateOne(
      systemId: SystemId,
      system: MapSystemWithAll,
      connectionsChange: List[MapWormholeConnectionWithSigs],
      connectionRanks: List[MapWormholeConnectionRank]
  ): MapState =
    this.copy(
      systems = this.systems.updated(systemId, system),
      connections = connectionsChange.foldLeft(this.connections) { case (conns, whc) =>
        conns.updated(whc.connection.id, whc)
      },
      connectionRanks = connectionRanks.foldLeft(this.connectionRanks) { case (ranks, r) =>
        ranks.updated(r.connectionId, r)
      }
    )

  def updateConnection(
      whc: MapWormholeConnectionWithSigs,
      connectionRanks: List[MapWormholeConnectionRank]
  ): MapState =
    this.copy(
      systems = this.systems
        .updatedWith(whc.connection.fromSystemId)(
          _.map(msa => msa.copy(connections = updateConnectionById(msa.connections, whc.connection)))
        )
        .updatedWith(whc.connection.toSystemId)(
          _.map(msa => msa.copy(connections = updateConnectionById(msa.connections, whc.connection)))
        ),
      connections = this.connections.updated(whc.connection.id, whc),
      connectionRanks = connectionRanks.foldLeft(this.connectionRanks) { case (ranks, r) =>
        ranks.updated(r.connectionId, r)
      }
    )

  def removeConnection(
      whc: MapWormholeConnectionWithSigs,
      connectionRanks: List[MapWormholeConnectionRank]
  ): MapState =
    this.copy(
      systems = this.systems
        .updatedWith(whc.connection.fromSystemId)(
          _.map(msa =>
            msa.copy(
              connections = removeConnectionById(msa.connections, whc.connection),
              signatures = removeSignatureById(msa.signatures, whc.fromSignature.map(_.signatureId))
            )
          )
        )
        .updatedWith(whc.connection.toSystemId)(
          _.map(msa =>
            msa.copy(
              connections = removeConnectionById(msa.connections, whc.connection),
              signatures = removeSignatureById(msa.signatures, whc.toSignature.map(_.signatureId))
            )
          )
        ),
      connections = this.connections.removed(whc.connection.id),
      connectionRanks = connectionRanks.foldLeft(this.connectionRanks) { case (ranks, r) =>
        ranks.updated(r.connectionId, r)
      }
    )

  /** Return only ranks that are different
    */
  def diffRanks(ranks: List[MapWormholeConnectionRank]): List[MapWormholeConnectionRank] =
    ranks.filterNot(r => this.connectionRanks.get(r.connectionId).contains(r))

object MapState:
  def apply(
      systems: List[MapSystemWithAll],
      connections: List[MapWormholeConnectionWithSigs],
      connectionRanks: List[MapWormholeConnectionRank]
  ): MapState =
    new MapState(
      systems.map(msa => msa.sys.systemId -> msa).toMap,
      connections.map(whc => whc.connection.id -> whc).toMap,
      connectionRanks.map(whr => whr.connectionId -> whr).toMap
    )

case class MapSessionId(characterId: Long, sessionId: UUID)
case class Identified[T](sessionId: Option[MapSessionId], value: T)

case class NewMapSystemSignature(
    signatureId: String,
    signatureGroup: model.SignatureGroup,
    signatureTypeName: Option[String] = None,
    wormholeIsEol: Option[Boolean] = None,
    wormholeTypeId: Option[Long] = None,
    wormholeMassSize: model.WormholeMassSize = model.WormholeMassSize.Unknown,
    wormholeMassStatus: model.WormholeMassStatus = model.WormholeMassStatus.Unknown,
    wormholeK162Type: Option[model.WormholeK162Type] = None,
    wormholeConnectionId: Option[ConnectionId] = None
)

enum MapRequest:
  case MapSnapshot
  case AddSystem(
      systemId: SystemId,
      name: Option[String],
      isPinned: Boolean,
      displayData: model.SystemDisplayData,
      stance: Option[model.IntelStance]
  )
  case AddSystemConnection(fromSystemId: SystemId, toSystemId: SystemId)
  case AddSystemSignature(
      systemId: SystemId,
      signature: NewMapSystemSignature
  )
  case UpdateSystemAttribute(systemId: SystemId, pinned: Option[Boolean], intelStance: Option[model.IntelStance])
  case UpdateSystemDisplay(systemId: SystemId, displayData: model.SystemDisplayData)
  case UpdateSystemSignatures(systemId: SystemId, replaceAll: Boolean, scanned: List[NewMapSystemSignature])
  case RenameSystem(systemId: SystemId, name: Option[String])
  case RemoveSystem(systemId: SystemId)
  case RemoveSystemSignatures(systemId: SystemId, signatures: Option[NonEmptyChunk[String]])
  case RemoveSystemConnection(connectionId: ConnectionId)

enum MapResponse:
  case ConnectionSnapshot(
      connection: MapWormholeConnectionWithSigs,
      rank: MapWormholeConnectionRank
  )
  case ConnectionsRemoved(connections: List[MapWormholeConnection])
  case Error(message: String)
  case MapSnapshot(
      systems: Map[SystemId, MapSystemWithAll],
      connections: Map[ConnectionId, MapWormholeConnectionWithSigs],
      connectionRanks: Map[ConnectionId, MapWormholeConnectionRank]
  )
  case SystemSnapshot(
      systemId: SystemId,
      sys: MapSystemWithAll,
      connections: Map[ConnectionId, MapWormholeConnectionWithSigs],
      connectionRanks: Map[ConnectionId, MapWormholeConnectionRank]
  )
  case SystemDisplayUpdate(systemId: SystemId, name: Option[String], displayData: model.SystemDisplayData)
  case SystemRemoved(systemId: SystemId)

/** Mini-reactive/lightweight actor that has a state of the whole map in memory and makes corresponding db changes
  */
object MapEntity extends ReactiveEntity[MapEnv, MapId, MapState, Identified[MapRequest], Identified[MapResponse]]:
  override def tag = "Map"

  override def hydrate(key: MapId): URIO[MapEnv, MapState] =
    (for
      systems         <- MapQueries.getMapSystemAll(key)
      connections     <- MapQueries.getWormholeConnectionsWithSigs(key, None)
      connectionRanks <- MapQueries.getWormholeConnectionRanksAll(key)
    yield MapState(systems, connections, connectionRanks)).orDie

  override def handle(
      mapId: MapId,
      state: MapState,
      in: Identified[MapRequest]
  ): URIO[MapEnv, (MapState, Chunk[Identified[MapResponse]])] =
    ZIO
      .clockWith(_.instant)
      .flatMap(now =>
        (query
          .transaction(
            in match
              case Identified(Some(id), MapRequest.MapSnapshot) =>
                ZIO.succeed(
                  state -> reply(id, MapResponse.MapSnapshot(state.systems, state.connections, state.connectionRanks))
                )
              case Identified(Some(sid), add: MapRequest.AddSystem)
                  if state.getSystem(add.systemId).forall(_.display.isEmpty) =>
                identified(sid, "add", addSystem(mapId, state, sid, now, add))
              case Identified(_, add: MapRequest.AddSystem) =>
                ZIO.logDebug(s"no-op adding existing system $add").as(state -> Chunk.empty)
              case Identified(Some(sid), addConn: MapRequest.AddSystemConnection) =>
                whenSystemsExist(state, addConn.fromSystemId, addConn.toSystemId)(
                  identified(sid, "addSystemConnection", insertSystemConnection(mapId, state, sid, now, addConn))
                )
              case Identified(Some(sid), addSig: MapRequest.AddSystemSignature) =>
                whenSystemExists(addSig.systemId, state)(
                  identified(sid, "addSystemSignature", upsertSystemSignature(mapId, state, sid, now, addSig))
                )
              case Identified(Some(sid), usd: MapRequest.UpdateSystemDisplay) =>
                whenSystemExists(usd.systemId, state)(
                  identified(sid, "updateDisplay", updateSystemDisplay(mapId, state, usd))
                )
              case Identified(Some(sid), usa: MapRequest.UpdateSystemAttribute) =>
                whenSystemExists(usa.systemId, state)(
                  identified(sid, "updateAttribute", updateSystemAttribute(mapId, state, sid, now, usa))
                )
              case Identified(Some(sid), uss: MapRequest.UpdateSystemSignatures) =>
                whenSystemExists(uss.systemId, state)(
                  identified(sid, "updateSystemSignatures", updateSystemSignatures(mapId, state, sid, now, uss))
                )
              case Identified(Some(sid), rs: MapRequest.RemoveSystem) =>
                whenSystemExists(rs.systemId, state)(
                  identified(sid, "removeFromDisplay", removeSystemFromDisplay(mapId, state, sid, rs))
                )
              case Identified(Some(sid), rsc: MapRequest.RemoveSystemConnection) =>
                identified(sid, "removeSystemConnection", removeSystemConnection(mapId, state, sid, rsc))
              case Identified(Some(sid), rss: MapRequest.RemoveSystemSignatures) =>
                whenSystemExists(rss.systemId, state)(
                  identified(sid, "removeSystemSignatures", removeSystemSignatures(mapId, state, sid, now, rss))
                )
              case Identified(Some(sid), rs: MapRequest.RenameSystem) =>
                whenSystemExists(rs.systemId, state)(
                  identified(sid, "renameSystem", renameSystem(mapId, state, sid, now, rs))
                )
              // fall-through case
              case Identified(None, _) =>
                ZIO.logWarning("non-identified request not processed").as(state -> Chunk.empty)
          )) @@ Log.MapId(mapId)
      )
      .orDie

  private inline def identified[R, E, A](sid: MapSessionId, op: String, f: ZIO[R, E, A]): ZIO[R, E, A] =
    f @@ Log.SessionId(sid.sessionId) @@ Log.CharacterId(sid.characterId) @@ Log.MapOperation(op)

  private def addSystem(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      add: MapRequest.AddSystem
  ) =
    for
      curr <- query.map.getMapSystem(mapId, add.systemId) // need to get system to not override existing params
      _ <- query.map.upsertMapSystem(
        model.MapSystem(
          mapId = mapId,
          systemId = add.systemId,
          name = add.name.orElse(curr.flatMap(_.name)),
          isPinned = add.isPinned,
          chainNamingStrategy = curr.map(_.chainNamingStrategy).getOrElse(model.ChainNamingStrategy.Manual),
          description = curr.flatMap(_.description),
          stance = add.stance.orElse(curr.map(_.stance)).getOrElse(model.IntelStance.Unknown),
          updatedByCharacterId = sessionId.characterId,
          updatedAt = now
        )
      )
      _ <- query.map.upsertMapSystemDisplay(
        model.MapSystemDisplay(mapId, add.systemId, add.displayData.displayType, add.displayData)
      )
      sys   <- loadSingleSystem(mapId, add.systemId)
      conns <- MapQueries.getWormholeConnectionsWithSigsBySystemId(mapId, add.systemId)
      ranks <- MapQueries.getWormholeConnectionRanksForSystem(mapId, add.systemId)
    yield withState(state.updateOne(sys.sys.systemId, sys, conns, ranks))(s =>
      s -> broadcast(s.systemSnapshot(sys.sys.systemId))
    )

  private def insertSystemConnection(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      addConn: MapRequest.AddSystemConnection
  ) =
    for
      whc <- query.map.insertMapWormholeConnection(
        MapWormholeConnection(
          id = 0L,
          mapId = mapId,
          fromSystemId = addConn.fromSystemId,
          toSystemId = addConn.toSystemId,
          isDeleted = false,
          createdAt = now,
          createdByCharacterId = sessionId.characterId,
          updatedAt = now,
          updatedByCharacterId = sessionId.characterId
        )
      )
      conn  <- loadSingleConnection(mapId, whc.id)
      ranks <- MapQueries.getWormholeConnectionRanksForSystems(mapId, addConn.toSystemId, addConn.fromSystemId)
    yield withState(state.updateConnection(conn, ranks)): nextState =>
      nextState -> broadcastMany(
        state
          .diffRanks(ranks)
          .map(r =>
            MapResponse
              .ConnectionSnapshot(nextState.connections(r.connectionId), nextState.connectionRanks(r.connectionId))
          )*
      )

  private def removeSystemConnection(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      removeConn: MapRequest.RemoveSystemConnection
  ) =
    for
      _ <- query.map.deleteMapWormholeConnection(removeConn.connectionId, sessionId.characterId)
      whcOpt <- MapQueries
        .getWormholeConnectionsWithSigs(mapId, Some(removeConn.connectionId), includeDeleted = true)
        .map(_.headOption)
      ranks <- whcOpt
        .map(whc =>
          MapQueries.getWormholeConnectionRanksForSystems(mapId, whc.connection.toSystemId, whc.connection.fromSystemId)
        )
        .getOrElse(ZIO.succeed(Nil))
    yield whcOpt
      .map(whc => state.removeConnection(whc, ranks) -> broadcast(MapResponse.ConnectionsRemoved(List(whc.connection))))
      .getOrElse(state -> Chunk.empty)

  private def upsertSystemSignature(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      addSig: MapRequest.AddSystemSignature
  ) =
    for
      _ <- query.map.upsertMapSystemSignature(
        newModelSignature(now, sessionId, (mapId, addSig.systemId), addSig.signature)
      )
      sys <- loadSingleSystem(mapId, addSig.systemId)
      connOpt <- addSig.signature.wormholeConnectionId
        .map(whcId => loadSingleConnection(mapId, whcId).map(Some(_)))
        .getOrElse(ZIO.none)
    yield withState(state.updateOne(sys.sys.systemId, sys, connOpt.toList, Nil))(state =>
      state -> broadcast(
        MapResponse.SystemSnapshot(
          systemId = sys.sys.systemId,
          sys = sys,
          connections = state.connectionsForSystem(sys.sys.systemId),
          connectionRanks = state.connectionRanksForSystem(sys.sys.systemId)
        )
      )
    )

  private def updateSystemDisplay(mapId: MapId, state: MapState, usd: MapRequest.UpdateSystemDisplay) =
    for
      _ <- query.map.upsertMapSystemDisplay(
        model.MapSystemDisplay(
          mapId = mapId,
          systemId = usd.systemId,
          displayType = usd.displayData.displayType,
          data = usd.displayData
        )
      )
      sys <- loadSingleSystem(mapId, usd.systemId)
    yield withState(state.updateOne(sys.sys.systemId, sys, Nil, Nil))(nextState =>
      nextState ->
        broadcast(
          if (state.getSystem(sys.sys.systemId).flatMap(_.display).isEmpty) nextState.systemSnapshot(sys.sys.systemId)
          else MapResponse.SystemDisplayUpdate(sys.sys.systemId, sys.sys.name, sys.display.get)
        )
    )

  private def updateSystemAttribute(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      usa: MapRequest.UpdateSystemAttribute
  ) =
    for
      _   <- query.map.updateMapAttribute(mapId, usa.systemId, usa.pinned, usa.intelStance, now, sessionId.characterId)
      sys <- loadSingleSystem(mapId, usa.systemId)
    yield withState(state.updateOne(sys.sys.systemId, sys, Nil, Nil))(nextState =>
      nextState -> sys.display
        .map(_ => broadcast(nextState.systemSnapshot(sys.sys.systemId)))
        .getOrElse(Chunk.empty)
    )

  private def updateSystemSignatures(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      uss: MapRequest.UpdateSystemSignatures
  ) =
    // TODO: signature updates cannot currently change connection ids so only a single system needs to be reloaded
    for
      _ <-
        if (uss.replaceAll) query.map.deleteMapSystemSignatures(mapId, uss.systemId, now, sessionId.characterId)
        else ZIO.succeed(0)
      mapSystemId = (mapId, uss.systemId)
      mapSystem   = state.getSystem(uss.systemId).get
      _ <- ZIO.foreachDiscard(
        uss.scanned
          .map(lookupExisting(mapSystem, _))
          .map((prevOpt, newSig) => toModelSignature(now, sessionId, mapSystemId, prevOpt, newSig))
      )(query.map.upsertMapSystemSignature)
      sys   <- loadSingleSystem(mapId, uss.systemId)
      conns <- MapQueries.getWormholeConnectionsWithSigsBySystemId(mapId, uss.systemId)
    yield withState(state.updateOne(sys.sys.systemId, sys, conns, Nil))(nextState =>
      nextState -> broadcast(nextState.systemSnapshot(sys.sys.systemId))
    )

  private def removeSystemFromDisplay(
      mapId: MapId,
      state: MapState,
      @unused sessionId: MapSessionId,
      rs: MapRequest.RemoveSystem
  ) =
    for
      _   <- query.map.deleteMapSystemDisplay(mapId, rs.systemId)
      sys <- loadSingleSystem(mapId, rs.systemId)
    yield state.updateOne(sys.sys.systemId, sys, Nil, Nil) -> broadcast(MapResponse.SystemRemoved(sys.sys.systemId))

  private def renameSystem(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      rs: MapRequest.RenameSystem
  ) =
    for
      _   <- query.map.updateMapSystemName(mapId, rs.systemId, rs.name, now, sessionId.characterId)
      sys <- loadSingleSystem(mapId, rs.systemId)
    yield state.updateOne(sys.sys.systemId, sys, Nil, Nil) -> sys.display
      .map(displayData => broadcast(MapResponse.SystemDisplayUpdate(sys.sys.systemId, sys.sys.name, displayData)))
      .getOrElse(Chunk.empty)

  private def removeSystemSignatures(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      rss: MapRequest.RemoveSystemSignatures
  ) =
    for
      _ <- rss.signatures match
        case None => query.map.deleteMapSystemSignatures(mapId, rss.systemId, now, sessionId.characterId)
        case Some(ids) =>
          ZIO.foreach(ids)(id => query.map.deleteMapSystemSignature(mapId, rss.systemId, id, sessionId.characterId))
      sys   <- loadSingleSystem(mapId, rss.systemId)
      conns <- MapQueries.getWormholeConnectionsWithSigsBySystemId(mapId, rss.systemId)
    yield withState(state.updateOne(sys.sys.systemId, sys, conns, Nil))(s =>
      s -> broadcast(s.systemSnapshot(sys.sys.systemId))
    )

  private inline def loadSingleSystem(mapId: MapId, systemId: SystemId) =
    MapQueries
      .getMapSystemAll(mapId, Some(systemId))
      .filterOrDieMessage(_.size == 1)(s"BUG: expected exactly 1 system to be returned")
      .map(_.head)

  private inline def loadSingleConnection(mapId: MapId, connectionId: ConnectionId) =
    MapQueries
      .getWormholeConnectionsWithSigs(mapId, Some(connectionId))
      .filterOrDieMessage(_.size == 1)(s"BUG: expected exactly 1 connection to be returned")
      .map(_.head)

  private inline def whenSystemExists(systemId: SystemId, state: MapState)(
      f: RIO[MapEnv, (MapState, Chunk[Identified[MapResponse]])]
  ) =
    ZIO.when(state.hasSystem(systemId))(f).map(_.getOrElse(state -> Chunk.empty)) @@ Log.SystemId(systemId)

  private inline def whenSystemsExist(state: MapState, systemIds: SystemId*)(
      f: RIO[MapEnv, (MapState, Chunk[Identified[MapResponse]])]
  ) =
    ZIO.when(systemIds.forall(state.hasSystem(_)))(f).map(_.getOrElse(state -> Chunk.empty)) @@ Log.SystemIds(systemIds)

  private inline def lookupExisting(
      prev: MapSystemWithAll,
      newSig: NewMapSystemSignature
  ): (Option[model.MapSystemSignature], NewMapSystemSignature) =
    prev.signatures.find(_.signatureId == newSig.signatureId) -> newSig

object MapReactive:
  private val MailboxSize = 128 // TODO - configurable?

  type Service = MiniReactive[MapId, Identified[MapRequest], Identified[MapResponse]]

  def layer: ZLayer[MapEnv, Nothing, Service] =
    MiniReactive.layer(MapEntity, MiniReactiveConfig(MailboxSize, 10.seconds))

private[map] def toModelSignature(
    now: Instant,
    sessionId: MapSessionId,
    mapSystem: (MapId, SystemId),
    prevOpt: Option[model.MapSystemSignature],
    newSig: NewMapSystemSignature
): model.MapSystemSignature =
  prevOpt match
    case Some(prevSig) => mergeModelSignature(now, sessionId, mapSystem, prevSig, newSig)
    case None          => newModelSignature(now, sessionId, mapSystem, newSig)

private def newModelSignature(
    now: Instant,
    sessionId: MapSessionId,
    mapSystem: (MapId, SystemId),
    newSig: NewMapSystemSignature
): model.MapSystemSignature =
  model.MapSystemSignature(
    mapId = mapSystem._1,
    systemId = mapSystem._2,
    signatureId = newSig.signatureId,
    isDeleted = false,
    signatureGroup = newSig.signatureGroup,
    signatureTypeName = newSig.signatureTypeName,
    wormholeIsEol = newSig.wormholeIsEol,
    wormholeTypeId = newSig.wormholeTypeId,
    wormholeEolAt = newSig.wormholeIsEol.filter(_ == true).map(_ => now),
    wormholeMassSize = Option.when(newSig.signatureGroup == model.SignatureGroup.Wormhole)(newSig.wormholeMassSize),
    wormholeMassStatus = Option.when(newSig.signatureGroup == model.SignatureGroup.Wormhole)(newSig.wormholeMassStatus),
    wormholeK162Type = newSig.wormholeK162Type,
    wormholeConnectionId = newSig.wormholeConnectionId,
    createdAt = now,
    createdByCharacterId = sessionId.characterId,
    updatedAt = now,
    updatedByCharacterId = sessionId.characterId
  )

private def mergeModelSignature(
    now: Instant,
    sessionId: MapSessionId,
    mapSystem: (MapId, SystemId),
    prev: model.MapSystemSignature,
    newSig: NewMapSystemSignature
): model.MapSystemSignature =
  if (newSig.signatureGroup == model.SignatureGroup.Unknown)
    prev.copy(isDeleted = false, updatedAt = now, updatedByCharacterId = sessionId.characterId)
  else if (prev.signatureGroup != newSig.signatureGroup)
    newModelSignature(now, sessionId, mapSystem, newSig).copy(
      createdAt = prev.createdAt,
      createdByCharacterId = prev.createdByCharacterId
    )
  else
    val k162Changed =
      (prev.wormholeK162Type.isDefined && prev.wormholeTypeId.isEmpty && newSig.wormholeK162Type.isEmpty && newSig.wormholeTypeId.isDefined) || (
        prev.wormholeK162Type.isEmpty && prev.wormholeTypeId.isDefined && newSig.wormholeK162Type.isDefined && newSig.wormholeTypeId.isEmpty
      )

    prev.copy(
      isDeleted = false,
      signatureTypeName = newSig.signatureTypeName.orElse(prev.signatureTypeName),
      wormholeIsEol = newSig.wormholeIsEol.orElse(prev.wormholeIsEol),
      wormholeTypeId =
        if (k162Changed) newSig.wormholeTypeId
        else newSig.wormholeTypeId.orElse(prev.wormholeTypeId) /* TODO how to represent removing the type id? */,
      wormholeEolAt =
        if (newSig.wormholeIsEol.isEmpty) prev.wormholeEolAt
        else newSig.wormholeIsEol.filter(_ == true).map(_ => now),
      wormholeMassSize =
        if (newSig.wormholeMassSize == model.WormholeMassSize.Unknown) prev.wormholeMassSize
        else Option.when(newSig.signatureGroup == model.SignatureGroup.Wormhole)(newSig.wormholeMassSize),
      wormholeMassStatus =
        if (newSig.wormholeMassStatus == model.WormholeMassStatus.Unknown) prev.wormholeMassStatus
        else Option.when(newSig.signatureGroup == model.SignatureGroup.Wormhole)(newSig.wormholeMassStatus),
      wormholeK162Type =
        if (k162Changed) newSig.wormholeK162Type
        else newSig.wormholeK162Type.orElse(prev.wormholeK162Type),
      wormholeConnectionId = newSig.wormholeConnectionId.orElse(prev.wormholeConnectionId),
      updatedAt = now,
      updatedByCharacterId = sessionId.characterId
    )

private inline def reply(sessionId: MapSessionId, value: MapResponse): Chunk[Identified[MapResponse]] =
  Chunk.single(Identified(Some(sessionId), value))
private inline def broadcast(value: MapResponse): Chunk[Identified[MapResponse]] =
  Chunk.single(Identified(None, value))
private inline def broadcastMany(values: MapResponse*): Chunk[Identified[MapResponse]] =
  Chunk(values.map(v => Identified(None, v))*)

private inline def withState[A](state: MapState)(f: MapState => A): A = f(state)

private inline def updateConnectionById(
    arr: Array[MapWormholeConnection],
    whc: MapWormholeConnection
): Array[MapWormholeConnection] =
  arr.indexWhere(_.id == whc.id) match
    case -1 => arr.appended(whc)
    case idx =>
      arr.update(idx, whc)
      arr

private inline def removeConnectionById(
    arr: Array[MapWormholeConnection],
    whc: MapWormholeConnection
) =
  arr.indexWhere(_.id == whc.id) match
    case -1  => arr
    case idx => arr.patch(idx, Nil, 1)

private inline def removeSignatureById(arr: Array[MapSystemSignature], idOpt: Option[String]) =
  idOpt
    .map(sigId =>
      arr.indexWhere(_.signatureId == sigId) match
        case -1  => arr
        case idx => arr.patch(idx, Nil, 1)
    )
    .getOrElse(arr)

// convenience methods
extension (s: MapState)
  def systemSnapshot(systemId: SystemId): MapResponse.SystemSnapshot =
    MapResponse.SystemSnapshot(
      systemId = systemId,
      sys = s.systems(systemId),
      connections = s.connectionsForSystem(systemId),
      connectionRanks = s.connectionRanksForSystem(systemId)
    )
