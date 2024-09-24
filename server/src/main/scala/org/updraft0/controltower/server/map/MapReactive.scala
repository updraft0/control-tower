package org.updraft0.controltower.server.map

import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model.{
  MapSystemSignature,
  MapWormholeConnection,
  MapWormholeConnectionJump,
  displayType
}
import org.updraft0.controltower.db.{model, query}
import org.updraft0.controltower.server.db.*
import org.updraft0.controltower.server.*
import org.updraft0.controltower.server.tracking.{CharacterLocationState, LocationTracker, LocationUpdate}
import org.updraft0.minireactive.*
import zio.*

import java.time.Instant
import java.util.UUID
import java.util.concurrent.TimeoutException

type MapEnv     = MapConfig & javax.sql.DataSource & LocationTracker & MapPermissionTracker
type ShipTypeId = Int

case class MapConfig(
    cleanupPeriod: Duration,
    staleConnectionRemovalInterval: Duration,
    eolConnectionRemovalInterval: Duration,
    hardDeletionInterval: Duration,
    queryTimeout: Duration
)

private[map] case class MapSolarSystem(
    systemId: SystemId,
    name: String,
    whClass: WormholeClass,
    regionId: Long,
    constellationId: Long,
    gates: Map[SystemId, Long]
) derives CanEqual

private[map] case class MapRef(solarSystems: Map[SystemId, MapSolarSystem])

private[map] case class MapState(
    systems: Map[SystemId, MapSystemWithAll],
    connections: Map[ConnectionId, MapWormholeConnectionWithSigs],
    connectionRanks: Map[ConnectionId, MapWormholeConnectionRank],
    locations: Map[CharacterId, MapLocationState],
    locationsOnline: Map[
      CharacterId,
      (SystemId, ShipTypeId)
    ], // manual cache of location systems - used to prevent too many updates
    ref: MapRef,
    queryTimeout: Duration
):
  // TODO this should be loaded from DB
  val displayType: model.MapDisplayType = model.MapDisplayType.Manual

  def locationsForUpdate: Map[CharacterId, CharacterLocationState.InSystem] =
    locations.view.filter(_._2.locationInfo.isDefined).mapValues(_.locationInfo.get).toMap

  def getSystem(id: SystemId): Option[MapSystemWithAll] = systems.get(id)
  def hasSystem(id: SystemId): Boolean                  = systems.get(id).exists(_.display.nonEmpty)

  private inline def hasConnectionInternal(fromSystem: SystemId, toSystem: SystemId): Boolean =
    systems.get(fromSystem).exists(_.connections.exists(c => c.toSystemId == toSystem || c.fromSystemId == toSystem))

  def hasConnection(fromSystem: SystemId, toSystem: SystemId): Boolean =
    val res       = hasConnectionInternal(fromSystem, toSystem)
    val otherSide = hasConnectionInternal(toSystem, fromSystem)
    if (res != otherSide)
      throw new IllegalStateException(
        s"Inconsistent state: ${fromSystem}-->${toSystem} @ $res but ${toSystem}-->${fromSystem} @ $otherSide"
      )
    res

  def getConnection(fromSystem: SystemId, toSystem: SystemId): MapWormholeConnection =
    systems(fromSystem).connections.find(c => c.toSystemId == toSystem || c.fromSystemId == toSystem).head
  def hasGateBetween(fromSystem: SystemId, toSystem: SystemId): Boolean =
    ref.solarSystems.get(fromSystem).exists(_.gates.contains(toSystem))

  def connectionsForSystem(id: SystemId): Map[ConnectionId, MapWormholeConnectionWithSigs] =
    systems(id).connections.map(c => c.id -> connections(c.id)).toMap
  def connectionsForSystems(ids: Chunk[SystemId]): Chunk[MapWormholeConnection] =
    ids.foldLeft(Chunk.empty)((s, sId) => s ++ systems.get(sId).map(_.connections).getOrElse(Chunk.empty))

  def connectionRanksForSystem(id: SystemId): Map[ConnectionId, MapWormholeConnectionRank] =
    systems(id).connections.map(c => c.id -> connectionRanks(c.id)).toMap

  def refSystem(id: SystemId): Option[MapSolarSystem] = ref.solarSystems.get(id)

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

  def removeSystems(
      removedSystemIds: Chunk[SystemId],
      removedConnectionIds: Chunk[ConnectionId],
      refreshedSystemIds: Chunk[SystemId],
      connectionRanks: List[MapWormholeConnectionRank],
      connectionsWithSigs: List[MapWormholeConnectionWithSigs]
  ): MapState =
    this.copy(
      systems = refreshedSystemIds.foldLeft(this.systems.removedAll(removedSystemIds))((ss, refreshedSystemId) =>
        ss.updatedWith(refreshedSystemId) {
          case None =>
            throw new IllegalStateException(s"System ${refreshedSystemId} was refreshed but not present in state")
          case Some(prev) =>
            Some(
              prev.copy(
                connections = prev.connections.filterNot(c => removedConnectionIds.contains(c.id)),
                signatures = prev.signatures.filterNot(_.wormholeConnectionId.exists(removedConnectionIds.contains))
              )
            )
        }
      ),
      connections = connectionsWithSigs.foldLeft(this.connections.removedAll(removedConnectionIds))((cc, mwhcs) =>
        cc.updated(mwhcs.connection.id, mwhcs)
      ),
      connectionRanks = connectionRanks.map(whcr => whcr.connectionId -> whcr).toMap
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

  def addConnectionJump(jump: MapWormholeConnectionJump): MapState =
    this.copy(
      connections = this.connections.updatedWith(jump.connectionId)(
        _.map(whcs => whcs.copy(jumps = (whcs.jumps :+ jump).sortBy(_.createdAt)))
      )
    )

  /** Return only ranks that are different
    */
  def diffRanks(ranks: List[MapWormholeConnectionRank]): List[MapWormholeConnectionRank] =
    ranks.filterNot(r => this.connectionRanks.get(r.connectionId).contains(r))

object MapState:
  def apply(
      systems: List[MapSystemWithAll],
      connections: List[MapWormholeConnectionWithSigs],
      connectionRanks: List[MapWormholeConnectionRank],
      ref: MapRef,
      queryTimeout: Duration
  ): MapState =
    new MapState(
      systems.map(msa => msa.sys.systemId -> msa).toMap,
      connections.map(whc => whc.connection.id -> whc).toMap,
      connectionRanks.map(whr => whr.connectionId -> whr).toMap,
      locations = Map.empty,
      locationsOnline = Map.empty,
      ref,
      queryTimeout = queryTimeout
    )

case class MapSessionId(characterId: CharacterId, sessionId: UUID) derives CanEqual
case class Identified[T](sessionId: Option[MapSessionId], value: T)

case class NewMapSystemSignature(
    signatureId: SigId,
    signatureGroup: model.SignatureGroup,
    signatureTypeName: Option[String] = None,
    wormholeIsEol: Option[Boolean] = None,
    wormholeTypeId: Option[Long] = None,
    wormholeMassSize: model.WormholeMassSize = model.WormholeMassSize.Unknown,
    wormholeMassStatus: model.WormholeMassStatus = model.WormholeMassStatus.Unknown,
    wormholeK162Type: Option[model.WormholeK162Type] = None,
    wormholeConnectionId: UnknownOrUnset[ConnectionId] = UnknownOrUnset.Unknown()
)

private[map] case class MapLocationState(
    characterId: CharacterId,
    role: model.MapRole,
    online: Boolean,
    locationInfo: Option[CharacterLocationState.InSystem]
)

private[map] enum LocationUpdateAction:
  case AddMapSystem(
      system: SystemId,
      characterId: CharacterId,
      role: model.MapRole,
      adjacentTo: Option[SystemId],
      updatedAt: Instant
  )
  case AddMapConnection(
      fromSystem: SystemId,
      toSystem: SystemId,
      characterId: CharacterId,
      role: model.MapRole,
      info: CharacterLocationState.InSystem
  )
  case AddJump(
      characterId: CharacterId,
      fromSystem: SystemId,
      toSystem: SystemId,
      info: CharacterLocationState.InSystem
  )

private[map] sealed trait InternalMapRequest

enum MapRequest derives CanEqual:
  case MapSnapshot
  case AddSystem(
      systemId: SystemId,
      name: Option[Option[String]],
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
  case RemoveSystems(systemIds: NonEmptyChunk[SystemId])
  case RemoveSystemSignatures(systemId: SystemId, signatures: Option[NonEmptyChunk[SigId]])
  case RemoveSystemConnection(connectionId: ConnectionId)
  // internals
  case UpdateLocations(u: LocationUpdate)                         extends MapRequest with InternalMapRequest
  case UpdateCharacters(roleMap: Map[CharacterId, model.MapRole]) extends MapRequest with InternalMapRequest
  case RemoveOldConnections                                       extends MapRequest with InternalMapRequest

enum MapResponse:
  case ConnectionSnapshot(
      connection: MapWormholeConnectionWithSigs,
      rank: MapWormholeConnectionRank
  )
  case ConnectionsRemoved(connections: List[MapWormholeConnection])
  case ConnectionJumped(jump: MapWormholeConnectionJump)
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
  case SystemsRemoved(
      removedSystemIds: Chunk[SystemId],
      removedConnectionIds: Chunk[ConnectionId],
      updatedSystems: Chunk[MapSystemWithAll],
      updatedConnections: Map[ConnectionId, MapWormholeConnectionWithSigs],
      updatedConnectionRanks: Map[ConnectionId, MapWormholeConnectionRank]
  )
  case CharacterLocations(locations: Map[CharacterId, CharacterLocationState.InSystem])

/** Mini-reactive/lightweight actor that has a state of the whole map in memory and makes corresponding db changes
  */
object MapEntity extends ReactiveEntity[MapEnv, MapId, MapState, Identified[MapRequest], Identified[MapResponse]]:
  override def tag = "Map"

  override def hydrate(key: MapId, in: Enqueue[Identified[MapRequest]]): URIO[Scope & MapEnv, MapState] =
    // FIXME there is a race condition here
    (for
      config          <- ZIO.service[MapConfig]
      systems         <- MapQueries.getMapSystemAll(key)
      connections     <- MapQueries.getWormholeConnectionsWithSigs(key, None)
      connectionRanks <- MapQueries.getWormholeConnectionRanksAll(key)
      mapRef          <- loadMapRef()
      // transient state - listen to location updates
      locationUpdates <- ZIO.serviceWithZIO[LocationTracker](_.updates)
      _ <- locationUpdates.take
        .flatMap(u => in.offer(Identified(None, MapRequest.UpdateLocations(u))))
        .forever
        .ignoreLogged
        .forkScoped
      // transient state - listen to map permission updates
      permissionUpdates <- ZIO.serviceWithZIO[MapPermissionTracker](_.subscribe(key))
      _ <- permissionUpdates.take
        .flatMap {
          case MapSessionMessage.MapCharacters(`key`, roleMap) =>
            in.offer(Identified(None, MapRequest.UpdateCharacters(roleMap)))
          case _ => ZIO.unit
        }
        .forever
        .ignoreLogged
        .forkScoped
      // self-messages for cleanup
      _ <- in
        .offer(Identified(None, MapRequest.RemoveOldConnections))
        .repeat(Schedule.fixed(config.cleanupPeriod))
        .ignoreLogged
        .forkScoped
    yield MapState(systems, connections, connectionRanks, mapRef, config.queryTimeout)).orDie

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
              case Identified(Some(id), MapRequest.MapSnapshot) if state.locationsOnline.nonEmpty =>
                // the check for non-empty locations prevents a race where the map startup has not yet received roles
                ZIO.succeed(
                  state -> replyMany(
                    id,
                    MapResponse.MapSnapshot(state.systems, state.connections, state.connectionRanks),
                    MapResponse.CharacterLocations(state.locationsForUpdate)
                  )
                )
              case Identified(Some(id), MapRequest.MapSnapshot) =>
                ZIO.succeed(
                  state -> reply(id, MapResponse.MapSnapshot(state.systems, state.connections, state.connectionRanks))
                )
              case Identified(Some(sid), add: MapRequest.AddSystem) if !state.hasSystem(add.systemId) =>
                identified(sid, "add", addSystem(mapId, state, sid.characterId, now, add))
              case Identified(_, add: MapRequest.AddSystem) =>
                ZIO.logDebug(s"no-op adding existing system $add").as(state -> Chunk.empty)
              case Identified(Some(sid), addConn: MapRequest.AddSystemConnection) =>
                whenSystemsExist(state, addConn.fromSystemId, addConn.toSystemId)(
                  identified(
                    sid,
                    "addSystemConnection",
                    insertSystemConnection(mapId, state, sid.characterId, now, addConn)
                  )
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
                  identified(
                    sid,
                    "removeFromDisplay",
                    removeSystemsAndConnections(mapId, state, sid, NonEmptyChunk(rs.systemId))
                  )
                )
              case Identified(Some(sid), rss: MapRequest.RemoveSystems) =>
                NonEmptyChunk
                  .fromChunk(rss.systemIds.filter(state.hasSystem))
                  .fold(ZIO.logDebug("no-op removing systems that do not exist").as(state -> Chunk.empty))(systemIds =>
                    identified(
                      sid,
                      "removeMultipleFromDisplay",
                      removeSystemsAndConnections(mapId, state, sid, systemIds)
                    )
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
              case Identified(_, ul: MapRequest.UpdateLocations) =>
                updateCharacterLocations(mapId, state, ul.u) @@ Log.MapOperation("updateLocations")
              case Identified(_, uc: MapRequest.UpdateCharacters) =>
                updateCharacterRoles(mapId, state, uc.roleMap) @@ Log.MapOperation("updateCharacters")
              case Identified(_, MapRequest.RemoveOldConnections) =>
                removeOldConnections(mapId, state) @@ Log.MapOperation("removeOldConnections")
              // fall-through case
              case Identified(None, _) =>
                ZIO.logWarning("non-identified request not processed").as(state -> Chunk.empty)
            ,
            timeoutOpt = Some(state.queryTimeout)
          )) @@ Log.MapId(mapId)
      )
      .logError("map operation failed")
      .catchSome { case _: TimeoutException =>
        ZIO.succeed((state, Chunk(Identified(in.sessionId, MapResponse.Error("Action took too long, try again")))))
      }
      .orDie

  private inline def identified[R, E, A](sid: MapSessionId, op: String, f: ZIO[R, E, A]): ZIO[R, E, A] =
    f @@ Log.SessionId(sid.sessionId) @@ Log.CharacterId(sid.characterId) @@ Log.MapOperation(op)

  private def addSystem(
      mapId: MapId,
      state: MapState,
      charId: CharacterId,
      now: Instant,
      add: MapRequest.AddSystem
  ) =
    for
      curr <- query.map.getMapSystem(mapId, add.systemId) // need to get system to not override existing params
      _ <- query.map.upsertMapSystem(
        model.MapSystem(
          mapId = mapId,
          systemId = add.systemId,
          name = add.name.getOrElse(curr.flatMap(_.name)),
          isPinned = add.isPinned,
          chainNamingStrategy = curr.map(_.chainNamingStrategy).getOrElse(model.ChainNamingStrategy.Manual),
          description = curr.flatMap(_.description),
          stance = add.stance.orElse(curr.map(_.stance)).getOrElse(model.IntelStance.Unknown),
          updatedByCharacterId = charId,
          updatedAt = now
        )
      )
      _ <- query.map.upsertMapSystemDisplay(
        model.MapSystemDisplay(mapId, add.systemId, add.displayData.displayType, add.displayData)
      )
      resp <- reloadSystemSnapshot(mapId, add.systemId)(state)
    yield resp

  private def insertSystemConnection(
      mapId: MapId,
      state: MapState,
      charId: CharacterId,
      now: Instant,
      addConn: MapRequest.AddSystemConnection
  ) =
    for
      whc <- query.map.insertMapWormholeConnection(
        MapWormholeConnection(
          id = ConnectionId.Invalid,
          mapId = mapId,
          fromSystemId = addConn.fromSystemId,
          toSystemId = addConn.toSystemId,
          isDeleted = false,
          createdAt = now,
          createdByCharacterId = charId,
          updatedAt = now,
          updatedByCharacterId = charId
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

  private def insertSystemConnectionJump(
      mapId: MapId,
      state: MapState,
      charId: CharacterId,
      now: Instant,
      connectionId: ConnectionId,
      shipTypeId: Int
  ) =
    val jump = model.MapWormholeConnectionJump(connectionId, charId, shipTypeId, None, now)
    query.map
      .insertMapWormholeConnectionJump(jump)
      .as(state.addConnectionJump(jump) -> broadcast(MapResponse.ConnectionJumped(jump)))

  private def removeSystemConnection(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      removeConn: MapRequest.RemoveSystemConnection
  ) =
    for
      _ <- query.map.deleteMapWormholeConnection(mapId, removeConn.connectionId, sessionId.characterId)
      // delete any signatures that map to those connection ids
      _ <- query.map.deleteSignaturesWithConnectionIds(mapId, Chunk(removeConn.connectionId), sessionId.characterId)
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
      connOpt <- addSig.signature.wormholeConnectionId.asOption
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
    // when replacing all and the signatures being removed have connection ids, those connections need to be removed
    // this is very similar to removeSystemSignatures()
    val updateSigIds = uss.scanned.map(_.signatureId).toSet
    val removedSignatures =
      Option
        .when(uss.replaceAll)(
          state.getSystem(uss.systemId).map(_.signatures.filterNot(mss => updateSigIds.contains(mss.signatureId)))
        )
        .flatten
        .getOrElse(Chunk.empty)
    val removedConnectionIds = removedSignatures.flatMap(_.wormholeConnectionId.toChunk)
    // gather all system ids those connections affect
    val systemIdsToRefresh =
      removedConnectionIds
        .map(state.connections)
        .toSet
        .flatMap(whc => Set(whc.connection.toSystemId, whc.connection.fromSystemId))
    for
      // delete the connections if any were found
      _ <- query.map.deleteMapWormholeConnections(mapId, removedConnectionIds, sessionId.characterId)
      // delete any signatures that map to those connection ids
      _ <- query.map.deleteSignaturesWithConnectionIds(mapId, removedConnectionIds, sessionId.characterId)
      deletedConnections <- query.map.getWormholeConnections(mapId, removedConnectionIds, isDeleted = true)
      // delete signatures when replacing all
      _ <- query.map.deleteMapSystemSignaturesAll(mapId, uss.systemId, now, sessionId.characterId).when(uss.replaceAll)
      mapSystemId = (mapId, uss.systemId)
      mapSystem   = state.getSystem(uss.systemId).get
      _ <- ZIO.foreachDiscard(
        uss.scanned
          .map(lookupExisting(mapSystem, _))
          .map((prevOpt, newSig) => toModelSignature(now, sessionId, mapSystemId, prevOpt, newSig))
      )(query.map.upsertMapSystemSignature)
      // if some connections were found, will reload multiple systems
      resp <-
        if (systemIdsToRefresh.nonEmpty)
          combineMany(
            state,
            Chunk(removeConnections(deletedConnections)) ++
              systemIdsToRefresh.map(sId => reloadSystemSnapshot(mapId, sId))
          )
        else reloadSystemSnapshot(mapId, uss.systemId)(state)
    yield resp

  private def removeSystemsAndConnections(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      systemIds: NonEmptyChunk[SystemId]
  ) =
    val connections   = state.connectionsForSystems(systemIds.toChunk)
    val connectionIds = connections.map(_.id).sorted.dedupe
    val refreshedSystemIds = connections
      .flatMap(c => Chunk(c.toSystemId, c.fromSystemId).filterNot(sId => systemIds.contains(sId)))
      .sorted
      .dedupe
    for
      // trace log
      _ <- ZIO.logTrace(
        s"Removing multiple systems [${systemIds.mkString(",")}], caused ${connectionIds.size} connection removals and ${refreshedSystemIds.size} system updates"
      )
      // mark connections as removed
      _                  <- query.map.deleteMapWormholeConnections(mapId, connectionIds, sessionId.characterId)
      deletedConnections <- query.map.getWormholeConnections(mapId, connectionIds, isDeleted = true)
      // remove signatures that have those connections
      _ <- query.map.deleteSignaturesWithConnectionIds(mapId, connectionIds, sessionId.characterId)
      // remove the display of the system
      _ <- query.map.deleteMapSystemDisplays(mapId, systemIds.toChunk)
      // recompute all the connection ranks
      connectionRanks <- MapQueries.getWormholeConnectionRanksAll(mapId)
      // load all the refreshed connections with sigs
      connectionsWithSigs <- MapQueries.getWormholeConnectionsWithSigsBySystemIds(mapId, refreshedSystemIds)
    yield withState(
      state.removeSystems(
        systemIds.toChunk,
        connectionIds,
        refreshedSystemIds,
        connectionRanks,
        connectionsWithSigs
      )
    )(nextState =>
      nextState -> broadcast(
        MapResponse.SystemsRemoved(
          removedSystemIds = systemIds.toChunk,
          removedConnectionIds = connectionIds,
          updatedSystems = refreshedSystemIds.map(nextState.systems),
          updatedConnections = connectionsWithSigs.map(whcs => whcs.connection.id -> whcs).toMap,
          updatedConnectionRanks = nextState.connectionRanks
        )
      )
    )

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
      // gather all connection ids present in the signatures about to be deleted
      connectionIds <- query.map.getSystemConnectionIdsInSignatures(mapId, rss.systemId, rss.signatures.map(_.toChunk))
      // gather all system ids those connections affect
      systemIdsToRefresh = Chunk.fromIterable(
        connectionIds
          .map(state.connections)
          .toSet
          .flatMap(whc => Set(whc.connection.toSystemId, whc.connection.fromSystemId))
      )
      // delete the connections if any were found
      _ <- query.map.deleteMapWormholeConnections(mapId, Chunk.fromIterable(connectionIds), sessionId.characterId)
      // delete any signatures that map to those connection ids
      _ <- query.map.deleteSignaturesWithConnectionIds(mapId, Chunk.fromIterable(connectionIds), sessionId.characterId)
      deletedConnections <- query.map.getWormholeConnections(mapId, Chunk.fromIterable(connectionIds), isDeleted = true)
      // delete the signatures
      _ <- rss.signatures match
        case None      => query.map.deleteMapSystemSignaturesAll(mapId, rss.systemId, now, sessionId.characterId)
        case Some(ids) => query.map.deleteMapSystemSignatures(mapId, rss.systemId, ids, sessionId.characterId)
      // if some connections were found, will reload multiple systems
      resp <-
        if (systemIdsToRefresh.nonEmpty)
          combineMany(
            state,
            Chunk(removeConnections(deletedConnections)) ++
              systemIdsToRefresh.map(sId => reloadSystemSnapshot(mapId, sId))
          )
        else reloadSystemSnapshot(mapId, rss.systemId)(state)
    yield resp

  // partially remove connections - state will be updated later with systems
  private def removeConnections(connectionsRemoved: List[MapWormholeConnection])(state: MapState) =
    val connectionIds = connectionsRemoved.map(_.id)
    val nextState = state.copy(
      connections = state.connections.removedAll(connectionIds),
      connectionRanks = state.connectionRanks.removedAll(connectionIds)
    )
    val resp =
      if (connectionsRemoved.nonEmpty) broadcast(MapResponse.ConnectionsRemoved(connectionsRemoved)) else Chunk.empty
    ZIO.succeed((nextState, resp))

  private def reloadSystemSnapshot(mapId: MapId, systemId: SystemId)(state: MapState) =
    for
      sys   <- loadSingleSystem(mapId, systemId)
      conns <- MapQueries.getWormholeConnectionsWithSigsBySystemId(mapId, systemId)
      ranks <- MapQueries.getWormholeConnectionRanksForSystem(mapId, systemId)
    yield withState(state.updateOne(sys.sys.systemId, sys, conns, ranks))(s =>
      s -> broadcast(s.systemSnapshot(sys.sys.systemId))
    )

  private def updateCharacterLocations(
      mapId: MapId,
      state: MapState,
      upd: LocationUpdate
  ): URIO[MapEnv, (MapState, Chunk[Identified[MapResponse]])] =
    val (nextState, changes) = processCharacterLocationChanges(state, upd)
    val locationsUpdate =
      if (state.locationsOnline != nextState.locationsOnline)
        broadcast(
          MapResponse.CharacterLocations(
            nextState.locations.view.filter(_._2.locationInfo.isDefined).mapValues(_.locationInfo.get).toMap
          )
        )
      else Chunk.empty
    ZIO.foldLeft(changes)((nextState, locationsUpdate)):
      case ((st, responses), asm: LocationUpdateAction.AddMapSystem)
          if asm.adjacentTo.isEmpty && !st.hasSystem(asm.system) && !isKnownSpace(st, asm.system) =>
        addSystemFromLocation(mapId, st, responses, asm).orDie
      case ((st, responses), asm: LocationUpdateAction.AddMapSystem)
          if asm.adjacentTo.exists(fromSystemId =>
            !st
              .hasConnection(fromSystemId, asm.system) && isPotentialWormholeJump(st, fromSystemId, asm.system)
          ) && !st.hasSystem(asm.system) =>
        addSystemFromLocation(mapId, st, responses, asm).orDie
      case ((st, responses), amc: LocationUpdateAction.AddMapConnection)
          if !st
            .hasConnection(amc.fromSystem, amc.toSystem) && isPotentialWormholeJump(st, amc.fromSystem, amc.toSystem) =>
        addMapConnectionFromLocation(mapId, st, responses, amc).orDie
      case ((st, responses), aj: LocationUpdateAction.AddJump) if st.hasConnection(aj.fromSystem, aj.toSystem) =>
        addMapConnectionJump(mapId, st, responses, st.getConnection(aj.fromSystem, aj.toSystem).id, aj).orDie
      case (prev, msg) =>
        ZIO.succeed(prev)

  private def addSystemFromLocation(
      mapId: MapId,
      state: MapState,
      responses: Chunk[Identified[MapResponse]],
      action: LocationUpdateAction.AddMapSystem
  ) = addSystem(
    mapId,
    state,
    action.characterId,
    action.updatedAt,
    MapRequest.AddSystem(
      systemId = action.system,
      name = None,
      isPinned = false,
      displayData = generateDisplayData(state, action.system, action.adjacentTo),
      stance = None
    )
  ).map((st, r) => st -> (responses ++ r))

  private def generateDisplayData(state: MapState, systemId: SystemId, adjacentTo: Option[SystemId]) =
    state.displayType match
      case model.MapDisplayType.Manual =>
        val prevDisplay = adjacentTo.flatMap(prevId => state.systems.get(prevId).flatMap(_.display))
        prevDisplay match
          case None =>
            model.SystemDisplayData.Manual(0, 0) // origin position
          case Some(model.SystemDisplayData.Manual(x, y)) =>
            // this must necessarily replicate the frontend code
            // TODO: need to take collisions etc. into account
            val newX = x + MagicConstant.SystemBoxSizeX + (MagicConstant.SystemBoxSizeX / 3)
            model.SystemDisplayData.Manual(newX - (newX % MagicConstant.GridSnapPx), y)

  private def addMapConnectionFromLocation(
      mapId: MapId,
      state: MapState,
      responses: Chunk[Identified[MapResponse]],
      action: LocationUpdateAction.AddMapConnection
  ) =
    insertSystemConnection(
      mapId,
      state,
      action.characterId,
      action.info.updatedAt,
      MapRequest.AddSystemConnection(action.fromSystem, action.toSystem)
    ).map((st, r) => st -> (responses ++ r))

  private def addMapConnectionJump(
      mapId: MapId,
      state: MapState,
      responses: Chunk[Identified[MapResponse]],
      connectionId: ConnectionId,
      action: LocationUpdateAction.AddJump
  ) =
    insertSystemConnectionJump(
      mapId,
      state,
      action.characterId,
      action.info.updatedAt,
      connectionId,
      action.info.shipTypeId
    ).map((st, r) => st -> (responses ++ r))

  private def processCharacterLocationChanges(
      state: MapState,
      upd: LocationUpdate
  ): (MapState, Chunk[LocationUpdateAction]) =
    val (nextState, actions) = upd.state.iterator.foldLeft(state -> Chunk.empty[LocationUpdateAction]):
      case ((s, actions), (charId, charState)) =>
        val (online, nextLocation) = charState match
          case is: CharacterLocationState.InSystem => true  -> Some(is)
          case _                                   => false -> None

        var nextActions = Chunk.empty[LocationUpdateAction]
        val nextLocs = s.locations.updatedWith(charId):
          case None => None // we need the role to be able to proceed so wait until next update
          case Some(prev) =>
            val prevLocation = prev.locationInfo
            nextActions = (prevLocation, nextLocation) match
              case (None, Some(n)) if online =>
                // character has no previous location and is online - add the system to map (if not already there)
                Chunk(LocationUpdateAction.AddMapSystem(n.system, charId, prev.role, None, n.updatedAt))
              case (Some(p), Some(n)) if online && p.system != n.system && prev.role != model.MapRole.Viewer =>
                // character has changed location and is online - add the system and a connection to the map
                Chunk(
                  LocationUpdateAction.AddMapSystem(p.system, charId, prev.role, Some(n.system), n.updatedAt),
                  LocationUpdateAction.AddMapSystem(n.system, charId, prev.role, Some(p.system), n.updatedAt),
                  LocationUpdateAction.AddMapConnection(p.system, n.system, charId, prev.role, n),
                  LocationUpdateAction.AddJump(charId, p.system, n.system, n)
                )
              case (Some(p), Some(n)) if online && p.system != n.system && prev.role == model.MapRole.Viewer =>
                // character has jumped but cannot edit the map
                Chunk(
                  LocationUpdateAction.AddJump(charId, p.system, n.system, n)
                )
              case _ => Chunk.empty

            Some(prev.copy(online = online, locationInfo = nextLocation))
        (s.copy(locations = nextLocs), actions ++ nextActions)
    recomputeOnlineMap(pruneLocations(nextState, upd)) -> actions

  private def pruneLocations(state: MapState, upd: LocationUpdate): MapState =
    val nextLocations = (state.locations.keySet -- upd.state.keySet).foldLeft(state.locations):
      case (s, charId) =>
        s.updatedWith(charId):
          case None       => None
          case Some(prev) => Some(prev.copy(locationInfo = None))
    state.copy(locations = nextLocations)

  private def recomputeOnlineMap(state: MapState): MapState =
    state.copy(locationsOnline =
      state.locations
        .filter((_, mls) => mls.locationInfo.isDefined && mls.online)
        .transform: (_, mls) =>
          val locationInfo = mls.locationInfo.get
          locationInfo.system -> locationInfo.shipTypeId
    )

  private def isPotentialWormholeJump(state: MapState, fromSystemId: SystemId, toSystemId: SystemId) =
    val differentSystem = fromSystemId != toSystemId
    val noGate          = !state.hasGateBetween(fromSystemId, toSystemId)
    val notJita         = fromSystemId != MagicConstant.Jita && toSystemId != MagicConstant.Jita
    val isTarget        = state.refSystem(fromSystemId).zip(state.refSystem(toSystemId)).exists(isTargetForJumps)
    differentSystem && noGate && notJita && isTarget

  private def isKnownSpace(state: MapState, systemId: SystemId) =
    state.refSystem(systemId).exists(_.whClass.spaceType == SpaceType.Known)

  private[map] def isTargetForJumps(fromSystem: MapSolarSystem, toSystem: MapSolarSystem) =
    (fromSystem.whClass.spaceType, toSystem.whClass.spaceType) match
      // wormholes are always jumps
      case (SpaceType.Wormhole, _) => true
      case (_, SpaceType.Wormhole) => true
      // internal, abyssal space is never a target for jumps
      case (_, SpaceType.Abyssal)  => false
      case (SpaceType.Abyssal, _)  => false
      case (_, SpaceType.Internal) => false
      case (SpaceType.Internal, _) => false
      // others are a potential target (stargate check for known space already happened)
      case _ => true

  private def updateCharacterRoles(mapId: MapId, state: MapState, upd: Map[CharacterId, model.MapRole]) =
    ZIO.succeed(state.copy(locations = upd.iterator.foldLeft(state.locations) { case (s, (charId, role)) =>
      s.updatedWith(charId) {
        case None       => Some(MapLocationState(charId, role, online = false, locationInfo = None))
        case Some(prev) => Some(prev.copy(role = role))
      }
    // TODO: should probably remove the characters that do not have roles
    }) -> Chunk.empty)

  private def removeOldConnections(mapId: MapId, state: MapState) =
    for
      config <- ZIO.service[MapConfig]
      // perform hard deletes (for which clients don't need to be updated)
      hardDeleteSignatures    <- MapQueries.getHardDeleteSignatures(mapId, config.hardDeletionInterval)
      hardDeleteConnectionIds <- MapQueries.getHardDeleteConnectionIds(mapId, config.hardDeletionInterval)
      rSigCount <- query.map
        .hardDeleteMapWormholeSignatures(mapId, Chunk.fromIterable(hardDeleteSignatures))
        .when(hardDeleteSignatures.nonEmpty)
        .someOrElse(0)
      rCJumpCount <- query.map
        .hardDeleteMapWormholeConnectionJumps(mapId, Chunk.fromIterable(hardDeleteConnectionIds))
        .when(hardDeleteConnectionIds.nonEmpty)
        .someOrElse(0)
      rConnCount <- query.map
        .hardDeleteMapWormholeConnections(mapId, Chunk.fromIterable(hardDeleteConnectionIds))
        .when(hardDeleteConnectionIds.nonEmpty)
        .someOrElse(0)
      _ <- ZIO.logTrace(s"Hard deleted $rSigCount sigs, $rCJumpCount jumps and $rConnCount connections")
      // get expired connections
      expiredConnections <- MapQueries.getWormholeConnectionsWithSigsExpiredOrEol(
        mapId,
        config.staleConnectionRemovalInterval,
        config.eolConnectionRemovalInterval
      )
      // delete the connections if any were found
      expiredConnectionIds = Chunk.fromIterable(expiredConnections.map(_.connection.id))
      systemIdsToRefresh   = expiredConnections.map(_.systemIds).toSet.flatten
      eConnCount <- query.map
        .deleteMapWormholeConnections(mapId, expiredConnectionIds, CharacterId.System)
        .when(expiredConnectionIds.nonEmpty)
        .someOrElse(0)
      // delete any signatures that map to those connection ids
      eSigCount <- query.map
        .deleteSignaturesWithConnectionIds(mapId, expiredConnectionIds, CharacterId.System)
        .when(expiredConnectionIds.nonEmpty)
        .someOrElse(0)
      _ <- ZIO.logTrace(s"Removed expired $eConnCount connections, $eSigCount signatures")
      deletedConnections <- query.map
        .getWormholeConnections(mapId, expiredConnectionIds, isDeleted = true)
        .when(expiredConnectionIds.nonEmpty)
        .someOrElse(Nil)
      // reload state
      res <- combineMany(
        state,
        Chunk(removeConnections(deletedConnections)) ++
          systemIdsToRefresh.map(sId => reloadSystemSnapshot(mapId, sId))
      ).when(expiredConnections.nonEmpty).someOrElse(state -> Chunk.empty)
    yield res

  private inline def loadSingleSystem(mapId: MapId, systemId: SystemId) =
    MapQueries
      .getMapSystemAll(mapId, Some(systemId))
      .filterOrDieMessage(_.size == 1)(
        s"BUG: expected exactly 1 (map) system to be returned for (map=$mapId, system=$systemId)"
      )
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
    wormholeConnectionId = newSig.wormholeConnectionId.asOption,
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
      wormholeConnectionId = UnknownOrUnset(prev.wormholeConnectionId).updateWith(newSig.wormholeConnectionId).asOption,
      updatedAt = now,
      updatedByCharacterId = sessionId.characterId
    )

private inline def reply(sessionId: MapSessionId, value: MapResponse): Chunk[Identified[MapResponse]] =
  Chunk.single(Identified(Some(sessionId), value))
private inline def replyMany(sessionId: MapSessionId, values: MapResponse*): Chunk[Identified[MapResponse]] =
  Chunk(values.map(v => Identified(Some(sessionId), v))*)
private inline def broadcast(value: MapResponse): Chunk[Identified[MapResponse]] =
  Chunk.single(Identified(None, value))
private inline def broadcastMany(values: MapResponse*): Chunk[Identified[MapResponse]] =
  Chunk(values.map(v => Identified(None, v))*)

private inline def withState[A](state: MapState)(f: MapState => A): A = f(state)

private inline def updateConnectionById(
    arr: Chunk[MapWormholeConnection],
    whc: MapWormholeConnection
): Chunk[MapWormholeConnection] =
  arr.indexWhere(_.id == whc.id) match
    case -1  => arr.appended(whc)
    case idx => arr.updated(idx, whc)

private inline def removeConnectionById(
    arr: Chunk[MapWormholeConnection],
    whc: MapWormholeConnection
) =
  arr.indexWhere(_.id == whc.id) match
    case -1  => arr
    case idx => arr.patch(idx, Nil, 1)

private inline def removeSignatureById(arr: Chunk[MapSystemSignature], idOpt: Option[SigId]) =
  idOpt
    .map(sigId =>
      arr.indexWhere(_.signatureId == sigId) match
        case -1  => arr
        case idx => arr.patch(idx, Nil, 1)
    )
    .getOrElse(arr)

private def foldLeft[A](
    initial: MapState,
    xs: Chunk[A],
    f: (A, MapState) => ZIO[MapEnv, Throwable, (MapState, Chunk[Identified[MapResponse]])]
) =
  ZIO.foldLeft(xs)((initial, Chunk.empty[Identified[MapResponse]])):
    case ((prev, responses), a) => f(a, prev).map((s, r) => (s, responses ++ r))

private def combineMany(
    initial: MapState,
    fs: Chunk[MapState => ZIO[MapEnv, Throwable, (MapState, Chunk[Identified[MapResponse]])]]
) =
  ZIO.foldLeft(fs)((initial, Chunk.empty[Identified[MapResponse]])):
    case ((prev, responses), f) => f(prev).map((s, r) => (s, responses ++ r))

private def loadMapRef() =
  ReferenceQueries.getAllSolarSystemsWithGates.map(allSolar =>
    MapRef(
      solarSystems = allSolar
        .filter(_.sys.whClassId.nonEmpty)
        .map(ss =>
          ss.sys.id -> MapSolarSystem(
            systemId = ss.sys.id,
            name = ss.sys.name,
            whClass = WormholeClasses.ById(ss.sys.whClassId.get),
            regionId = ss.sys.regionId,
            constellationId = ss.sys.constellationId,
            gates = ss.gates.map(sg => sg.outSystemId -> sg.inGateId).toMap
          )
        )
        .toMap
    )
  )

// convenience methods
extension (s: MapState)
  def systemSnapshot(systemId: SystemId): MapResponse.SystemSnapshot =
    MapResponse.SystemSnapshot(
      systemId = systemId,
      sys = s.systems(systemId),
      connections = s.connectionsForSystem(systemId),
      connectionRanks = s.connectionRanksForSystem(systemId)
    )
