package org.updraft0.controltower.server.map

import org.updraft0.controltower.db.model.{ChainNamingStrategy, SystemDisplayData, displayType}
import org.updraft0.controltower.db.{model, query}
import org.updraft0.controltower.server.db.{MapQueries, MapSystemWithAll}
import org.updraft0.minireactive.*
import zio.*
import java.util.UUID
import org.updraft0.controltower.server.Log

import java.time.Instant

type MapId    = Long
type MapEnv   = javax.sql.DataSource
type SystemId = Long

private type MapState = Map[SystemId, MapSystemWithAll]

case class MapSessionId(characterId: Long, sessionId: UUID)
case class Identified[T](sessionId: Option[MapSessionId], value: T)

enum MapRequest:
  case MapSnapshot
  case AddSystem(
      systemId: SystemId,
      name: Option[String],
      isPinned: Boolean,
      displayData: model.SystemDisplayData,
      stance: model.IntelStance
  )
  case UpdateSystemAttribute(systemId: SystemId, pinned: Option[Boolean], intelStance: Option[model.IntelStance])
  case UpdateSystemDisplay(systemId: SystemId, displayData: model.SystemDisplayData)
  case RenameSystem(systemId: SystemId, name: Option[String])
  case RemoveSystem(systemId: SystemId)

enum MapResponse:
  case MapSnapshot(all: Map[SystemId, MapSystemWithAll])
  case SystemSnapshot(systemId: SystemId, sys: MapSystemWithAll)
  case SystemDisplayUpdate(systemId: SystemId, name: Option[String], displayData: model.SystemDisplayData)
  case SystemRemoved(systemId: SystemId)

/** Mini-reactive/lightweight actor that has a state of the whole map in memory and makes corresponding db changes
  */
object MapEntity extends ReactiveEntity[MapEnv, MapId, MapState, Identified[MapRequest], Identified[MapResponse]]:
  override def tag = "Map"

  override def hydrate(key: MapId): URIO[MapEnv, MapState] =
    query
      .transaction(MapQueries.getMapSystemAll(key))
      .orDie
      .map(allToState)

  override def handle(
      mapId: MapId,
      state: MapState,
      in: Identified[MapRequest]
  ): URIO[MapEnv, (MapState, Chunk[Identified[MapResponse]])] =
    (query
      .transaction(
        in match
          case Identified(id, MapRequest.MapSnapshot) =>
            ZIO.succeed(state -> Chunk.single(Identified(id, MapResponse.MapSnapshot(state))))
          case Identified(Some(sid), add: MapRequest.AddSystem) if state.get(add.systemId).forall(_.display.isEmpty) =>
            identified(sid, "add", addSystem(mapId, state, sid, add))
          case Identified(_, add: MapRequest.AddSystem) =>
            ZIO.logDebug(s"no-op adding existing system $add").as(state -> Chunk.empty)
          case Identified(Some(sid), usd: MapRequest.UpdateSystemDisplay) =>
            whenSystemExists(usd.systemId, state)(
              identified(sid, "updateDisplay", updateSystemDisplay(mapId, state, usd))
            )
          case Identified(Some(sid), usa: MapRequest.UpdateSystemAttribute) =>
            whenSystemExists(usa.systemId, state)(
              identified(sid, "updateAttribute", updateSystemAttribute(mapId, state, sid, usa))
            )
          case Identified(Some(sid), rs: MapRequest.RemoveSystem) =>
            whenSystemExists(rs.systemId, state)(
              identified(sid, "removeFromDisplay", removeSystemFromDisplay(mapId, state, sid, rs))
            )
          case Identified(Some(sid), rs: MapRequest.RenameSystem) =>
            whenSystemExists(rs.systemId, state)(
              identified(sid, "renameSystem", renameSystem(mapId, state, sid, rs))
            )
          // fall-through case
          case Identified(None, _) => ZIO.logWarning("non-identified request not processed").as(state -> Chunk.empty)
      ) @@ Log.MapId(mapId)).orDie

  private inline def identified[R, E, A](sid: MapSessionId, op: String, f: ZIO[R, E, A]): ZIO[R, E, A] =
    f @@ Log.SessionId(sid.sessionId) @@ Log.CharacterId(sid.characterId) @@ Log.MapOperation(op)

  private def allToState(all: List[MapSystemWithAll]): MapState =
    all.map(ms => ms.sys.systemId -> ms).toMap

  private def addSystem(mapId: MapId, state: MapState, sessionId: MapSessionId, add: MapRequest.AddSystem) =
    for
      _ <- query.map.upsertMapSystem(
        model.MapSystem(
          mapId = mapId,
          systemId = add.systemId,
          name = add.name,
          isPinned = add.isPinned,
          chainNamingStrategy = ChainNamingStrategy.Manual,
          description = None,
          stance = add.stance,
          updatedByCharacterId = sessionId.characterId,
          updatedAt = Instant.EPOCH
        )
      )
      _ <- query.map.upsertMapSystemDisplay(
        model.MapSystemDisplay(mapId, add.systemId, add.displayData.displayType, add.displayData)
      )
      sys <- loadSingleSystem(mapId, add.systemId)
    yield state.updated(sys.sys.systemId, sys) -> Chunk.single(
      Identified(None, MapResponse.SystemSnapshot(sys.sys.systemId, sys))
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
      update =
        if (state(usd.systemId).display.isEmpty) MapResponse.SystemSnapshot(sys.sys.systemId, sys)
        else MapResponse.SystemDisplayUpdate(sys.sys.systemId, sys.sys.name, sys.display.get)
    yield state.updated(sys.sys.systemId, sys) -> Chunk.single(Identified(None, update))

  private def updateSystemAttribute(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      usa: MapRequest.UpdateSystemAttribute
  ) =
    for
      _   <- query.map.updateMapAttribute(mapId, usa.systemId, usa.pinned, usa.intelStance, sessionId.characterId)
      sys <- loadSingleSystem(mapId, usa.systemId)
    yield state.updated(sys.sys.systemId, sys) -> sys.display
      .map(displayData => Chunk.single(Identified(None, MapResponse.SystemSnapshot(sys.sys.systemId, sys))))
      .getOrElse(Chunk.empty)

  private def removeSystemFromDisplay(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      rs: MapRequest.RemoveSystem
  ) =
    for
      _   <- query.map.deleteMapSystemDisplay(mapId, rs.systemId)
      sys <- loadSingleSystem(mapId, rs.systemId)
    yield state.updated(sys.sys.systemId, sys) -> Chunk.single(
      Identified(None, MapResponse.SystemRemoved(sys.sys.systemId))
    )

  private def renameSystem(mapId: MapId, state: MapState, sessionId: MapSessionId, rs: MapRequest.RenameSystem) =
    for
      _   <- query.map.updateMapSystemName(mapId, rs.systemId, rs.name, sessionId.characterId)
      sys <- loadSingleSystem(mapId, rs.systemId)
    yield state.updated(sys.sys.systemId, sys) -> sys.display
      .map(displayData =>
        Chunk.single(Identified(None, MapResponse.SystemDisplayUpdate(sys.sys.systemId, sys.sys.name, displayData)))
      )
      .getOrElse(Chunk.empty)

  private inline def loadSingleSystem(mapId: MapId, systemId: SystemId) =
    MapQueries
      .getMapSystemAll(mapId, Some(systemId))
      .filterOrDieMessage(_.size == 1)(s"BUG: expected exactly 1 system to be returned")
      .map(_.head)

  private inline def whenSystemExists(systemId: SystemId, state: MapState)(
      f: RIO[MapEnv, (MapState, Chunk[Identified[MapResponse]])]
  ) =
    ZIO.when(state.contains(systemId))(f).map(_.getOrElse(state -> Chunk.empty)) @@ Log.SystemId(systemId)

object MapReactive:
  private val MailboxSize = 128 // TODO - configurable?

  type Service = MiniReactive[MapId, Identified[MapRequest], Identified[MapResponse]]

  def layer: ZLayer[MapEnv, Nothing, Service] =
    MiniReactive.layer(MapEntity, MiniReactiveConfig(MailboxSize, 10.seconds))
