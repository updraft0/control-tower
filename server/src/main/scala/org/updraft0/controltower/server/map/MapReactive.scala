package org.updraft0.controltower.server.map

import org.updraft0.controltower.db.model.{ChainNamingStrategy, SystemDisplayData, displayType}
import org.updraft0.controltower.db.{model, query}
import org.updraft0.controltower.server.db.{MapQueries, MapSystemWithAll}
import org.updraft0.minireactive.*
import zio.*

import java.time.Instant

type MapId    = Long
type MapEnv   = javax.sql.DataSource
type SystemId = Long

private type MapState = Map[SystemId, MapSystemWithAll]

case class Identified[T](characterId: Option[Long], value: T)

enum MapRequest:
  case MapSnapshot
  case AddSystem(
      systemId: SystemId,
      name: Option[String],
      isPinned: Boolean,
      displayData: model.SystemDisplayData,
      stance: model.IntelStance
  )
  case UpdateSystemDisplay(systemId: SystemId, displayData: model.SystemDisplayData)
  case RemoveSystem(systemId: SystemId)

enum MapResponse:
  case MapSnapshot(all: Map[SystemId, MapSystemWithAll])
  case SystemSnapshot(systemId: SystemId, sys: MapSystemWithAll)
  case SystemDisplayUpdate(systemId: SystemId, displayData: model.SystemDisplayData)
  case SystemRemoved(systemId: SystemId)

/** Mini-reactive/lightweight actor that has a state of the whole map in memory and makes corresponding db changes
  */
class MapEntity extends ReactiveEntity[MapEnv, MapId, MapState, Identified[MapRequest], Identified[MapResponse]]:
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
    query
      .transaction(
        in match
          case Identified(id, MapRequest.MapSnapshot) =>
            ZIO.succeed(state -> Chunk.single(Identified(id, MapResponse.MapSnapshot(state))))
          case Identified(Some(cid), add: MapRequest.AddSystem) =>
            addSystem(mapId, state, cid, add)
          case Identified(Some(_), usd: MapRequest.UpdateSystemDisplay) =>
            whenSystemExists(usd.systemId, state)(updateSystemDisplay(mapId, state, usd))
          case Identified(Some(characterId), rs: MapRequest.RemoveSystem) =>
            whenSystemExists(rs.systemId, state)(removeSystemFromDisplay(mapId, state, characterId, rs))
          // fall-through case
          case Identified(None, _) => ZIO.logDebug("non-identified request not processed").as(state -> Chunk.empty)
      )
      .orDie

  private def allToState(all: List[MapSystemWithAll]): MapState =
    all.map(ms => ms.sys.systemId -> ms).toMap

  private def addSystem(mapId: MapId, state: MapState, characterId: Long, add: MapRequest.AddSystem) =
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
          updatedByCharacterId = characterId,
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
        else MapResponse.SystemDisplayUpdate(sys.sys.systemId, sys.display.get)
    yield state.updated(sys.sys.systemId, sys) -> Chunk.single(Identified(None, update))

  private def removeSystemFromDisplay(mapId: MapId, state: MapState, characterId: Long, rs: MapRequest.RemoveSystem) =
    for
      _   <- query.map.deleteMapSystemDisplay(mapId, rs.systemId)
      sys <- loadSingleSystem(mapId, rs.systemId)
    yield state.updated(sys.sys.systemId, sys) -> Chunk.single(
      Identified(None, MapResponse.SystemRemoved(sys.sys.systemId))
    )

  private inline def loadSingleSystem(mapId: MapId, systemId: SystemId) =
    MapQueries
      .getMapSystemAll(mapId, Some(systemId))
      .filterOrDieMessage(_.size == 1)(s"BUG: expected exactly 1 system to be returned")
      .map(_.head)

  private inline def whenSystemExists(systemId: SystemId, state: MapState)(
      f: RIO[MapEnv, (MapState, Chunk[Identified[MapResponse]])]
  ) =
    ZIO.when(state.contains(systemId))(f).map(_.getOrElse(state -> Chunk.empty))

object MapReactive:
  private val MailboxSize = 128 // TODO - configurable?

  type Service = MiniReactive[MapId, Identified[MapRequest], Identified[MapResponse]]

  def layer: ZLayer[MapEnv, Nothing, Service] =
    MiniReactive.layer(MapEntity(), MiniReactiveConfig(MailboxSize))
