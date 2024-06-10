package org.updraft0.controltower.server.map

import org.updraft0.controltower.constant.{CharacterId, MapId}
import org.updraft0.controltower.db.model.MapRole
import org.updraft0.controltower.server.Log
import org.updraft0.controltower.server.auth.MapPolicy
import zio.*

trait MapPermissionTracker:
  def reloadPermissions(mapId: MapId): UIO[Unit]
  def subscribe(mapId: MapId): ZIO[Scope, Nothing, Dequeue[MapSessionMessage]]
  def subscribeSession(mapId: MapId, characterId: CharacterId): ZIO[Scope, Nothing, Dequeue[MapSessionMessage]]

object MapPermissionTracker:
  type Env = javax.sql.DataSource

  // Interval of permissions updates for maps
  private val PermissionReloadInterval = 5.minutes

  // Capacity of internal (dropping) hub for session messages (across all maps/characters)
  private val HubCapacity = 512

  private val SubscriberQueueCapacity = 64

  private case class ByMapState(allCharacters: Map[CharacterId, MapRole])
  private type State = Map[MapId, ByMapState]

  private enum InternalMessage derives CanEqual:
    case AddMap(mapId: MapId)
    case RemoveMap(mapId: MapId)
    case RefreshMap(mapId: MapId, ignoreUpToDate: Boolean = false)
    case RefreshAll

  def layer: ZLayer[Env, Nothing, MapPermissionTracker] = ZLayer.scoped(apply())

  def apply() =
    for
      state  <- Ref.make[State](Map.empty)
      inHub  <- Hub.bounded[InternalMessage](HubCapacity)
      outHub <- Hub.dropping[MapSessionMessage](HubCapacity)
      // note: cannot subscribe to maps ourselves because that would lead to circular deps
      // reload permissions every interval
      _ <- inHub
        .publish(InternalMessage.RefreshAll)
        .schedule(Schedule.once.andThen(Schedule.fixed(PermissionReloadInterval)))
        .forkScoped
      // process messages
      inQ <- inHub.subscribe
      _ <- (inQ.take.flatMap(msg => processIn(state, outHub, msg)).ignoreLogged @@ Log.BackgroundOperation(
        "mapPermissionTracker"
      )).forever.forkScoped
    yield new MapPermissionTracker:
      override def reloadPermissions(mapId: MapId): UIO[Unit] = inHub.publish(InternalMessage.RefreshMap(mapId)).unit
      override def subscribe(mapId: MapId): ZIO[Scope, Nothing, Dequeue[MapSessionMessage]] =
        for
          filterQ <- Queue.dropping[MapSessionMessage](SubscriberQueueCapacity)
          deQ     <- outHub.subscribe
          _       <- inHub.publish(InternalMessage.RefreshMap(mapId, ignoreUpToDate = true))
          _ <- (deQ.take
            .flatMap {
              case msg @ MapSessionMessage.MapCharacters(`mapId`, _) => filterQ.offer(msg)
              case _                                                 => ZIO.unit
            }
            @@ Log.BackgroundOperation("mapPermissionTracker") @@ Log.MapId(mapId)).forever.forkScoped
        yield filterQ
      override def subscribeSession(
          mapId: MapId,
          characterId: CharacterId
      ): ZIO[Scope, Nothing, Dequeue[MapSessionMessage]] =
        for
          filterQ <- Queue.dropping[MapSessionMessage](SubscriberQueueCapacity)
          deQ     <- outHub.subscribe
          _ <- deQ.take
            .flatMap {
              case MapSessionMessage.MapCharacters(`mapId`, allChars) =>
                filterQ.offer(MapSessionMessage.RoleChanged(characterId, allChars.get(characterId)))
              case _ => ZIO.unit
            }
            .forever
            .forkScoped
        yield filterQ

  private def processIn(state: Ref[State], outQ: Enqueue[MapSessionMessage], msg: InternalMessage) =
    msg match
      case InternalMessage.AddMap(mapId) =>
        state.get.flatMap(curr => refetchForMapId(mapId, state, outQ, false).unless(curr.contains(mapId)))
      case InternalMessage.RemoveMap(mapId) =>
        state.update(st => st.removed(mapId))
      case InternalMessage.RefreshMap(mapId, ignoreUpToDate) =>
        refetchForMapId(mapId, state, outQ, ignoreUpToDate)
      case InternalMessage.RefreshAll =>
        state.get.flatMap(curr => ZIO.foreach(curr.keys)(mapId => refetchForMapId(mapId, state, outQ, false)))

  private def refetchForMapId(
      mapId: MapId,
      state: Ref[State],
      outQ: Enqueue[MapSessionMessage],
      ignoreUpToDate: Boolean
  ) =
    for
      roleMap <- MapPolicy.characterIdsForMap(mapId)
      isUpToDate <- state.modify { currSt =>
        var upToDate = false
        val res = currSt.updatedWith(mapId):
          case None => Some(ByMapState(roleMap)) // should not happen but
          case Some(prevMap) =>
            upToDate = prevMap.allCharacters == roleMap
            Some(ByMapState(roleMap))
        upToDate -> res
      }
      curr <- state.get
      _ <- outQ
        .offer(MapSessionMessage.MapCharacters(mapId, curr(mapId).allCharacters))
        .unless(isUpToDate && !ignoreUpToDate)
    yield ()
