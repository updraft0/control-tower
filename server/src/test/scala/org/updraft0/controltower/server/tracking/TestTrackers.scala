package org.updraft0.controltower.server.tracking

import org.updraft0.controltower.constant.{CharacterId, MapId}
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.model.MapRole
import org.updraft0.controltower.server.map.MapSessionMessage.MapCharacters
import org.updraft0.controltower.server.map.{MapPermissionTracker, MapSessionMessage}
import zio.*

/** An in-memory location tracker that can be manually updated during test code
  */
trait TestLocationTracker extends LocationTracker:
  // test introspection
  def testTrackingRequest: Queue[LocationTrackingRequest]
  def testLocationUpdate: Hub[LocationUpdate]

object TestLocationTracker:

  def empty: ZLayer[Any, Nothing, TestLocationTracker] =
    ZLayer.scoped:
      for
        incoming  <- Queue.unbounded[LocationTrackingRequest]
        updateHub <- Hub.unbounded[LocationUpdate]
      yield new TestLocationTracker:
        override def testTrackingRequest: Queue[LocationTrackingRequest] = incoming
        override def testLocationUpdate: Hub[LocationUpdate]             = updateHub
        override def inbound: Enqueue[LocationTrackingRequest]           = incoming
        override def updates: URIO[Scope, Dequeue[LocationUpdate]]       = updateHub.subscribe

/** An in-memory permission tracker that can be manually updated with a map of roles per character
  */
trait TestPermissionTracker extends MapPermissionTracker:
  // test introspection
  def testPushPermissions(mapId: MapId, all: Map[CharacterId, model.MapRole]): UIO[Unit]
  def testHub: Hub[MapSessionMessage]

object TestPermissionTracker:

  def empty: ZLayer[Any, Nothing, TestPermissionTracker] =
    ZLayer.scoped:
      for
        stateRef <- Ref.make(Map.empty[MapId, Map[CharacterId, model.MapRole]])
        hub      <- Hub.unbounded[MapSessionMessage]
      yield new TestPermissionTracker:
        override def testPushPermissions(mapId: MapId, all: Map[CharacterId, MapRole]): UIO[Unit] =
          // note: currently no way to issue individual character role changes for testing
          stateRef.update(s => s.updated(mapId, all)) *> hub.publish(MapCharacters(mapId, all)).unit
        override def testHub: Hub[MapSessionMessage]                                          = hub
        override def reloadPermissions(mapId: MapId): UIO[Unit]                               = ZIO.unit
        override def subscribe(mapId: MapId): ZIO[Scope, Nothing, Dequeue[MapSessionMessage]] = hub.subscribe
        override def subscribeSession(
            mapId: MapId,
            characterId: CharacterId
        ): ZIO[Scope, Nothing, Dequeue[MapSessionMessage]] =
          ZIO.dieMessage("subscribing by session not supported in test permission tracker")
