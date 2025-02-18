package org.updraft0.controltower.server.map

import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model.{ChainNamingStrategy, IntelStance, MapSystem, MapWormholeConnection}
import org.updraft0.controltower.server.db.{MapSystemWithAll, MapWormholeConnectionRank, MapWormholeConnectionWithSigs}
import zio.Chunk
import zio.test.*

import java.time.{Duration, Instant}

object MapStateSpec extends ZIOSpecDefault:

  val MapId1    = MapId(1)
  val SystemId1 = SystemId(1)
  val SystemId2 = SystemId(2)
  val SystemId3 = SystemId(3)
  val SystemId4 = SystemId(4)
  val SystemId5 = SystemId(5)
  val conn1     = ConnectionId(1)
  val conn2     = ConnectionId(2)
  val conn3     = ConnectionId(3)
  val conn4     = ConnectionId(4)

  override def spec = suite("MapState.systemsReachableVia")(
    test("single chain: A -(1)- B; B -(2) - C; D"):
      val state = mkState(
        MapId1,
        Set(SystemId1, SystemId2, SystemId3, SystemId4),
        (conn1, SystemId1, SystemId2),
        (conn2, SystemId2, SystemId3)
      )
      val left  = state.systemsReachableVia(conn2, fromSide = true)
      val right = state.systemsReachableVia(conn2, fromSide = false)

      assertTrue(
        left == Set(SystemId1, SystemId2),
        right == Set(SystemId3),
        state.isReachableBetween(SystemId1, SystemId3),
        !state.isReachableBetween(SystemId1, SystemId4)
      )
    ,
    test("single chain: A -(1)- B; B -(2)- C; B -(3)- D; "):
      val state = mkState(
        MapId1,
        Set(SystemId1, SystemId2, SystemId3, SystemId4, SystemId5),
        (conn1, SystemId1, SystemId2),
        (conn2, SystemId2, SystemId3),
        (conn3, SystemId2, SystemId4),
        (conn4, SystemId4, SystemId5)
      )

      val left  = state.systemsReachableVia(conn3, fromSide = true)
      val right = state.systemsReachableVia(conn3, fromSide = false)

      assertTrue(
        left == Set(SystemId1, SystemId2, SystemId3),
        right == Set(SystemId4, SystemId5),
        state.isReachableBetween(SystemId1, SystemId3),
        state.isReachableBetween(SystemId2, SystemId4)
      )
    ,
    test("disconnected chain: A -(1)- B; C -(2)- D"):
      val state = mkState(
        MapId1,
        Set(SystemId1, SystemId2, SystemId3, SystemId4),
        (conn1, SystemId1, SystemId2),
        (conn2, SystemId3, SystemId4)
      )

      val left  = state.systemsReachableVia(conn1, fromSide = true)
      val right = state.systemsReachableVia(conn1, fromSide = false)

      assertTrue(
        left == Set(SystemId1),
        right == Set(SystemId2),
        state.isReachableBetween(SystemId1, SystemId2),
        state.isReachableBetween(SystemId3, SystemId4),
        !state.isReachableBetween(SystemId1, SystemId4)
      )
    ,
    test("circular chain: A -(1)- B; B -(2)- C; C -(3)- A; C -(4)- D;"):
      val state = mkState(
        MapId1,
        Set(SystemId1, SystemId2, SystemId3, SystemId4),
        (conn1, SystemId1, SystemId2),
        (conn2, SystemId2, SystemId3),
        (conn3, SystemId3, SystemId1),
        (conn4, SystemId3, SystemId4)
      )

      val left  = state.systemsReachableVia(conn2, fromSide = true)
      val right = state.systemsReachableVia(conn2, fromSide = false)

      assertTrue(
        left == Set(SystemId1, SystemId2, SystemId3, SystemId4),
        left == right,
        state.isReachableBetween(SystemId1, SystemId3),
        state.isReachableBetween(SystemId4, SystemId1)
      )
  )

  def mkState(mapId: MapId, systems: Set[SystemId], connections: (ConnectionId, SystemId, SystemId)*): MapState =
    val ref = MapRef(
      systems.map(sId => sId -> MapSolarSystem(sId, s"SolarTest ${sId}", WormholeClass.C1, -1, -1, Map.empty)).toMap
    )

    val initialState = MapState(
      systems.toList.map: sId =>
        MapSystemWithAll(
          sys = MapSystem(
            mapId = mapId,
            systemId = sId,
            name = None,
            isPinned = false,
            chainNamingStrategy = ChainNamingStrategy.Manual,
            description = None,
            stance = IntelStance.Unknown,
            updatedByCharacterId = CharacterId.Invalid,
            updatedAt = Instant.EPOCH
          ),
          display = None,
          signatures = Chunk.empty,
          connections = Chunk.empty,
          intel = None,
          notes = Chunk.empty,
          structures = Chunk.empty,
          pings = Chunk.empty
        ),
      connections = Nil,
      connectionRanks = Nil,
      ref,
      Chunk.empty,
      Duration.ZERO
    )

    connections.foldLeft(initialState):
      case (st, (cId, fromId, toId)) =>
        val conn = MapWormholeConnectionWithSigs(
          connection = MapWormholeConnection(
            id = cId,
            mapId = mapId,
            fromSystemId = fromId,
            toSystemId = toId,
            isDeleted = false,
            createdAt = Instant.EPOCH,
            createdByCharacterId = CharacterId.Invalid,
            updatedAt = Instant.EPOCH,
            updatedByCharacterId = CharacterId.Invalid
          ),
          jumps = Chunk.empty,
          fromSignature = None,
          toSignature = None
        )

        st.copy(
          systems = st.systems
            .updatedWith(fromId)(
              _.map(msa =>
                msa.copy(
                  connections = msa.connections :+ conn.connection
                )
              )
            )
            .updatedWith(toId)(
              _.map(msa => msa.copy(connections = msa.connections :+ conn.connection))
            ),
          connections = st.connections + (cId -> conn),
          // dummy update on rank
          connectionRanks = st.connectionRanks + (cId -> MapWormholeConnectionRank(cId, 0, 0, 0, 0))
        )
