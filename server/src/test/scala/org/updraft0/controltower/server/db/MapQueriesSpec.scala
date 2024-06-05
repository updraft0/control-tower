package org.updraft0.controltower.server.db

import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.server
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.query

import zio.*
import zio.test.*

import java.time.Instant

object MapQueriesSpec extends ZIOSpecDefault:
  private val DefaultMap = model.MapModel(42L, "test", model.MapDisplayType.Manual, Instant.EPOCH, 1L, None, None)

  override def spec = suite("MapQueries::map_wormhole_connection")(
    test("can compute ranks of incoming wormholes"):
        /*
          format: off

             ______________
            |             \
            S1 --> S2  ---> S4 --> S3
           /      /
          |  S3 -/--------> S6
          |      \
          \------|--------> S5

          format: on
         */

        val systems = List(
          system(100L, Some("System 1")),
          system(200L, Some("System 2")),
          system(300L, Some("System 3")),
          system(400L, Some("System 4")),
          system(500L, Some("System 5")),
          system(600L, Some("System 6"))
        )

        val connections = List(
          connection(100L, 200L),
          connection(200L, 400L),
          connection(400L, 100L),
          connection(300L, 200L),
          connection(300L, 500L),
          connection(100L, 500L),
          connection(300L, 600L),
          connection(400L, 300L)
        )

        val connectionRanks = Map(
          (100L, 200L) -> MapWormholeConnectionRank(0L, 1, 2, 1, 2),
          (200L, 400L) -> MapWormholeConnectionRank(0L, 1, 1, 1, 1),
          (400L, 100L) -> MapWormholeConnectionRank(0L, 1, 2, 1, 1),
          (300L, 200L) -> MapWormholeConnectionRank(0L, 1, 3, 2, 2),
          (300L, 500L) -> MapWormholeConnectionRank(0L, 2, 3, 1, 2),
          (100L, 500L) -> MapWormholeConnectionRank(0L, 2, 2, 2, 2),
          (300L, 600L) -> MapWormholeConnectionRank(0L, 3, 3, 1, 1),
          (400L, 300L) -> MapWormholeConnectionRank(0L, 2, 2, 1, 1)
        )

        query.transaction(
          for
            // 0. insert reference data, systems + connections
            _             <- query.map.upsertMap(DefaultMap)
            _             <- ZIO.foreachDiscard(systems)(query.map.upsertMapSystem)
            connsInserted <- ZIO.foreach(connections)(query.map.insertMapWormholeConnection)
            // 1. compute the ranks of all connections
            allRanks <- MapQueries.getWormholeConnectionRanksAll(DefaultMap.id)
            // 2. get the expected values (fixup database generated ids)
            connsMap          = connectionsById(connsInserted)
            connRanksExpected = connectionRanks.map((k, v) => connsMap(k).id -> v.copy(connectionId = connsMap(k).id))
            allRanksMap       = ranksById(allRanks)
            // 3. test getting ranks for particular systems
            ranks100 <- MapQueries.getWormholeConnectionRanksForSystem(DefaultMap.id, 100L).map(ranksById)
            ranks200 <- MapQueries.getWormholeConnectionRanksForSystem(DefaultMap.id, 200L).map(ranksById)
            ranks300 <- MapQueries.getWormholeConnectionRanksForSystem(DefaultMap.id, 300L).map(ranksById)
            ranks400 <- MapQueries.getWormholeConnectionRanksForSystem(DefaultMap.id, 400L).map(ranksById)
            ranks500 <- MapQueries.getWormholeConnectionRanksForSystem(DefaultMap.id, 500L).map(ranksById)
            ranks600 <- MapQueries.getWormholeConnectionRanksForSystem(DefaultMap.id, 600L).map(ranksById)
          yield assertTrue(
            connsInserted.size == connections.size && allRanksMap == connRanksExpected &&
              containsAll(ranks100, connRanksExpected) && ranks100.size == 3 &&
              containsAll(ranks200, connRanksExpected) && ranks200.size == 3 &&
              containsAll(ranks300, connRanksExpected) && ranks300.size == 4 &&
              containsAll(ranks400, connRanksExpected) && ranks400.size == 3 &&
              containsAll(ranks500, connRanksExpected) && ranks500.size == 2 &&
              containsAll(ranks600, connRanksExpected) && ranks600.size == 1
          )
        )
  ).provideLayer(TempDb.empty)

  private def system(id: model.SystemId, name: Option[String]): model.MapSystem =
    model.MapSystem(
      mapId = DefaultMap.id,
      systemId = id,
      name = name,
      isPinned = false,
      chainNamingStrategy = model.ChainNamingStrategy.Manual,
      description = None,
      stance = model.IntelStance.Unknown,
      updatedByCharacterId = CharacterId(1L),
      updatedAt = Instant.EPOCH
    )

  private def connection(fromSystem: model.SystemId, toSystem: model.SystemId): model.MapWormholeConnection =
    model.MapWormholeConnection(
      id = 0L,
      mapId = DefaultMap.id,
      fromSystemId = fromSystem,
      toSystemId = toSystem,
      isDeleted = false,
      createdAt = Instant.EPOCH,
      createdByCharacterId = CharacterId(1L),
      updatedAt = Instant.EPOCH,
      updatedByCharacterId = CharacterId(1L)
    )

  private def connectionsById(got: List[model.MapWormholeConnection]): Map[(Long, Long), model.MapWormholeConnection] =
    got.map(whc => (whc.fromSystemId, whc.toSystemId) -> whc).toMap

  private def ranksById(got: List[MapWormholeConnectionRank]): Map[Long, MapWormholeConnectionRank] =
    got.map(r => r.connectionId -> r).toMap

  private def containsAll[K, V](test: Map[K, V], all: Map[K, V]): Boolean =
    test.forall((k, v) => all.contains(k) && all(k) == v)
