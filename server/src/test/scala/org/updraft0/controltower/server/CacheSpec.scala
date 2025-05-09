package org.updraft0.controltower.server

import zio.*
import zio.concurrent.*
import zio.query.*
import zio.test.*

object DataCacheSpec extends ZIOSpecDefault:

  case class GetRequest(c: Char) extends Request[Nothing, Option[Int]]

  def stubDataSource(record: ConcurrentMap[Char, Int], exclude: Set[Char]): DataSource.Batched[Any, GetRequest] =
    new DataSource.Batched[Any, GetRequest]:
      override val identifier: String = "StubDataSource"
      override def run(requests: Chunk[GetRequest])(implicit trace: Trace): ZIO[Any, Nothing, CompletedRequestMap] =
        ZIO.foreach(requests)(r =>
          record.compute(
            r.c,
            (_, prev) =>
              prev.map(i => if exclude.contains(r.c) then i else i + 1).orElse(Option.when(!exclude.contains(r.c))(1))
          )
        ) *> ZIO.succeed(
          CompletedRequestMap.fromIterable(
            requests.map(r => (r, Exit.succeed(Option.when(!exclude.contains(r.c))(r.c.intValue))))
          )
        )

  override def spec =
    suite("DataCache")(
      test("with basic operations works"):
        for
          cache                         <- DataCache[String, Int]()
          _                             <- cache.put("a", 1)
          _                             <- cache.putSome("b", None)
          _                             <- cache.put("a", 2)
          a                             <- cache.get("a")
          b                             <- cache.get("b")
          _                             <- cache.put("c", 3)
          (keysInCache, keysNotInCache) <- cache.keyIntersection(Chunk("a", "b", "c", "d"))
          all                           <- cache.bulkLookup(Chunk("a", "b", "c", "d"))
        yield assertTrue(
          a.contains(2),
          b.isEmpty,
          keysInCache == Chunk("a", "c"),
          keysNotInCache == Chunk("b", "d"),
          all == Chunk("a" -> Some(2), "b" -> None, "c" -> Some(3), "d" -> None)
        )
      ,
      test("with data source with fallback"):
        for
          // 0. setup
          lookedUp  <- ConcurrentMap.make[Char, Int]()
          populated <- ConcurrentMap.make[Char, Int]()
          stubSource = stubDataSource(lookedUp, Set('d', 'e'))
          cached <- DataCache.dataSource[Any, Nothing, GetRequest, Int](
            stubSource,
            k => populated.compute(k.c, (_, prev) => prev.map(_ + 1).orElse(Some(1))).as(Some(k.c.toInt))
          )
          // 1. query for [a, b, c]
          query1          <- ZQuery.fromRequests(Chunk('a', 'b', 'c').map(GetRequest.apply))(cached).run
          afterQ1LookedUp <- lookedUp.toChunk
          afterQ1Loaded   <- populated.toChunk
          // 2. query for [b, d, e, g]
          query2          <- ZQuery.fromRequests(Chunk('b', 'd', 'e', 'g').map(GetRequest.apply))(cached).run
          afterQ2LookedUp <- lookedUp.toChunk
          afterQ2Loaded   <- populated.toChunk
          // 3. query for [a, d, e, f]
          query3          <- ZQuery.fromRequests(Chunk('a', 'd', 'e', 'f').map(GetRequest.apply))(cached).run
          afterQ3LookedUp <- lookedUp.toChunk
          afterQ3Loaded   <- populated.toChunk
        yield assertTrue(
          afterQ1LookedUp == Chunk('a'.toInt -> 1, 'b'.toInt -> 1, 'c'.toInt -> 1),
          afterQ1Loaded.isEmpty,
          afterQ2LookedUp == Chunk('a'.toInt -> 1, 'b'.toInt -> 1, 'c'.toInt -> 1, 'g'.toInt -> 1),
          afterQ2Loaded == Chunk('d'.toInt -> 1, 'e'.toInt -> 1),
          afterQ3LookedUp == Chunk('a'.toInt -> 1, 'b'.toInt -> 1, 'c'.toInt -> 1, 'f'.toInt -> 1, 'g'.toInt -> 1),
          afterQ3Loaded == Chunk('d'.toInt -> 1, 'e'.toInt -> 1)
        )
    )
