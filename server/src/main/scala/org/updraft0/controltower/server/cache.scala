package org.updraft0.controltower.server

import zio.*
import zio.concurrent.*
import zio.query.*

/** A simple caching interface
  */
trait DataCache[K, V]:
  def get(k: K): UIO[Option[V]]
  def put(k: K, v: V): UIO[Unit]

  def keyExists(k: K): UIO[Option[K]] =
    get(k).map(vOpt => Option.when(vOpt.isDefined)(k))

  /** Returns (keys in map, keys not in map)
    */
  def keyIntersection(ks: Chunk[K]): UIO[(Chunk[K], Chunk[K])] =
    ZIO.foldLeft(ks)((Chunk.empty[K], Chunk.empty[K]))((s, k) =>
      keyExists(k).map:
        case None    => s.copy(_2 = s._2.appended(k))
        case Some(_) => s.copy(_1 = s._1.appended(k))
    )

  def bulkLookup(ks: Chunk[K]): UIO[Chunk[(K, Option[V])]] =
    ZIO.foreach(ks)(k => get(k).map(v => k -> v))

  def putSome(k: K, vOpt: Option[V]): UIO[Unit] =
    vOpt match
      case None    => ZIO.unit
      case Some(v) => put(k, v)

object DataCache:
  def apply[K, V]()(using CanEqual[K, K]): ZIO[Any, Nothing, DataCache[K, V]] =
    ConcurrentMap
      .make[K, V]()
      .map: m =>
        new DataCache:
          override def get(k: K)       = m.get(k)
          override def put(k: K, v: V) = m.put(k, v).unit

  /** Make a three-tier data source that uses a cache to store things in memory, has a backing datasource and a slower
    * non-batched populate function
    */
  def dataSource[R, E, K <: Request[E, Option[V]], V](
      source: DataSource[R, K],
      populate: K => ZIO[R, E, Option[V]],
      parallel: Int = 16
  )(using CanEqual[K, K]): UIO[DataSource.Batched[R, K]] =
    DataCache[K, V]().map: cache =>
      val populateSome: K => URIO[R, Unit] = (k: K) => populate(k).flatMap(v => cache.putSome(k, v)).ignoreLogged
      new DataSource.Batched[R, K]:
        override val identifier = s"Cached${source.identifier}"
        override def run(requests: Chunk[K])(using Trace): ZIO[R, Nothing, CompletedRequestMap] =
          for
            // 1. lookup any entries in the cache
            inCacheTup <- cache.keyIntersection(requests)
            (inCache, notInCache) = inCacheTup
            _ <- ZIO.logInfo(s"read cached: $inCache, not: $notInCache")
            // 2. use the real data source to lookup entries that we couldn't fetch
            lookedUp <- ZIO
              .when(notInCache.nonEmpty)(source.runAll(Chunk(notInCache)))
              .someOrElse(CompletedRequestMap.empty)
            // 3. save the new entries in the cache
            _ <- ZIO.foreachParDiscard(notInCache): k =>
              lookedUp.lookup(k) match
                case Some(Exit.Success(Some(v))) => cache.put(k, v)
                case _                           => populateSome(k)
            // 4. resolve everything from cache
            res <- cache
              .bulkLookup(requests)
              .map(r => CompletedRequestMap.fromIterable(r.map((k, v) => k -> Exit.succeed(v))))
          yield res
