package org.updraft0.controltower.sdeloader

import io.getquill.JsonValue
import org.updraft0.controltower.constant.WormholeClasses
import org.updraft0.controltower.db.model.{SystemStaticWormhole, Wormhole}
import org.updraft0.controltower.db.query.map
import zio.*
import zio.stream.{ZPipeline, ZStream}

import java.io.IOException
import javax.sql.DataSource
import java.time.Instant

/** Load data derived from the SDE export (including static mappings not found elsewhere such as wormhole statics)
  */
def loadDerivedData: RIO[DataSource, Long] =
  for
    whc <- loadDerivedWormholeAttributes()
    sc  <- loadDerivedWormholeStatics()
  yield whc + sc

private def loadDerivedWormholeAttributes(): RIO[DataSource, Long] =
  map.getWormholesUsingTypeDogma
    .map(_.map(toWormhole.tupled.apply).filterNot(_.name.startsWith("QA ")))
    .flatMap(whs => ZIO.foreach(whs)(map.upsertWormhole).map(_.sum))

private def loadDerivedWormholeStatics(): RIO[DataSource, Long] =
  for
    systemsByName <- map.getWormholeSystemNames
    wormholeTypes <- wormholeTypeNamesNonUnique
    statics       <- readSystemStatics(systemsByName, wormholeTypes).runCollect
    loaded <- ZIO.foreach(statics)(s =>
      map.upsertSystemStatic(s).tapError(_ => ZIO.logError(s"failed to load static: $s"))
    )
  yield loaded.sum

private inline def wormholeTypeNamesNonUnique =
  // some wormhole names like J244 are not unique, and because these are loaded from SDE using a lowest id first
  // we need to load the lowest id here too
  map.getWormholeTypeNames
    .map(
      _.map((k, v) => k.stripPrefix("Wormhole ") -> v).groupBy(_._1).map((k, xs) => k -> xs.map(_._2).sorted.head).toMap
    )

private def toWormhole(id: Long, name: String, attributes: JsonValue[Map[String, Double]]): Wormhole =
  Wormhole(
    typeId = id,
    name = name.stripPrefix("Wormhole "),
    massRegeneration = attributes.value("wormholeMassRegeneration").toLong,
    maxJumpMass = attributes.value("wormholeMaxJumpMass").toLong,
    maxStableMass = attributes.value("wormholeMaxStableMass").toLong,
    maxStableTime = attributes.value("wormholeMaxStableTime").toLong,
    targetClass = WormholeClasses.ById(attributes.value("wormholeTargetSystemClass").toInt)
  )

private def readSystemStatics(
    systemsByName: Map[String, Long],
    wormholeTypes: Map[String, Long]
): ZStream[Any, Throwable, SystemStaticWormhole] =
  ZStream
    .fromInputStreamZIO(
      ZIO
        .attemptBlockingIO(getClass.getResourceAsStream("/map/reference/ref_system_static_wormhole.csv"))
        .filterOrFail(_ != null)(new IOException("BUG: Could not read the static csv file"))
    )
    .via(ZPipeline.utf8Decode)
    .via(ZPipeline.splitLines)
    .filterNot(_.startsWith("#"))
    .map(_.split(',') match {
      case Array(jName, staticName) => SystemStaticWormhole(systemsByName(jName), wormholeTypes(staticName))
      case _ => throw new IllegalStateException("BUG: Malformed static csv file, expected line to be jname,staticname")
    })
