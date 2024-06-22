package org.updraft0.controltower.sdeloader

import io.getquill.JsonValue
import org.updraft0.controltower.constant.{SystemId, WormholeClass, WormholeClasses}
import org.updraft0.controltower.db.model.{SignatureGroup, SignatureInGroup, SystemStaticWormhole, Wormhole}
import org.updraft0.controltower.db.query.map
import zio.*
import zio.stream.{ZPipeline, ZStream}

import java.io.IOException
import javax.sql.DataSource

/** Load data derived from the SDE export (including static mappings not found elsewhere such as wormhole statics)
  */
def loadDerivedData: RIO[DataSource, Long] =
  for
    whc <- ZIO.logSpan("loading wormhole attributes")(loadDerivedWormholeAttributes())
    sc  <- ZIO.logSpan("loading wormhole statics")(loadDerivedWormholeStatics())
    sig <- ZIO.logSpan("loading group signature types")(loadSignaturesInGroup())
  yield whc + sc + sig

private def loadDerivedWormholeAttributes(): RIO[DataSource, Long] =
  map.getWormholesUsingTypeDogma
    .map(_.map(toWormhole.tupled.apply).filterNot(_.name.startsWith("QA ")))
    .flatMap(whs => map.upsertWormholes(whs))

private def loadDerivedWormholeStatics(): RIO[DataSource, Long] =
  for
    systemsByName <- map.getWormholeSystemNames
    wormholeTypes <- wormholeTypeNamesNonUnique
    statics       <- readSystemStatics(systemsByName, wormholeTypes).runCollect
    loaded        <- map.upsertSystemStatics(statics.toList)
  yield loaded

private def loadSignaturesInGroup(): RIO[DataSource, Long] =
  for
    wormholeTypes <- wormholeTypeNamesNonUnique
    sigs          <- readSignaturesInGroup(wormholeTypes).runCollect
    loaded        <- map.upsertSignaturesInGroup(sigs.toList)
  yield loaded

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
    systemsByName: Map[String, SystemId],
    wormholeTypes: Map[String, Long]
): ZStream[Any, Throwable, SystemStaticWormhole] =
  ZStream
    .fromInputStreamZIO(
      ZIO
        .attemptBlockingIO(ImportState.getClass.getResourceAsStream("/map/reference/ref_system_static_wormhole.csv"))
        .filterOrFail(_ != null)(new IOException("BUG: Could not read the static csv file"))
    )
    .via(ZPipeline.utf8Decode)
    .via(ZPipeline.splitLines)
    .filterNot(_.startsWith("#"))
    .map(_.split(',') match {
      case Array(jName, staticName) => SystemStaticWormhole(systemsByName(jName), wormholeTypes(staticName))
      case _ => throw new IllegalStateException("BUG: Malformed static csv file, expected line to be jname,staticname")
    })

private def readSignaturesInGroup(wormholeTypes: Map[String, Long]): ZStream[Any, Throwable, SignatureInGroup] =
  ZStream
    .fromInputStreamZIO(
      ZIO
        .attemptBlockingIO(ImportState.getClass.getResourceAsStream("/map/reference/ref_signature_in_group.csv"))
        .filterOrFail(_ != null)(new IOException("BUG: Could not read the signature group csv file"))
    )
    .via(ZPipeline.utf8Decode)
    .via(ZPipeline.splitLines)
    .filterNot(l => l.startsWith("#") || l.isBlank)
    .map(l =>
      l.split(';') match {
        case Array(groupName, name, whClasses) =>
          val group   = SignatureGroup.valueOf(groupName)
          val classes = whClasses.split(',').map(WormholeClass.valueOf).toSet
          if (group == SignatureGroup.Wormhole && !wormholeTypes.contains(name))
            throw new IllegalStateException(s"Invalid wormhole type '$name' in file")
          SignatureInGroup(group, name, classes)
        case _ =>
          throw new IllegalStateException(
            s"BUG: Malformed signature group csv file, expected line '$l' to be group;name;class1,class2..."
          )
      }
    )
