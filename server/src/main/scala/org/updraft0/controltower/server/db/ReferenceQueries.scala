package org.updraft0.controltower.server.db

import io.getquill.*
import org.updraft0.controltower.constant.{WormholeClasses, WormholeEffects}
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.query.*
import org.updraft0.controltower.protocol
import org.updraft0.controltower.protocol.jsoncodec.given
import org.updraft0.controltower.protocol.{StationOperation, StationService}
import zio.*

/** Queries for "reference" data (not map-dependent)
  */
object ReferenceQueries:
  import ctx.{*, given}
  import map.given
  import map.schema.*
  import sde.schema.*
  import zio.json.*

  private val ShipCategoryId = 6

  def getVersion: Result[model.Version] =
    run(quote(version).sortBy(_.createdAt)(Ord.desc).take(1)).map(_.head)

  def getFactions: Result[List[protocol.Faction]] =
    run(faction).map(_.map(f => protocol.Faction(f.id, f.name, f.corporationId, f.militiaCorporationId)))

  def getShipTypes: Result[List[protocol.ShipType]] =
    run(itemType.join(itemGroup.filter(_.categoryId == lift(ShipCategoryId))).on(_.groupId == _.id))
      .map(_.map((it, ig) => protocol.ShipType(it.id, it.name, it.groupId, ig.name, it.mass.getOrElse(0.0).toLong)))

  def getSolarSystemByName(name: String): Result[List[protocol.SolarSystem]] =
    getSolarSystemsByName(Some(name))

  def getSolarSystemsByName(name: Option[String]): Result[List[protocol.SolarSystem]] =
    run(quote {
      (for
        sys <- solarSystem.filter(s => lift(name).forall(_ == s.name))
        reg <- region.join(_.id == sys.regionId)
        ptj <- (solarSystemPlanet
          .join(itemType)
          .on(_.typeId == _.id)
          .leftJoin(itemName)
          .on(_._1.id == _.id))
          .join(_._1._1.systemId == sys.id)
        stj <- (npcStation
          .join(npcCorporation)
          .on(_.ownerId == _.id)
          .leftJoin(faction)
          .on((sc, f) => sc._2.factionId == Option(f.id)))
          .leftJoin(_._1._1.systemId == sys.id)
        whj <- (systemStaticWormhole
          .filter(ss => sql"${ss.validFrom} < (unixepoch() * 1000)".asCondition) // TODO test this
          .filter(ss => sql"${ss.validUntil} > (unixepoch() * 1000)".asCondition)
          .join(wormhole)
          .on(_.staticTypeId == _.typeId))
          .leftJoin(_._1.systemId == sys.id)
      yield (sys, reg, ptj, stj, whj))
        .groupByMap((sys, reg, _, _, _) => (sys, reg))((sys, reg, ptj, stj, whj) =>
          (
            sys,
            reg,
            jsonGroupArrayDistinct[protocol.Planet](
              jsonObject4(
                "idx",
                ptj._1._1.idx,
                "name",
                ptj._2.map(_.name),
                "typeName",
                ptj._1._2.name,
                "typeId",
                ptj._1._1.typeId
              )
            ),
            jsonGroupArrayFilterNullDistinct[protocol.Station](
              jsonObject8(
                "id",
                stj.map(_._1._1.id),
                "name",
                stj.map(_._1._1.name),
                "typeId",
                stj.map(_._1._1.typeId),
                "corporationId",
                stj.map(_._1._1.ownerId),
                "operationId",
                stj.map(_._1._1.operationId),
                "corporationName",
                stj.map(_._1._2.name),
                "factionId",
                stj.flatMap(_._1._2.factionId),
                "factionName",
                stj.flatMap(_._2).map(_.name)
              ),
              stj.map(_._1._1.id)
            ),
            jsonGroupArrayFilterNullDistinct[protocol.WormholeStatic](
              jsonObject2(
                "typeId",
                whj.map(_._1.staticTypeId),
                "name",
                whj.map(_._2.name)
              ),
              whj.map(_._1.systemId)
            )
          )
        )
    }).map(
      _.map((sys, reg, planets, stations, wormholeStatics) =>
        protocol.SolarSystem(
          id = sys.id,
          name = sys.name,
          regionId = sys.regionId,
          regionName = sys.regionName,
          constellationId = sys.constellationId,
          constellationName = sys.constellationName,
          stations = stations.value,
          planets = planets.value,
          effect = sys.effectTypeId.map(WormholeEffects.ById.apply),
          systemClass = sys.whClassId.orElse(reg.whClassId).map(WormholeClasses.ById.apply),
          wormholeStatics = wormholeStatics.value,
          security = sys.security,
          starTypeId = sys.starTypeId
        )
      )
    )

  def getStarTypes: Result[List[protocol.StarType]] =
    run(quote {
      (for
        it <- itemType
        _  <- itemGroup.join(_.id == it.groupId).filter(_.name == "Sun")
      yield it)
    }).map(_.map(it => protocol.StarType(it.id, it.name.stripPrefix("Sun "))))

  def getStationOperations: Result[List[protocol.StationOperation]] =
    run(quote {
      (for
        op    <- stationOperation
        opSvc <- stationOperationService.join(_.operationId == op.id)
        svc   <- stationService.join(_.id == opSvc.serviceId)
      yield (op.id, op.name, svc.id, svc.name))
        .groupByMap(k => (k._1, k._2))(v =>
          (v._1, v._2, jsonGroupArray[StationService](jsonObject2("id", v._3, "name", v._4)))
        )
    }).map(_.map(r => protocol.StationOperation(r._1.toInt, r._2, r._3.value)))

  def getWormholeTypes: Result[List[protocol.WormholeType]] =
    run(wormhole).map(
      _.map(wh =>
        protocol.WormholeType(
          wh.typeId,
          wh.name,
          wh.massRegeneration,
          wh.maxJumpMass,
          wh.maxStableMass,
          wh.maxStableTime,
          wh.targetClass
        )
      )
    )

  def getSignaturesInGroup: Result[List[protocol.SignatureInGroup]] =
    run(signatureInGroup).map(
      _.map(sig =>
        protocol.SignatureInGroup(
          protocol.SignatureGroup.valueOf(sig.signatureGroup.name()),
          sig.name,
          sig.targetClasses.toList
        )
      )
    )
