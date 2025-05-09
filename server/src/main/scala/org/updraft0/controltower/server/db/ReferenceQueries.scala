package org.updraft0.controltower.server.db

import io.getquill.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.query.*
import org.updraft0.controltower.protocol
import org.updraft0.controltower.protocol.jsoncodec.{config, given}
import zio.*

import scala.annotation.nowarn

case class StargateBothSides(
    inGateId: Long,
    outGateId: Long,
    inSystemId: SystemId,
    outSystemId: SystemId
)
case class SolarSystemWithGates(sys: model.SolarSystem, gates: Array[StargateBothSides])

/** Queries for "reference" data (not map-dependent)
  */
@nowarn("msg=unused import")
object ReferenceQueries:
  import ctx.{*, given}
  import auth.given
  import map.given
  import map.schema.*
  import sde.schema.*

  import com.github.plokhotnyuk.jsoniter_scala.core.*
  import com.github.plokhotnyuk.jsoniter_scala.macros.*

  private given JsonValueCodec[StargateBothSides] = JsonCodecMaker.make(config)

  private val ShipCategoryId = 6
  private val StructureGroupIds = Set(
    1404 /* Engineering Complex */,
    1406 /* Refinery */,
    1657 /* Citadel */,
    365 /* POS */
  )

  def getFactions: Result[List[protocol.Faction]] =
    run(faction).map(_.map(f => protocol.Faction(f.id, f.name, f.corporationId, f.militiaCorporationId)))

  def getShipTypes: Result[List[protocol.ShipType]] =
    run(itemType.join(itemGroup.filter(_.categoryId == lift(ShipCategoryId))).on(_.groupId == _.id))
      .map(
        _.map((it, ig) =>
          protocol.ShipType(
            TypeId(it.id.toInt) /* FIXME */,
            it.name,
            it.groupId,
            ig.name,
            it.mass.getOrElse(0.0).toLong
          )
        )
      )

  def getAllSolarSystemsWithGates: Result[List[SolarSystemWithGates]] =
    run(quote {
      (for
        ss      <- solarSystem
        r       <- region.join(_.id == ss.regionId)
        inGate  <- stargate.leftJoin(_.systemId == ss.id)
        outGate <- stargate.leftJoin(osg => inGate.exists(_.toStargateId == osg.id))
      yield (ss, r, inGate, outGate)).groupByMap((ss, _, _, _) => ss.id)((ss, r, isg, osg) =>
        (
          ss,
          r.whClassId,
          jsonGroupArrayFilterNullDistinct[StargateBothSides](
            jsonObject4(
              "in_gate_id",
              isg.map(_.id),
              "out_gate_id",
              osg.map(_.systemId),
              "in_system_id",
              isg.map(_.systemId),
              "out_system_id",
              osg.map(_.systemId)
            ),
            isg.map(_.id)
          )
        )
      )
    }).map(
      _.map((ss, whClass, sgs) => SolarSystemWithGates(ss.copy(whClassId = ss.whClassId.orElse(whClass)), sgs.value))
    )

  def getSolarSystemByName(name: String): Result[List[protocol.SolarSystem]] =
    getSolarSystemsByName(Some(name), None)

  def getSolarSystemById(id: SystemId): Result[List[protocol.SolarSystem]] =
    getSolarSystemsByName(None, Some(id))

  def getSolarSystemsByName(name: Option[String], id: Option[SystemId]): Result[List[protocol.SolarSystem]] =
    run(quote {
      (for
        sys <- solarSystem.filter(s =>
          infix"(${lift(name).forall(_ == s.name)})".asCondition &&
            infix"(${lift(id).forall(_ == s.id)})".asCondition
        )
        reg <- region.join(_.id == sys.regionId)
        ptj <- (solarSystemPlanet
          .join(itemType)
          .on(_.typeId == _.id)
          .leftJoin(itemName)
          .on(_._1.id == _.id))
          .leftJoin(_._1._1.systemId == sys.id)
        stj <- (npcStation
          .join(npcCorporation)
          .on(_.ownerId == _.id)
          .leftJoin(faction)
          .on((sc, f) => sc._2.factionId == Option(f.id)))
          .leftJoin(_._1._1.systemId == sys.id)
        whj <- (systemStaticWormhole
          .filter(ss => sql"${ss.validFrom} < (unixepoch() * 1000)".asCondition)
          .filter(ss => sql"${ss.validUntil} > (unixepoch() * 1000)".asCondition)
          .join(wormhole)
          .on(_.staticTypeId == _.typeId))
          .leftJoin(_._1.systemId == sys.id)
        sgj <- stargate.leftJoin(_.systemId == sys.id)
      yield (sys, reg, ptj, stj, whj, sgj))
        .groupByMap((sys, reg, _, _, _, _) => (sys, reg))((sys, reg, ptj, stj, whj, sgj) =>
          (
            sys,
            reg,
            jsonGroupArrayFilterNullDistinct[protocol.Planet](
              jsonObject4(
                "idx",
                ptj.map(_._1._1.idx),
                "name",
                ptj.map(_._2.map(_.name)),
                "type_name",
                ptj.map(_._1._2.name),
                "type_id",
                ptj.map(_._1._1.typeId)
              ),
              ptj.map(_._1._1.idx)
            ),
            jsonGroupArrayFilterNullDistinct[protocol.Station](
              jsonObject8(
                "id",
                stj.map(_._1._1.id),
                "name",
                stj.map(_._1._1.name),
                "type_id",
                stj.map(_._1._1.typeId),
                "corporation_id",
                stj.map(_._1._1.ownerId),
                "operation_id",
                stj.map(_._1._1.operationId),
                "corporation_name",
                stj.map(_._1._2.name),
                "faction_id",
                stj.flatMap(_._1._2.factionId),
                "faction_name",
                stj.flatMap(_._2).map(_.name)
              ),
              stj.map(_._1._1.id)
            ),
            jsonGroupArrayFilterNullDistinct[protocol.WormholeStatic](
              jsonObject2(
                "type_id",
                whj.map(_._1.staticTypeId),
                "name",
                whj.map(_._2.name)
              ),
              whj.map(_._1.systemId)
            ),
            jsonGroupArrayFilterNullDistinct[protocol.Stargate](
              jsonObject3(
                "id",
                sgj.map(_.id),
                "system_id",
                sgj.map(_.systemId),
                "to_stargate_id",
                sgj.map(_.toStargateId)
              ),
              sgj.map(_.id)
            )
          )
        )
    }).map(
      _.map((sys, reg, planets, stations, wormholeStatics, stargates) =>
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
          gates = stargates.value,
          security = sys.security,
          starTypeId = sys.starTypeId.map(id => TypeId(id.toInt) /* FIXME */ )
        )
      )
    )

  def getStarTypes: Result[List[protocol.StarType]] =
    run(quote {
      (for
        it <- itemType
        _  <- itemGroup.join(_.id == it.groupId).filter(_.name == "Sun")
      yield it)
    }).map(_.map(it => protocol.StarType(TypeId(it.id.toInt) /* FIXME */, it.name.stripPrefix("Sun "))))

  def getStationOperations: Result[List[protocol.StationOperation]] =
    run(quote {
      (for
        op    <- stationOperation
        opSvc <- stationOperationService.join(_.operationId == op.id)
        svc   <- stationService.join(_.id == opSvc.serviceId)
      yield (op.id, op.name, svc.id, svc.name))
        .groupByMap(k => (k._1, k._2))(v =>
          (v._1, v._2, jsonGroupArray[protocol.StationService](jsonObject2("id", v._3, "name", v._4)))
        )
    }).map(_.map(r => protocol.StationOperation(r._1.toInt, r._2, r._3.value)))

  def getWormholeTypes: Result[List[protocol.WormholeType]] =
    run(wormhole).map(
      _.map(wh =>
        protocol.WormholeType(
          TypeId(wh.typeId.toInt) /* FIXME */,
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
          sig.targetClasses.toArray
        )
      )
    )

  def getStructureTypes: Result[List[model.ItemType]] =
    run(
      quote(
        itemType
          .filter(it => liftQuery(StructureGroupIds).contains(it.groupId))
          .filter(_.name != "QA Control Tower")
          .filter(_.name != "QA Fuel Control Tower")
      )
    )

  def getStructureTypesAsProto: Result[Map[TypeId, protocol.StructureType]] =
    getStructureTypes.map(
      _.map: item =>
        val typeId = TypeId(item.id.toInt)
        (
          typeId,
          item.groupId match
            case 365L /* POS */ =>
              protocol.StructureType.PlayerOwned(
                size =
                  if (item.name.endsWith("Small")) protocol.PlayerStructureSize.Small
                  else if (item.name.endsWith("Medium")) protocol.PlayerStructureSize.Medium
                  else protocol.PlayerStructureSize.Large,
                typeName = item.name,
                typeId = typeId
              )
            case _ =>
              val typeSimple =
                if (item.name.contains("Fortizar")) "Fortizar"
                else if (item.name.contains("Keepstar")) "Keepstar"
                else item.name
              protocol.StructureType.Upwell(
                `type` = protocol.UpwellStructureType.valueOf(typeSimple),
                typeName = item.name,
                typeId = typeId
              )
        )
      .toMap
    )
