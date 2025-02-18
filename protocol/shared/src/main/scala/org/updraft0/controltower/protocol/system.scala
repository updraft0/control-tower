package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.*

case class Planet(
    idx: Int,
    name: Option[String],
    typeName: String,
    typeId: TypeId
)

case class Station(
    id: Long,
    name: String,
    typeId: TypeId,
    operationId: Int,
    factionId: Option[Long],
    factionName: Option[String],
    corporationId: CorporationId,
    corporationName: String
)

case class WormholeStatic(typeId: TypeId, name: String)
case class Stargate(id: Int, systemId: SystemId, toStargateId: Long)
case class SolarSystem(
    id: SystemId,
    name: String,
    regionId: Long,
    regionName: String,
    constellationId: Long,
    constellationName: String,
    stations: Array[Station],
    planets: Array[Planet],
    effect: Option[WormholeEffect],
    systemClass: Option[WormholeClass],
    wormholeStatics: Array[WormholeStatic],
    gates: Array[Stargate],
    security: Option[Double],
    starTypeId: Option[TypeId]
)
