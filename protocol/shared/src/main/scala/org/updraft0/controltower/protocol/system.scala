package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.{WormholeClass, WormholeEffect}

case class Planet(
    idx: Int,
    name: Option[String],
    typeName: String,
    typeId: Long
)

case class Station(
    id: Long,
    name: String,
    typeId: Long,
    operationId: Int,
    factionId: Option[Long],
    factionName: Option[String],
    corporationId: Long,
    corporationName: String
)

case class WormholeStatic(typeId: Int, name: String)
case class Stargate(id: Int, systemId: Long, toSystemId: Long)
case class SolarSystem(
    id: Long,
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
    starTypeId: Option[Long]
)
