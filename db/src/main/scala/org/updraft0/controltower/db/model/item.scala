package org.updraft0.controltower.db.model

case class Faction(
    id: Long,
    name: String,
    corporationId: Option[Long],
    description: String,
    shortDescription: Option[String],
    iconId: Long,
    militiaCorporationId: Option[Long],
    sizeFactor: Double,
    systemId: Long,
    uniqueName: Boolean
)
case class ItemCategory(id: Long, name: String, iconId: Option[Long])
case class ItemGroup(id: Long, categoryId: Long, name: String, iconId: Option[Long])
case class ItemName(id: Long, groupId: Long, name: String)
case class ItemType(id: Long, name: String, groupId: Long, description: Option[String])
case class NpcCorporation(
    id: Long,
    name: String,
    ceoId: Option[Long],
    description: Option[String],
    raceId: Option[Int],
    factionId: Option[Long],
    iconId: Option[Long],
    solarSystemId: Option[Long],
    stationId: Option[Long],
    ticker: String,
    uniqueName: Boolean
)

case class StationOperation(id: Long, activityId: Int, name: String, description: Option[String])
case class StationOperationService(operationId: Long, serviceId: Long)
case class StationService(id: Long, name: String)
