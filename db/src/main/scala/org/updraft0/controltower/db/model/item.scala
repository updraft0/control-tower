package org.updraft0.controltower.db.model

import org.updraft0.controltower.constant.TypeId
import java.time.Instant

case class Faction(
    id: Long,
    name: String,
    corporationId: Option[Long],
    iconId: Long,
    militiaCorporationId: Option[Long],
    sizeFactor: Double,
    systemId: Long,
    uniqueName: Boolean
)
case class ItemCategory(id: Long, name: String, iconId: Option[Long])
case class ItemGroup(id: Long, categoryId: Long, name: String, iconId: Option[Long])
case class ItemType(
    id: TypeId,
    name: String,
    groupId: Long,
    mass: Option[Double],
    volume: Option[Double]
)
case class NpcCorporation(
    id: Long,
    name: String,
    ceoId: Option[Long],
    raceId: Option[Int],
    factionId: Option[Long],
    iconId: Option[Long],
    solarSystemId: Option[Long],
    stationId: Option[Long],
    ticker: String,
    uniqueName: Boolean
)

case class StationOperation(id: Long, activityId: Int, name: String)
case class StationOperationService(operationId: Long, serviceId: Long)
case class StationService(id: Long, name: String)

case class SdeLoadMeta(sizeBytes: Long, checksum: String)

/** Metadata about refreshing the SDE import
  */
case class Version(id: Int, createdAt: Instant, releasedAt: Instant, buildNumber: Long, meta: SdeLoadMeta)
