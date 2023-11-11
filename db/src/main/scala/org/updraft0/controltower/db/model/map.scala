package org.updraft0.controltower.db.model

import java.time.Instant

// note: using the name `Map` conflicts with prelude
case class MapModel(id: Long, name: String, createdAt: Instant, creatorUserId: Long)

case class Alliance(
    id: Long,
    name: String,
    ticker: String,
    creatorCharacterId: Long,
    creatorCorporationId: Long,
    executorCorporationId: Long,
    createdAt: Instant,
    updatedAt: Instant
)

case class Corporation(
    id: Long,
    name: String,
    ticker: String,
    allianceId: Option[Long],
    ceoCharacterId: Long,
    creatorCharacterId: Long,
    homeStationId: Long,
    memberCount: Int,
    url: Option[String],
    createdAt: Instant,
    updatedAt: Instant
)
