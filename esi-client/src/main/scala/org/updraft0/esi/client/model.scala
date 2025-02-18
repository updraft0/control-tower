package org.updraft0.esi.client

import org.updraft0.controltower.constant.*

import java.time.Instant

case class CharacterRoles(roles: List[String])

case class Character(
    allianceId: Option[AllianceId],
    birthday: Instant,
    bloodlineId: Int,
    corporationId: CorporationId,
    description: Option[String],
    factionId: Option[Int],
    gender: String,
    name: String,
    raceId: Int,
    securityStatus: Option[Float],
    title: Option[String]
)
case class CharacterAffiliation(allianceId: Option[AllianceId], characterId: CharacterId, corporationId: CorporationId)

case class Corporation(
    allianceId: Option[AllianceId],
    ceoId: CharacterId,
    creatorId: CharacterId,
    dateFounded: Instant,
    description: String,
    factionId: Option[Int],
    homeStationId: Long,
    memberCount: Int,
    name: String,
    shares: Option[Int],
    taxRate: Float,
    ticker: String,
    url: Option[String],
    warEligible: Option[Boolean]
)

case class Alliance(
    creatorCorporationId: CorporationId,
    creatorId: CharacterId,
    dateFounded: Instant,
    executorCorporationId: Option[CorporationId],
    factionId: Option[Int],
    name: String,
    ticker: String
)
