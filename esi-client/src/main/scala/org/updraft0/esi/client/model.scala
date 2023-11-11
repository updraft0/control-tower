package org.updraft0.esi.client

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
