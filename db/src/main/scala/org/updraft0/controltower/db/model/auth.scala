package org.updraft0.controltower.db.model

import java.time.Instant
import java.util.UUID

type AllianceId    = Long
type CharacterId   = Long
type CorporationId = Long
type UserId        = Long

case class AuthCharacter(
    ownerHash: String,
    id: CharacterId,
    name: String,
    corporationId: CorporationId,
    allianceId: Option[AllianceId],
    bornAt: Instant,
    addedAt: Option[Instant],
    updatedAt: Option[Instant],
    lastOnlineAt: Option[Instant]
)

case class CharacterAuthToken(
    characterId: CharacterId,
    nonce: String,
    token: String,
    refreshToken: String,
    expiresAt: Instant,
    updatedAt: Option[Instant]
)

case class AuthUser(id: UserId, displayName: String, createdAt: Option[Instant])

case class UserCharacter(userId: UserId, characterId: CharacterId)

case class UserSession(
    sessionId: UUID,
    userId: UserId,
    createdAt: Instant,
    expiresAt: Instant,
    lastSeenAt: Option[Instant],
    ipAddress: Option[String],
    userAgent: Option[String]
)

enum MapRole derives CanEqual:
  case Viewer, Editor, Admin

enum PolicyMemberType derives CanEqual:
  case Character, Corporation, Alliance

case class MapPolicy(
    mapId: MapId,
    createdByUserId: UserId,
    createdAt: Instant
)

case class MapPolicyMember(
    mapId: MapId,
    memberId: Long,
    memberType: PolicyMemberType,
    isDeny: Boolean,
    role: MapRole,
    createdByUserId: UserId,
    createdAt: Instant,
    updatedByUserId: UserId,
    updatedAt: Instant
)
