package org.updraft0.controltower.db.model

import org.updraft0.controltower.constant.*

import java.time.Instant
import java.util.UUID

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

case class UserPreference(
    userId: UserId,
    preferenceJson: String
)
