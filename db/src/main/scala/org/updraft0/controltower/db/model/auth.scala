package org.updraft0.controltower.db.model

import java.time.Instant
import java.util.UUID

case class AuthCharacter(
    ownerHash: String,
    id: Long,
    name: String,
    corporationId: Long,
    allianceId: Option[Long],
    bornAt: Instant,
    addedAt: Option[Instant],
    updatedAt: Option[Instant],
    lastOnlineAt: Option[Instant]
)

case class CharacterAuthToken(
    characterId: Long,
    token: String,
    refreshToken: String,
    expiresAt: Instant,
    updatedAt: Option[Instant]
)

case class AuthUser(id: Long, displayName: String, createdAt: Option[Instant])

case class UserCharacter(userId: Long, characterId: Long)

case class UserSession(
    sessionId: UUID,
    userId: Long,
    createdAt: Instant,
    expiresAt: Instant,
    lastSeenAt: Option[Instant],
    ipAddress: Option[String],
    userAgent: Option[String]
)

enum MapRole:
  case Viewer, Editor, Admin

enum PolicyMemberType:
  case Character, Corporation, Alliance

case class MapPolicy(
    mapId: Long,
    defaultRole: MapRole,
    createdBy: Long,
    createdAt: Instant
)

case class MapPolicyMember(
    mapId: Long,
    memberId: Long,
    memberType: PolicyMemberType,
    isDeny: Boolean,
    role: MapRole,
    createdBy: Long,
    createdAt: Instant,
    updatedBy: Long,
    updatedAt: Instant
)
