package org.updraft0.controltower.protocol

import java.time.Instant

enum PolicyMemberType:
  case Character, Corporation, Alliance

case class MapPolicyMember(
    memberId: Long,
    memberType: PolicyMemberType,
    isDeny: Boolean,
    role: MapRole,
    createdBy: Option[Long] = None,
    createdAt: Option[Instant] = None,
    updatedBy: Option[Long] = None,
    updatedAt: Option[Instant] = None
)

case class NewMap(name: String, policyMembers: Vector[MapPolicyMember], createdByCharacterId: Long)

case class MapInfo(id: Long, name: String, createdByCharacterId: Long, createdAt: Instant)
