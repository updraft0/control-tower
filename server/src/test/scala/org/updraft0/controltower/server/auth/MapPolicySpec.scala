package org.updraft0.controltower.server.auth

import org.updraft0.controltower.db.model
import org.updraft0.controltower.server.auth.MapPolicy.resolveCharacterPoliciesToMapIds
import zio.*
import zio.test.*

import java.time.Instant

object MapPolicySpec extends ZIOSpecDefault:
  import MapPolicy.resolveCharacterMapPolicies

  private val AllianceId     = 6000L
  private val CorporationId1 = 300L
  private val CorporationId2 = 400L
  private val CharacterId1   = 10L
  private val CharacterId2   = 11L
  private val MapId1         = 1L
  private val MapId2         = 2L
  private val MapId3         = 3L

  override def spec = suite("MapPolicy.resolveCharacterMapPolicies")(
    test("[single] ==> allow(alliance, viewer)") {
      // single character policy
      val policies = List(mapPolicy(MapId1, AllianceId, model.PolicyMemberType.Alliance))
      assertTrue(
        resolveCharacterMapPolicies(Map(CharacterId1 -> policies)) == Map(
          CharacterId1 -> List(MapId1 -> model.MapRole.Viewer)
        )
      )
    },
    test("[single] ==> allow(corporation, viewer)") {
      // single character policy
      val policies = List(mapPolicy(MapId1, CorporationId1, model.PolicyMemberType.Corporation))
      assertTrue(
        resolveCharacterMapPolicies(Map(CharacterId1 -> policies)) == Map(
          CharacterId1 -> List(MapId1 -> model.MapRole.Viewer)
        )
      )
    },
    test("[single] ==> allow(character, editor)") {
      // single character policy
      val policies =
        List(mapPolicy(MapId1, CharacterId1, model.PolicyMemberType.Character, role = model.MapRole.Editor))
      assertTrue(
        resolveCharacterMapPolicies(Map(CharacterId1 -> policies)) == Map(
          CharacterId1 -> List(MapId1 -> model.MapRole.Editor)
        )
      )
    },
    test("[character, corporation] ==> allow(character, admin)") {
      // character policy takes precedence over corporation policy
      val policies = List(
        mapPolicy(MapId1, CharacterId1, model.PolicyMemberType.Character, role = model.MapRole.Admin),
        mapPolicy(MapId1, CorporationId1, model.PolicyMemberType.Corporation, role = model.MapRole.Editor)
      )
      assertTrue(
        resolveCharacterMapPolicies(Map(CharacterId1 -> policies)) == Map(
          CharacterId1 -> List(MapId1 -> model.MapRole.Admin)
        )
      )
    },
    test("[corporation, alliance] ==> allow(corporation, editor)") {
      // corporation policy takes precedence over alliance policy
      val policies = List(
        mapPolicy(MapId1, CorporationId1, model.PolicyMemberType.Corporation, role = model.MapRole.Editor),
        mapPolicy(MapId1, AllianceId, model.PolicyMemberType.Alliance, role = model.MapRole.Viewer)
      )
      assertTrue(
        resolveCharacterMapPolicies(Map(CharacterId1 -> policies)) == Map(
          CharacterId1 -> List(MapId1 -> model.MapRole.Editor)
        )
      )
    },
    test("[character, corporation, alliance] ==> deny(corporation)") {
      // the deny policy at corp level takes precedence
      val policies = List(
        mapPolicy(MapId1, CharacterId1, model.PolicyMemberType.Character),
        mapPolicy(MapId1, CorporationId1, model.PolicyMemberType.Corporation, isDeny = true),
        mapPolicy(MapId1, AllianceId, model.PolicyMemberType.Alliance)
      )
      assertTrue(resolveCharacterMapPolicies(Map(CharacterId1 -> policies)) == Map(CharacterId1 -> Nil))
    },
    test("multiple map policies") {
      val policies = List(
        // map1 - single corporation
        mapPolicy(MapId1, CorporationId2, model.PolicyMemberType.Corporation, role = model.MapRole.Editor),
        // map2 - deny, based on character
        mapPolicy(MapId2, CharacterId1, model.PolicyMemberType.Character, isDeny = true),
        mapPolicy(MapId2, CorporationId2, model.PolicyMemberType.Corporation, role = model.MapRole.Editor),
        // map3 - allow, but only as viewer (precedence DENY > character > corporation > alliance)
        mapPolicy(MapId3, CharacterId1, model.PolicyMemberType.Character, role = model.MapRole.Viewer),
        mapPolicy(MapId3, CorporationId2, model.PolicyMemberType.Corporation, role = model.MapRole.Editor),
        mapPolicy(MapId3, AllianceId, model.PolicyMemberType.Alliance, role = model.MapRole.Admin)
      )
      assertTrue(
        resolveCharacterMapPolicies(Map(CharacterId1 -> policies)) ==
          Map(CharacterId1 -> List(MapId1 -> model.MapRole.Editor, MapId3 -> model.MapRole.Viewer))
      )
    },
    test("multiple characters") {
      val policies = Map(
        CharacterId1 -> List(
          mapPolicy(MapId1, CorporationId1, model.PolicyMemberType.Corporation),
          mapPolicy(MapId2, AllianceId, model.PolicyMemberType.Alliance, role = model.MapRole.Editor)
        ),
        CharacterId2 -> List(
          mapPolicy(MapId2, CorporationId1, model.PolicyMemberType.Corporation),
          mapPolicy(MapId3, CharacterId2, model.PolicyMemberType.Character, role = model.MapRole.Admin)
        )
      )
      assertTrue(
        resolveCharacterMapPolicies(policies) == Map(
          CharacterId1 -> List(MapId1 -> model.MapRole.Viewer, MapId2 -> model.MapRole.Editor),
          CharacterId2 -> List(MapId2 -> model.MapRole.Viewer, MapId3 -> model.MapRole.Admin)
        )
      )
    }
  )

private def mapPolicy(
    mapId: model.MapId,
    memberId: Long,
    memberType: model.PolicyMemberType,
    isDeny: Boolean = false,
    role: model.MapRole = model.MapRole.Viewer
) =
  model.MapPolicyMember(
    mapId = mapId,
    memberId = memberId,
    memberType = memberType,
    isDeny = isDeny,
    role = role,
    createdByUserId = 0L,
    createdAt = Instant.EPOCH,
    updatedByUserId = 0L,
    updatedAt = Instant.EPOCH
  )
