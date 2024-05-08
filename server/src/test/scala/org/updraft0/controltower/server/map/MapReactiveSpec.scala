package org.updraft0.controltower.server.map

import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.db.model.*
import zio.test.*
import java.time.Instant
import java.util.UUID

object MapReactiveSpec extends ZIOSpecDefault:

  val DummySession  = MapSessionId(CharacterId(12345L), UUID.fromString("40000000-3000-2000-1000-000000000000"))
  val DummySession2 = MapSessionId(CharacterId(67890L), UUID.fromString("50000000-6000-7000-8000-900000000000"))

  val Time1 = Instant.ofEpochMilli(201000L)
  val Time2 = Instant.ofEpochMilli(202000L)

  val MapId     = 1L
  val SystemId1 = 30000142L
  val SystemId2 = 31002604L

  override def spec = suite("toModelSignature")(
    test("converts an Unknown signature"):
        val newSig = NewMapSystemSignature("UKN-000", SignatureGroup.Unknown)
        val expected = modelSignature(
          MapId,
          SystemId1,
          "UKN-000",
          createdAt = Time1,
          createdByCharacterId = DummySession.characterId,
          updatedAt = Time1,
          updatedByCharacterId = DummySession.characterId
        )
        assertTrue(
          toModelSignature(Time1, DummySession, (MapId, SystemId1), None, newSig) == expected
        )
    ,
    test("converts a Data signature"):
        val newSig = NewMapSystemSignature("DAT-001", SignatureGroup.Data, Some("Unsecured Frontier Enclave Relay"))
        val expected = modelSignature(
          MapId,
          SystemId1,
          "DAT-001",
          signatureGroup = SignatureGroup.Data,
          signatureTypeName = Some("Unsecured Frontier Enclave Relay"),
          createdAt = Time1,
          createdByCharacterId = DummySession.characterId,
          updatedAt = Time1,
          updatedByCharacterId = DummySession.characterId
        )
        assertTrue(
          toModelSignature(Time1, DummySession, (MapId, SystemId1), None, newSig) == expected
        )
    ,
    test("converts a Wormhole signature of Unknown subtype"):
        val newSig = NewMapSystemSignature("WHO-002", SignatureGroup.Wormhole)
        val expected = modelSignature(
          MapId,
          SystemId1,
          "WHO-002",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeMassSize = Some(WormholeMassSize.Unknown),
          wormholeMassStatus = Some(WormholeMassStatus.Unknown),
          createdAt = Time1,
          createdByCharacterId = DummySession.characterId,
          updatedAt = Time1,
          updatedByCharacterId = DummySession.characterId
        )
        assertTrue(
          toModelSignature(Time1, DummySession, (MapId, SystemId1), None, newSig) == expected
        )
    ,
    test("converts a Wormhole signature with a known type id and mass"):
        val newSig = NewMapSystemSignature("WHO-003", SignatureGroup.Wormhole)
        val expected = modelSignature(
          MapId,
          SystemId1,
          "WHO-003",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeMassSize = Some(WormholeMassSize.Unknown),
          wormholeMassStatus = Some(WormholeMassStatus.Unknown),
          createdAt = Time1,
          createdByCharacterId = DummySession.characterId,
          updatedAt = Time1,
          updatedByCharacterId = DummySession.characterId
        )
        assertTrue(
          toModelSignature(Time1, DummySession, (MapId, SystemId1), None, newSig) == expected
        )
    ,
    test("updates a signature Unknown -> Relic"):
        val prevSig = modelSignature(
          MapId,
          SystemId1,
          "REL-001",
          signatureGroup = SignatureGroup.Unknown,
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time1,
          updatedByCharacterId = CharacterId(42L)
        )
        val newSig = NewMapSystemSignature("REL-001", SignatureGroup.Relic, Some("Decayed Blood Raider Mass Grave"))
        val expected = modelSignature(
          MapId,
          SystemId1,
          "REL-001",
          signatureGroup = SignatureGroup.Relic,
          signatureTypeName = Some("Decayed Blood Raider Mass Grave"),
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time2,
          updatedByCharacterId = DummySession.characterId
        )
        assertTrue(
          toModelSignature(Time2, DummySession, (MapId, SystemId1), Some(prevSig), newSig) == expected
        )
    ,
    test("updates a signature Unknown -> Wormhole (Unknown)"):
        val prevSig = modelSignature(
          MapId,
          SystemId1,
          "WHO-003",
          signatureGroup = SignatureGroup.Unknown,
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time1,
          updatedByCharacterId = CharacterId(42L)
        )
        val newSig = NewMapSystemSignature("WHO-003", SignatureGroup.Wormhole)
        val expected = modelSignature(
          MapId,
          SystemId1,
          "WHO-003",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeMassSize = Some(WormholeMassSize.Unknown),
          wormholeMassStatus = Some(WormholeMassStatus.Unknown),
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time2,
          updatedByCharacterId = DummySession.characterId
        )
        assertTrue(
          toModelSignature(Time2, DummySession, (MapId, SystemId1), Some(prevSig), newSig) == expected
        )
    ,
    test("updates a signature Wormhole (Unknown) -> Wormhole (K162)"):
        val prevSig = modelSignature(
          MapId,
          SystemId1,
          "WHO-004",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeMassSize = Some(WormholeMassSize.Unknown),
          wormholeMassStatus = Some(WormholeMassStatus.Unknown),
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time1,
          updatedByCharacterId = CharacterId(42L)
        )
        val newSig = NewMapSystemSignature(
          "WHO-004",
          SignatureGroup.Wormhole,
          wormholeMassStatus = WormholeMassStatus.Fresh,
          wormholeK162Type = Some(WormholeK162Type.Dangerous)
        )
        val expected = modelSignature(
          MapId,
          SystemId1,
          "WHO-004",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeMassSize = Some(WormholeMassSize.Unknown),
          wormholeMassStatus = Some(WormholeMassStatus.Fresh),
          wormholeK162Type = Some(WormholeK162Type.Dangerous),
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time2,
          updatedByCharacterId = DummySession.characterId
        )
        assertTrue(
          toModelSignature(Time2, DummySession, (MapId, SystemId1), Some(prevSig), newSig) == expected
        )
    ,
    test("updates a signature Wormhole (Unknown) -> Wormhole (A009)"):
        val prevSig = modelSignature(
          MapId,
          SystemId1,
          "WHO-005",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeMassSize = Some(WormholeMassSize.Unknown),
          wormholeMassStatus = Some(WormholeMassStatus.Unknown),
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time1,
          updatedByCharacterId = CharacterId(42L)
        )
        val newSig = NewMapSystemSignature(
          "WHO-005",
          SignatureGroup.Wormhole,
          wormholeMassSize = WormholeMassSize.S,
          wormholeMassStatus = WormholeMassStatus.Fresh,
          wormholeTypeId = Some(34439)
        )
        val expected = modelSignature(
          MapId,
          SystemId1,
          "WHO-005",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeMassSize = Some(WormholeMassSize.S),
          wormholeMassStatus = Some(WormholeMassStatus.Fresh),
          wormholeTypeId = Some(34439),
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time2,
          updatedByCharacterId = DummySession.characterId
        )
        assertTrue(
          toModelSignature(Time2, DummySession, (MapId, SystemId1), Some(prevSig), newSig) == expected
        )
    ,
    test("updates a signature Wormhole with the scanning time only"):
        val prevSig = modelSignature(
          MapId,
          SystemId1,
          "WHO-006",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeMassSize = Some(WormholeMassSize.S),
          wormholeMassStatus = Some(WormholeMassStatus.Fresh),
          wormholeTypeId = Some(34439),
          wormholeConnectionId = Some(1234L),
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time1,
          updatedByCharacterId = CharacterId(42L)
        )
        val newSig = NewMapSystemSignature("WHO-006", SignatureGroup.Wormhole)
        val expected = prevSig.copy(
          updatedAt = Time2,
          updatedByCharacterId = DummySession.characterId
        )

        assertTrue(
          toModelSignature(Time2, DummySession, (MapId, SystemId1), Some(prevSig), newSig) == expected
        )
    ,
    test("updates a signature Wormhole with the scanning time, leaving the EOL status (and time) untouched"):
        val prevSig = modelSignature(
          MapId,
          SystemId1,
          "WHO-007",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeIsEol = Some(true),
          wormholeEolAt = Some(Time1),
          wormholeTypeId = Some(34439),
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time1,
          updatedByCharacterId = CharacterId(42L)
        )
        val newSig = NewMapSystemSignature("WHO-007", SignatureGroup.Unknown)
        val expected = prevSig.copy(
          updatedAt = Time2,
          updatedByCharacterId = DummySession.characterId
        )

        assertTrue(
          toModelSignature(Time2, DummySession, (MapId, SystemId1), Some(prevSig), newSig) == expected
        )
    ,
    test("updates a signature Wormhole from K162 -> A009"):
        val prevSig = modelSignature(
          MapId,
          SystemId1,
          "WHO-008",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeK162Type = Some(WormholeK162Type.Unknown),
          wormholeMassSize = Some(WormholeMassSize.S),
          wormholeMassStatus = Some(WormholeMassStatus.Fresh),
          wormholeConnectionId = Some(1234L),
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time1,
          updatedByCharacterId = CharacterId(42L)
        )
        val newSig = NewMapSystemSignature("WHO-008", SignatureGroup.Wormhole, wormholeTypeId = Some(34439))
        val expected = modelSignature(
          MapId,
          SystemId1,
          "WHO-008",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeMassSize = Some(WormholeMassSize.S),
          wormholeMassStatus = Some(WormholeMassStatus.Fresh),
          wormholeTypeId = Some(34439),
          wormholeConnectionId = Some(1234L),
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time2,
          updatedByCharacterId = DummySession.characterId
        )

        assertTrue(
          toModelSignature(Time2, DummySession, (MapId, SystemId1), Some(prevSig), newSig) == expected
        )
    ,
    test("updates a signature Wormhole by removing its EOL status"):
        val prevSig = modelSignature(
          MapId,
          SystemId1,
          "WHO-009",
          signatureGroup = SignatureGroup.Wormhole,
          wormholeIsEol = Some(true),
          wormholeEolAt = Some(Time1),
          wormholeTypeId = Some(34439),
          createdAt = Time1,
          createdByCharacterId = CharacterId(42L),
          updatedAt = Time1,
          updatedByCharacterId = CharacterId(42L)
        )
        val newSig = NewMapSystemSignature("WHO-009", SignatureGroup.Wormhole, wormholeIsEol = Some(false))
        val expected = prevSig.copy(
          wormholeIsEol = Some(false),
          wormholeEolAt = None,
          updatedAt = Time2,
          updatedByCharacterId = DummySession.characterId
        )

        assertTrue(
          toModelSignature(Time2, DummySession, (MapId, SystemId1), Some(prevSig), newSig) == expected
        )
  )

  // TODO: specs for MapReactive
  //      group: connection handling
  //        - adding a signature with a connection sends appropriate updates
  //        - rank of system signatures (e.g. when you have a ring)

private def modelSignature(
    mapId: MapId,
    systemId: SystemId,
    signatureId: String,
    isDeleted: Boolean = false,
    signatureGroup: SignatureGroup = SignatureGroup.Unknown,
    signatureTypeName: Option[String] = None,
    wormholeIsEol: Option[Boolean] = None,
    wormholeEolAt: Option[Instant] = None,
    wormholeTypeId: Option[Long] = None,
    wormholeMassSize: Option[WormholeMassSize] = None,
    wormholeMassStatus: Option[WormholeMassStatus] = None,
    wormholeK162Type: Option[WormholeK162Type] = None,
    wormholeConnectionId: Option[Long] = None,
    createdAt: Instant = Instant.EPOCH,
    createdByCharacterId: CharacterId = CharacterId(0L),
    updatedAt: Instant = Instant.EPOCH,
    updatedByCharacterId: CharacterId = CharacterId(0L)
): MapSystemSignature =
  MapSystemSignature(
    mapId = mapId,
    systemId = systemId,
    signatureId = signatureId,
    isDeleted = isDeleted,
    signatureGroup = signatureGroup,
    signatureTypeName = signatureTypeName,
    wormholeIsEol = wormholeIsEol,
    wormholeEolAt = wormholeEolAt,
    wormholeTypeId = wormholeTypeId,
    wormholeMassSize = wormholeMassSize,
    wormholeMassStatus = wormholeMassStatus,
    wormholeK162Type = wormholeK162Type,
    wormholeConnectionId = wormholeConnectionId,
    createdAt = createdAt,
    createdByCharacterId = createdByCharacterId,
    updatedAt = updatedAt,
    updatedByCharacterId = updatedByCharacterId
  )
