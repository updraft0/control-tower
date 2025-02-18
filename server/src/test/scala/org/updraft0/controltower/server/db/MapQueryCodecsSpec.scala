package org.updraft0.controltower.server.db

import org.updraft0.json.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

import zio.*
import zio.test.*

import java.time.Instant

/** Manual test for codecs used in json_array_agg queries
  */
object MapQueryCodecsSpec extends ZIOSpecDefault with MapQueryCodecs:

  override def spec = suite("MapQueryCodecs encode/decode")(
    test("a MapSystemSignature"):
      val value = MapSystemSignature(
        mapId = MapId(42),
        systemId = SystemId(12455),
        signatureId = SigId("ABC-234"),
        isDeleted = false,
        signatureGroup = SignatureGroup.Wormhole,
        signatureTypeName = None,
        wormholeIsEol = Some(true),
        wormholeEolAt = Some(Instant.EPOCH.plusSeconds(666)),
        wormholeTypeId = None,
        wormholeMassSize = Some(WormholeMassSize.S),
        wormholeMassStatus = Some(WormholeMassStatus.Fresh),
        wormholeK162Type = Some(WormholeK162Type.Unknown),
        wormholeConnectionId = None,
        createdAt = Instant.EPOCH.plusSeconds(1),
        createdByCharacterId = CharacterId(42),
        updatedAt = Instant.EPOCH.plusSeconds(2),
        updatedByCharacterId = CharacterId(43)
      )

      val expected = Json.Obj(
        "mapId"                -> Json.Num(42),
        "systemId"             -> Json.Num(12455),
        "signatureId"          -> Json.Str("ABC-234"),
        "isDeleted"            -> Json.Num(0),
        "signatureGroup"       -> Json.Num(7),
        "wormholeIsEol"        -> Json.Num(1),
        "wormholeEolAt"        -> Json.Num(666_000),
        "wormholeMassSize"     -> Json.Num(4),
        "wormholeMassStatus"   -> Json.Num(1),
        "wormholeK162Type"     -> Json.Num(0),
        "createdAt"            -> Json.Num(1_000),
        "createdByCharacterId" -> Json.Num(42),
        "updatedAt"            -> Json.Num(2_000),
        "updatedByCharacterId" -> Json.Num(43)
      )

      assertTrue(
        value.toJsonAST == expected,
        expected.as[MapSystemSignature] == Right(value)
      )
    ,
    test("an IntelSystemStructure"):
      val value = IntelSystemStructure(
        id = IntelStructureId(1),
        mapId = MapId(42),
        systemId = SystemId(12455),
        name = Some("Home"),
        ownerCorporationId = Some(CorporationId(1245)),
        itemTypeId = TypeId(40),
        nearestPlanetIdx = None,
        nearestMoonIdx = None,
        isOnline = Some(true),
        isDeleted = false,
        createdAt = Instant.EPOCH.plusSeconds(1),
        createdByCharacterId = CharacterId(42),
        updatedAt = Instant.EPOCH.plusSeconds(2),
        updatedByCharacterId = CharacterId(43),
        deletedAt = None,
        deletedByCharacterId = None
      )

      val expected = Json.Obj(
        "id"                   -> Json.Num(1),
        "mapId"                -> Json.Num(42),
        "systemId"             -> Json.Num(12455),
        "name"                 -> Json.Str("Home"),
        "ownerCorporationId"   -> Json.Num(1245),
        "itemTypeId"           -> Json.Num(40),
        "isOnline"             -> Json.Num(1),
        "isDeleted"            -> Json.Num(0),
        "createdAt"            -> Json.Num(1_000),
        "createdByCharacterId" -> Json.Num(42),
        "updatedAt"            -> Json.Num(2_000),
        "updatedByCharacterId" -> Json.Num(43)
      )

      assertTrue(
        value.toJsonAST == expected,
        expected.as[IntelSystemStructure] == Right(value)
      )
    ,
    test("an IntelSystemNote"):
      val value = IntelSystemNote(
        id = IntelNoteId(156547),
        mapId = MapId(42),
        systemId = SystemId(12455),
        note = "This is a note",
        isPinned = true,
        isDeleted = true,
        originalId = None,
        createdAt = Instant.EPOCH.plusSeconds(1),
        createdByCharacterId = CharacterId(42),
        deletedAt = Some(Instant.EPOCH.plusSeconds(2)),
        deletedByCharacterId = Some(CharacterId(43))
      )

      val expected = Json.Obj(
        "id"                   -> Json.Num(156547),
        "mapId"                -> Json.Num(42),
        "systemId"             -> Json.Num(12455),
        "note"                 -> Json.Str("This is a note"),
        "isPinned"             -> Json.Num(1),
        "isDeleted"            -> Json.Num(1),
        "createdAt"            -> Json.Num(1_000),
        "createdByCharacterId" -> Json.Num(42),
        "deletedAt"            -> Json.Num(2_000),
        "deletedByCharacterId" -> Json.Num(43)
      )

      assertTrue(
        value.toJsonAST == expected,
        expected.as[IntelSystemNote] == Right(value)
      )
    ,
    test("an IntelSystemPing"):
      val value = IntelSystemPing(
        id = IntelPingId(156547),
        mapId = MapId(42),
        systemId = SystemId(12455),
        pingUserId = Some(UserId(12)),
        pingMapGlobal = None,
        pingNote = Some("Help!"),
        isDeleted = false,
        createdAt = Instant.EPOCH.plusSeconds(1),
        createdByCharacterId = CharacterId(42),
        deletedAt = None,
        deletedByCharacterId = None
      )

      val expected = Json.Obj(
        "id"                   -> Json.Num(156547),
        "mapId"                -> Json.Num(42),
        "systemId"             -> Json.Num(12455),
        "pingUserId"           -> Json.Num(12),
        "pingNote"             -> Json.Str("Help!"),
        "isDeleted"            -> Json.Num(0),
        "createdAt"            -> Json.Num(1_000),
        "createdByCharacterId" -> Json.Num(42)
      )

      assertTrue(
        value.toJsonAST == expected,
        expected.as[IntelSystemPing] == Right(value)
      )
    ,
    test("a MapWormholeConnection"):
      val value = MapWormholeConnection(
        id = ConnectionId(156547),
        mapId = MapId(42),
        fromSystemId = SystemId(12455),
        toSystemId = SystemId(51861),
        isDeleted = false,
        createdAt = Instant.EPOCH.plusSeconds(1),
        createdByCharacterId = CharacterId(42),
        updatedAt = Instant.EPOCH.plusSeconds(2),
        updatedByCharacterId = CharacterId(43)
      )

      val expected = Json.Obj(
        "id"                   -> Json.Num(156547),
        "mapId"                -> Json.Num(42),
        "fromSystemId"         -> Json.Num(12455),
        "toSystemId"           -> Json.Num(51861),
        "isDeleted"            -> Json.Num(0),
        "createdAt"            -> Json.Num(1_000),
        "createdByCharacterId" -> Json.Num(42),
        "updatedAt"            -> Json.Num(2_000),
        "updatedByCharacterId" -> Json.Num(43)
      )

      assertTrue(
        value.toJsonAST == expected,
        expected.as[MapWormholeConnection] == Right(value)
      )
    ,
    test("a MapWormholeConnectionJump"):
      val value = MapWormholeConnectionJump(
        connectionId = ConnectionId(156547),
        characterId = CharacterId(995),
        shipTypeId = TypeId(883),
        massOverride = None,
        createdAt = Instant.EPOCH.plusSeconds(1)
      )

      val expected = Json.Obj(
        "connectionId" -> Json.Num(156547),
        "characterId"  -> Json.Num(995),
        "shipTypeId"   -> Json.Num(883),
        "createdAt"    -> Json.Num(1_000)
      )

      assertTrue(
        value.toJsonAST == expected,
        expected.as[MapWormholeConnectionJump] == Right(value)
      )
  )
