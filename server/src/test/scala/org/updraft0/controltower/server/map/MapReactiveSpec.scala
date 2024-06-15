package org.updraft0.controltower.server.map

import org.updraft0.controltower.constant.{SystemId as _, *}
import org.updraft0.controltower.db.model.*
import org.updraft0.controltower.db.query
import org.updraft0.controltower.server.db.{
  MapQueries,
  MapWormholeConnectionWithSigs,
  MapWormholeConnectionRank,
  MapSystemWithAll,
  TempDb
}
import org.updraft0.controltower.server.tracking.{TestLocationTracker, TestPermissionTracker}
import zio.*
import zio.logging.slf4j.bridge.Slf4jBridge
import zio.test.*
import zio.test.Assertion.*
import zio.logging.{ConsoleLoggerConfig, LogFilter, LogFormat, consoleLogger}

import java.time.Instant
import java.util.UUID

object MapReactiveSpec extends ZIOSpecDefault:

  val DummySession  = MapSessionId(CharacterId(12345), UUID.fromString("40000000-3000-2000-1000-000000000000"))
  val DummySession2 = MapSessionId(CharacterId(67890), UUID.fromString("50000000-6000-7000-8000-900000000000"))

  val Time1 = Instant.ofEpochMilli(201000L)
  val Time2 = Instant.ofEpochMilli(202000L)

  val MapId1    = MapId(1)
  val SystemId1 = 30000142L
  val SystemId2 = 31002604L
  val SystemId3 = 30000140L
  val SystemId4 = 30001447L
  val SystemId5 = 30000848L
  val SystemId6 = 30045328L
  val SystemId7 = 31000003L

  val TestSolarSystems = Map(
    SystemId1 -> MapSolarSystem(SystemId1, "Jita", WormholeClass.H, 10000002L, 20000020L, Map(SystemId3 -> 50001248)),
    SystemId2 -> MapSolarSystem(SystemId2, "J000102", WormholeClass.ShatteredFrig, 11000032L, 21000333L, Map.empty),
    SystemId3 -> MapSolarSystem(
      SystemId3,
      "Maurasi",
      WormholeClass.H,
      10000002L,
      20000020L,
      Map(SystemId1 -> 50000802)
    ),
    SystemId4 -> MapSolarSystem(SystemId4, "Taisy", WormholeClass.L, 10000016L, 20000212L, Map(SystemId5 -> 50014054)),
    SystemId5 -> MapSolarSystem(
      SystemId5,
      "M-OEE8",
      WormholeClass.NS,
      10000010L,
      20000124L,
      Map(SystemId4 -> 50014053)
    ),
    SystemId6 -> MapSolarSystem(SystemId6, "Ahtila", WormholeClass.Pochven, 10000070L, 20000788L, Map.empty),
    SystemId7 -> MapSolarSystem(SystemId7, "J164710", WormholeClass.VidetteDrifter, 11000033L, 21000334L, Map.empty)
  )

  val EmptyConnections     = Map.empty[ConnectionId, MapWormholeConnectionWithSigs]
  val EmptyConnectionRanks = Map.empty[ConnectionId, MapWormholeConnectionRank]

  override def spec = suite("toModelSignature")(
    test("converts an Unknown signature"):
      val newSig = NewMapSystemSignature(SigId("UKN-000"), SignatureGroup.Unknown)
      val expected = modelSignature(
        MapId1,
        SystemId1,
        "UKN-000",
        createdAt = Time1,
        createdByCharacterId = DummySession.characterId,
        updatedAt = Time1,
        updatedByCharacterId = DummySession.characterId
      )
      assertTrue(
        toModelSignature(Time1, DummySession, (MapId1, SystemId1), None, newSig) == expected
      )
    ,
    test("converts a Data signature"):
      val newSig =
        NewMapSystemSignature(SigId("DAT-001"), SignatureGroup.Data, Some("Unsecured Frontier Enclave Relay"))
      val expected = modelSignature(
        MapId1,
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
        toModelSignature(Time1, DummySession, (MapId1, SystemId1), None, newSig) == expected
      )
    ,
    test("converts a Wormhole signature of Unknown subtype"):
      val newSig = NewMapSystemSignature(SigId("WHO-002"), SignatureGroup.Wormhole)
      val expected = modelSignature(
        MapId1,
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
        toModelSignature(Time1, DummySession, (MapId1, SystemId1), None, newSig) == expected
      )
    ,
    test("converts a Wormhole signature with a known type id and mass"):
      val newSig = NewMapSystemSignature(SigId("WHO-003"), SignatureGroup.Wormhole)
      val expected = modelSignature(
        MapId1,
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
        toModelSignature(Time1, DummySession, (MapId1, SystemId1), None, newSig) == expected
      )
    ,
    test("updates a signature Unknown -> Relic"):
      val prevSig = modelSignature(
        MapId1,
        SystemId1,
        "REL-001",
        signatureGroup = SignatureGroup.Unknown,
        createdAt = Time1,
        createdByCharacterId = CharacterId(42L),
        updatedAt = Time1,
        updatedByCharacterId = CharacterId(42L)
      )
      val newSig =
        NewMapSystemSignature(SigId("REL-001"), SignatureGroup.Relic, Some("Decayed Blood Raider Mass Grave"))
      val expected = modelSignature(
        MapId1,
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
        toModelSignature(Time2, DummySession, (MapId1, SystemId1), Some(prevSig), newSig) == expected
      )
    ,
    test("updates a signature Unknown -> Wormhole (Unknown)"):
      val prevSig = modelSignature(
        MapId1,
        SystemId1,
        "WHO-003",
        signatureGroup = SignatureGroup.Unknown,
        createdAt = Time1,
        createdByCharacterId = CharacterId(42L),
        updatedAt = Time1,
        updatedByCharacterId = CharacterId(42L)
      )
      val newSig = NewMapSystemSignature(SigId("WHO-003"), SignatureGroup.Wormhole)
      val expected = modelSignature(
        MapId1,
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
        toModelSignature(Time2, DummySession, (MapId1, SystemId1), Some(prevSig), newSig) == expected
      )
    ,
    test("updates a signature Wormhole (Unknown) -> Wormhole (K162)"):
      val prevSig = modelSignature(
        MapId1,
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
        SigId("WHO-004"),
        SignatureGroup.Wormhole,
        wormholeMassStatus = WormholeMassStatus.Fresh,
        wormholeK162Type = Some(WormholeK162Type.Dangerous)
      )
      val expected = modelSignature(
        MapId1,
        SystemId1,
        SigId("WHO-004"),
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
        toModelSignature(Time2, DummySession, (MapId1, SystemId1), Some(prevSig), newSig) == expected
      )
    ,
    test("updates a signature Wormhole (Unknown) -> Wormhole (A009)"):
      val prevSig = modelSignature(
        MapId1,
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
        SigId("WHO-005"),
        SignatureGroup.Wormhole,
        wormholeMassSize = WormholeMassSize.S,
        wormholeMassStatus = WormholeMassStatus.Fresh,
        wormholeTypeId = Some(34439)
      )
      val expected = modelSignature(
        MapId1,
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
        toModelSignature(Time2, DummySession, (MapId1, SystemId1), Some(prevSig), newSig) == expected
      )
    ,
    test("updates a signature Wormhole with the scanning time only"):
      val prevSig = modelSignature(
        MapId1,
        SystemId1,
        "WHO-006",
        signatureGroup = SignatureGroup.Wormhole,
        wormholeMassSize = Some(WormholeMassSize.S),
        wormholeMassStatus = Some(WormholeMassStatus.Fresh),
        wormholeTypeId = Some(34439),
        wormholeConnectionId = Some(ConnectionId(1234L)),
        createdAt = Time1,
        createdByCharacterId = CharacterId(42L),
        updatedAt = Time1,
        updatedByCharacterId = CharacterId(42L)
      )
      val newSig = NewMapSystemSignature(SigId("WHO-006"), SignatureGroup.Wormhole)
      val expected = prevSig.copy(
        updatedAt = Time2,
        updatedByCharacterId = DummySession.characterId
      )

      assertTrue(
        toModelSignature(Time2, DummySession, (MapId1, SystemId1), Some(prevSig), newSig) == expected
      )
    ,
    test("updates a signature Wormhole with the scanning time, leaving the EOL status (and time) untouched"):
      val prevSig = modelSignature(
        MapId1,
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
      val newSig = NewMapSystemSignature(SigId("WHO-007"), SignatureGroup.Unknown)
      val expected = prevSig.copy(
        updatedAt = Time2,
        updatedByCharacterId = DummySession.characterId
      )

      assertTrue(
        toModelSignature(Time2, DummySession, (MapId1, SystemId1), Some(prevSig), newSig) == expected
      )
    ,
    test("updates a signature Wormhole from K162 -> A009"):
      val prevSig = modelSignature(
        MapId1,
        SystemId1,
        "WHO-008",
        signatureGroup = SignatureGroup.Wormhole,
        wormholeK162Type = Some(WormholeK162Type.Unknown),
        wormholeMassSize = Some(WormholeMassSize.S),
        wormholeMassStatus = Some(WormholeMassStatus.Fresh),
        wormholeConnectionId = Some(ConnectionId(1234)),
        createdAt = Time1,
        createdByCharacterId = CharacterId(42L),
        updatedAt = Time1,
        updatedByCharacterId = CharacterId(42L)
      )
      val newSig = NewMapSystemSignature(SigId("WHO-008"), SignatureGroup.Wormhole, wormholeTypeId = Some(34439))
      val expected = modelSignature(
        MapId1,
        SystemId1,
        "WHO-008",
        signatureGroup = SignatureGroup.Wormhole,
        wormholeMassSize = Some(WormholeMassSize.S),
        wormholeMassStatus = Some(WormholeMassStatus.Fresh),
        wormholeTypeId = Some(34439),
        wormholeConnectionId = Some(ConnectionId(1234L)),
        createdAt = Time1,
        createdByCharacterId = CharacterId(42L),
        updatedAt = Time2,
        updatedByCharacterId = DummySession.characterId
      )

      assertTrue(
        toModelSignature(Time2, DummySession, (MapId1, SystemId1), Some(prevSig), newSig) == expected
      )
    ,
    test("updates a signature Wormhole by removing its EOL status"):
      val prevSig = modelSignature(
        MapId1,
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
      val newSig = NewMapSystemSignature(SigId("WHO-009"), SignatureGroup.Wormhole, wormholeIsEol = Some(false))
      val expected = prevSig.copy(
        wormholeIsEol = Some(false),
        wormholeEolAt = None,
        updatedAt = Time2,
        updatedByCharacterId = DummySession.characterId
      )

      assertTrue(
        toModelSignature(Time2, DummySession, (MapId1, SystemId1), Some(prevSig), newSig) == expected
      )
  ) + (suite("MapEntity (connection and signature handling)")(
    test(
      "Creating a connection, linking with signatures and removing one of the signatures removes both signatures and the connection"
    )(
      for
        initial <- setupInitial(MapId1, TestSolarSystems)
        // create two systems, a connection between them and a wormhole signature
        respOne <- runMultiple(
          MapId1,
          initial.state,
          Chunk(
            addSystemRequest(SystemId2),                // -> SystemSnapshot(Id2)
            addSystemRequest(SystemId5),                // -> SystemSnapshot(Id5)
            addConnectionRequest(SystemId5, SystemId2), // -> ConnectionSnapshot
            addSignaturesRequest(
              SystemId2,
              NewMapSystemSignature(SigId("ABC-222"), SignatureGroup.Wormhole)
            ) // -> SystemSnapshot(Id2)
          )
        )
        (oneState, oneResponses) = respOne
        oneConnectionId          = oneState.connections.keys.head
        // connect the signature with the connection in Id2
        respTwo <- runMultiple(
          MapId1,
          oneState,
          Chunk(
            updateSignatureRequest(
              SystemId2,
              NewMapSystemSignature(
                SigId("ABC-222"),
                SignatureGroup.Wormhole,
                wormholeTypeId = Some(34140),
                wormholeMassSize = WormholeMassSize.S,
                wormholeConnectionId = Some(oneConnectionId)
              )
            ) // -> SystemSnapshot(Id2)
          )
        )
        (twoState, twoResponses) = respTwo
        // connect the signature with the connection in Id5
        respThree <- runMultiple(
          MapId1,
          twoState,
          Chunk(
            updateSignatureRequest(
              SystemId5,
              NewMapSystemSignature(
                SigId("DEF-678"),
                SignatureGroup.Wormhole,
                wormholeTypeId = None,
                wormholeMassSize = WormholeMassSize.S,
                wormholeConnectionId = Some(oneConnectionId)
              )
            ) // -> SystemSnapshot(Id5)
          )
        )
        (threeState, threeResponses) = respThree
        // delete the signature (and expect the connection and associated signatures to be gone)
        respFour <- runMultiple(MapId1, threeState, Chunk(removeSignatureRequest(SystemId2, SigId("ABC-222"))))
        (fourState, fourResponses) = respFour
      yield assertTrue(oneResponses.size == 4) &&
        assert(oneResponses)(hasAt(0)(matches:
          case Identified(
                None,
                MapResponse.SystemSnapshot(`SystemId2`, _, `EmptyConnections`, `EmptyConnectionRanks`)
              ) =>
            true
        )) &&
        assert(oneResponses)(hasAt(1)(matches:
          case Identified(
                None,
                MapResponse.SystemSnapshot(`SystemId5`, _, `EmptyConnections`, `EmptyConnectionRanks`)
              ) =>
            true
        )) &&
        assert(oneResponses)(hasAt(2)(matches:
          case Identified(
                None,
                MapResponse.ConnectionSnapshot(
                  MapWormholeConnectionWithSigs(
                    MapWormholeConnection(_, `MapId1`, `SystemId5`, `SystemId2`, false, _, _, _, _),
                    _,
                    None,
                    None
                  ),
                  rank
                )
              ) =>
            true
        )) &&
        assert(oneResponses)(hasAt(3)(matches:
          case Identified(
                None,
                MapResponse.SystemSnapshot(
                  `SystemId2`,
                  MapSystemWithAll(
                    _,
                    _,
                    _,
                    _,
                    sigs,
                    connections
                  ),
                  connectionMap,
                  connectionRanks
                )
              ) =>
            sigs.length == 1 && connections.length == 1 && connectionMap.size == 1 && connectionRanks.size == 1 &&
            connections(0) == connectionMap(oneConnectionId).connection && sigs(0).signatureId == SigId("ABC-222")
        )) && assertTrue(twoResponses.size == 1) &&
        assert(twoResponses)(hasAt(0)(matches:
          case Identified(
                None,
                MapResponse.SystemSnapshot(
                  `SystemId2`,
                  MapSystemWithAll(
                    _,
                    _,
                    _,
                    _,
                    sigs,
                    connections
                  ),
                  connectionMap,
                  connectionRanks
                )
              ) =>
            sigs.length == 1 && connections.length == 1 && connectionMap.size == 1 && connectionRanks.size == 1 &&
            connections(0) == connectionMap(oneConnectionId).connection && sigs(0).signatureId == SigId("ABC-222") &&
            sigs(0).wormholeConnectionId.contains(oneConnectionId) &&
            connectionMap(oneConnectionId).toSignature.contains(sigs(0)) &&
            sigs(0).wormholeMassSize.contains(WormholeMassSize.S)
        )) && assertTrue(threeResponses.size == 1) &&
        assert(threeResponses)(hasAt(0)(matches:
          case Identified(
                None,
                MapResponse.SystemSnapshot(
                  `SystemId5`,
                  MapSystemWithAll(
                    _,
                    _,
                    _,
                    _,
                    sigs,
                    connections
                  ),
                  connectionMap,
                  connectionRanks
                )
              ) =>
            sigs.length == 1 && connections.length == 1 && connectionMap.size == 1 && connectionRanks.size == 1 &&
            connections(0) == connectionMap(oneConnectionId).connection && sigs(0).signatureId == SigId("DEF-678") &&
            sigs(0).wormholeConnectionId.contains(oneConnectionId) &&
            connectionMap(oneConnectionId).fromSignature.contains(sigs(0)) &&
            sigs(0).wormholeMassSize.contains(WormholeMassSize.S)
        )) && assertTrue(fourResponses.size == 3) &&
        assert(fourResponses)(hasAt(0)(matches:
          case Identified(
                None,
                MapResponse.ConnectionsRemoved(
                  MapWormholeConnection(`oneConnectionId`, _, `SystemId5`, `SystemId2`, true, _, _, _, _) :: Nil
                )
              ) =>
            true
        )) &&
        assert(fourResponses)(hasAt(1)(matches:
          case Identified(
                None,
                MapResponse.SystemSnapshot(`SystemId2`, _, `EmptyConnections`, `EmptyConnectionRanks`)
              ) =>
            true
        )) &&
        assert(fourResponses)(hasAt(2)(matches:
          case Identified(
                None,
                MapResponse.SystemSnapshot(`SystemId5`, _, `EmptyConnections`, `EmptyConnectionRanks`)
              ) =>
            true
        ))
    )
  ).provideSome[Scope](testMapLayer) @@ TestAspect.withLiveEnvironment)

  // TODO: specs for MapReactive
  //      group: connection handling
  //        - [x] adding a signature with a connection sends appropriate updates
  //        - [ ] rank of system signatures (e.g. when you have a ring)

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
      wormholeConnectionId: Option[ConnectionId] = None,
      createdAt: Instant = Instant.EPOCH,
      createdByCharacterId: CharacterId = CharacterId(0L),
      updatedAt: Instant = Instant.EPOCH,
      updatedByCharacterId: CharacterId = CharacterId(0L)
  ): MapSystemSignature =
    MapSystemSignature(
      mapId = mapId,
      systemId = systemId,
      signatureId = SigId(signatureId),
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

  case class InitialState(inQ: Queue[Identified[MapRequest]], state: MapState)

  private def setupInitial(mapId: MapId, solarSystems: Map[SystemId, MapSolarSystem]) =
    for
      _            <- query.transaction(insertSolarSystems(solarSystems) *> insertMap(mapId))
      inQ          <- Queue.unbounded[Identified[MapRequest]]
      initialState <- MapEntity.hydrate(MapId1, inQ).map(updateRefData(_, TestSolarSystems))
    yield InitialState(inQ, initialState)

  private def addSystemRequest(
      systemId: SystemId,
      sessionId: MapSessionId = DummySession,
      name: Option[String] = None,
      isPinned: Boolean = false,
      displayData: SystemDisplayData = SystemDisplayData.Manual(0, 0),
      stance: Option[IntelStance] = None
  ) = Identified(Some(sessionId), MapRequest.AddSystem(systemId, name, isPinned, displayData, stance))

  private def addConnectionRequest(
      fromSystemId: SystemId,
      toSystemId: SystemId,
      sessionId: MapSessionId = DummySession
  ) =
    Identified(Some(sessionId), MapRequest.AddSystemConnection(fromSystemId, toSystemId))

  private def addSignaturesRequest(
      systemId: SystemId,
      newSig: NewMapSystemSignature,
      sessionId: MapSessionId = DummySession
  ) =
    Identified(Some(sessionId), MapRequest.AddSystemSignature(systemId, newSig))

  private def updateSignatureRequest(
      systemId: SystemId,
      update: NewMapSystemSignature,
      sessionId: MapSessionId = DummySession
  ) = Identified(Some(sessionId), MapRequest.UpdateSystemSignatures(systemId, false, List(update)))

  private def removeSignatureRequest(systemId: SystemId, sigId: SigId, sessionId: MapSessionId = DummySession) =
    Identified(Some(sessionId), MapRequest.RemoveSystemSignatures(systemId, Some(NonEmptyChunk(sigId))))

  private def testMapLayer: ZLayer[Scope, Throwable, MapEnv] = ZLayer.make[MapEnv](
    TempDb.empty,
    TestLocationTracker.empty,
    TestPermissionTracker.empty
    // uncomment to enable debug logging in tests
//    consoleLogger(ConsoleLoggerConfig.apply(LogFormat.colored, LogFilter.LogLevelByNameConfig(LogLevel.Debug))),
//    Slf4jBridge.init(LogFilter.acceptAll)
  )

  private def insertSolarSystems(solarSystems: Map[SystemId, MapSolarSystem]) =
    for
      countR <- ZIO.foreach(solarSystems.view.values.map(mss => (mss.regionId, mss.whClass.value)).toSet):
        (rId, classId) =>
          query.sde.upsertItemName(ItemName(rId, 3, s"region-${rId}")) *>
            query.sde.upsertRegion(Region(rId, s"region-${rId}", Some(classId), None))
      countC <- ZIO.foreach(solarSystems.view.values.map(mss => (mss.regionId, mss.constellationId)).toSet)(
        (rId, cId) =>
          query.sde.upsertItemName(ItemName(rId, 4, s"constellation-${rId}")) *>
            query.sde.insertConstellation(Constellation(cId, s"constellation-${cId}", rId, s"region-${rId}"))
      )
      countS <- ZIO.foreach(solarSystems.values): mss =>
        query.sde.insertSolarSystem(
          SolarSystem(
            id = mss.systemId,
            starId = None,
            starTypeId = None,
            name = mss.name,
            regionName = "region",
            regionId = mss.regionId,
            constellationName = "constellation",
            constellationId = mss.constellationId,
            effectTypeId = None,
            whClassId = None,
            securityClass = None,
            security = None,
            border = false,
            corridor = false,
            fringe = false,
            hub = false,
            international = false,
            regional = false
          )
        )
      _ <- ZIO.logDebug(s"Inserted ${countC.sum} constellations, ${countR.sum} regions, ${countS.sum} solar systems")
    yield ()

  private def insertMap(mapId: MapId) =
    query.map.upsertMap(
      MapModel(mapId, s"test-${mapId}", MapDisplayType.Manual, Instant.EPOCH, UserId.Invalid, None, None)
    )

  private def runMultiple(
      mapId: MapId,
      initialState: MapState,
      messages: Chunk[Identified[MapRequest]]
  ): URIO[MapEnv, (MapState, Chunk[Identified[MapResponse]])] =
    ZIO.foldLeft(messages)((initialState, Chunk.empty[Identified[MapResponse]])):
      case ((state, responses), req) => MapEntity.handle(mapId, state, req).map((s, r) => (s, responses ++ r))

  private def updateRefData(state: MapState, solarSystems: Map[SystemId, MapSolarSystem]) =
    state.copy(ref = MapRef(solarSystems = solarSystems))

  private inline def matches[A](pf: PartialFunction[A, Boolean]): Assertion[A] =
    Assertion[A](TestArrow.fromFunction(a => pf.unapply(a).getOrElse(false)))
