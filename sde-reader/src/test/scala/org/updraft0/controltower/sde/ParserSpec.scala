package org.updraft0.controltower.sde

import org.updraft0.controltower.sde.yaml.YAML
import zio.test.*

object ParserSpec extends ZIOSpecDefault {
  def spec =
    suite("Static Data Parser")(
      test("can parse a region") {
        val yamlString = """
            |center:
            |- -1.546206305243694e+17
            |- 7.859497411041216e+16
            |- 1.0482179762946699e+17
            |descriptionID: 267745
            |factionID: 500001
            |max:
            |- -1.2125016614624125e+17
            |- 1.0985765363671123e+17
            |- -6.076699997623242e+16
            |min:
            |- -1.8799109490249757e+17
            |- 4.7332294584113096e+16
            |- -1.4887659528270157e+17
            |nameID: 267744
            |nebula: 11805
            |regionID: 10000033
            |wormholeClassID: 7
            |""".stripMargin

        for
          yamlObj <- SDEParser.parseYaml[String](yamlString)
          region  <- SDEParser.parseRegion("a-region", yamlObj)
        yield assertTrue(
          region == Region(
            id = 10000033,
            nameId = 267744,
            name = "a-region",
            wormholeClass = Some(WormholeClass.Hi),
            factionId = Some(500001)
          )
        )
      },
      test("can parse a constellation") {
        val yaml = """
                     |center:
                     |- 7.71835779913189e+18
                     |- 2.324591951478377e+16
                     |- -9.35918520229382e+18
                     |constellationID: 21000017
                     |max:
                     |- 7.712917410607478e+18
                     |- 7818406172916770.0
                     |- 9.37042853623133e+18
                     |min:
                     |- 7.697308190366799e+18
                     |- -7790814067761380.0
                     |- 9.35481931599065e+18
                     |nameID: 268650
                     |radius: 7804610120339075.0
                     |""".stripMargin

        for
          yamlObj       <- SDEParser.parseYaml[String](yaml)
          constellation <- SDEParser.parseConstellation("a-region", "a-constellation", yamlObj)
        yield assertTrue(
          constellation == Constellation(
            id = 21000017,
            nameId = 268650,
            name = "a-constellation",
            region = "a-region"
          )
        )
      },
      test("can parse a solar system") {
        val yaml = """
            |border: true
            |center:
            |  - -8.513208811988198e+16
            |  - 8.381088189943238e+16
            |  - 1.3675173242587931e+17
            |corridor: false
            |fringe: false
            |hub: true
            |international: false
            |luminosity: 0.1019
            |max:
            |  - -8.513134605730618e+16
            |  - 8.38109148595377e+16
            |  - -1.3675047319042981e+17
            |min:
            |  - -8.513463514505675e+16
            |  - 8.381087229613197e+16
            |  - -1.367552764282794e+17
            |planets:
            |  40012324:
            |    celestialIndex: 1
            |    planetAttributes:
            |      heightMap1: 3841
            |      heightMap2: 3841
            |      population: false
            |      shaderPreset: 4331
            |    position:
            |      - -39175346041.0
            |      - 506946900.0
            |      - -47012567609.0
            |    radius: 2850000
            |    statistics:
            |      density: 3760.6646498806
            |      eccentricity: 0.070329
            |      escapeVelocity: 4132.74565069089
            |      fragmented: false
            |      life: 0.0
            |      locked: true
            |      massDust: 3.64659722456338e+23
            |      massGas: 1.59518e+16
            |      orbitPeriod: 20061000.0
            |      orbitRadius: 61197600000.0
            |      pressure: 0.0451114
            |      radius: 2850000.0
            |      rotationRate: 20061000.0
            |      spectralClass: '0.0'
            |      surfaceGravity: 2.99641870408851
            |      temperature: 247.612
            |    typeID: 2016
            |  40012331:
            |    asteroidBelts:
            |      40012332:
            |        position:
            |          - -240601374720.0
            |          - 3113902080.0
            |          - -403080683520.0
            |        statistics:
            |          density: 497.053
            |          eccentricity: 0.0301465
            |          escapeVelocity: 56.5557
            |          fragmented: false
            |          life: 0.0
            |          locked: true
            |          massDust: 2.56813e+18
            |          massGas: 298895.0
            |          orbitPeriod: 36231.9
            |          orbitRadius: 63125700.0
            |          pressure: 3.08394e-12
            |          radius: 107149.0
            |          rotationRate: 36231.9
            |          spectralClass: '0.0'
            |          surfaceGravity: 0.0148859
            |          temperature: 89.4062
            |        typeID: 15
            |      40012334:
            |        position:
            |          - -240710000640.0
            |          - 3113902080.0
            |          - -403027599360.0
            |        statistics:
            |          density: 480.614
            |          eccentricity: 0.00029699
            |          escapeVelocity: 51.415
            |          fragmented: false
            |          life: 0.0
            |          locked: true
            |          massDust: 1.96228e+18
            |          massGas: 1009970.0
            |          orbitPeriod: 79058.2
            |          orbitRadius: 106197000.0
            |          pressure: 1.08986e-11
            |          radius: 99061.6
            |          rotationRate: 79058.2
            |          spectralClass: '0.0'
            |          surfaceGravity: 0.0133071
            |          temperature: 89.4062
            |        typeID: 15
            |    celestialIndex: 5
            |    moons:
            |      40012333:
            |        planetAttributes:
            |          heightMap1: 3907
            |          heightMap2: 3907
            |          population: false
            |          shaderPreset: 4220
            |        position:
            |          - -240708510863.0
            |          - 3114878252.0
            |          - -402994706902.0
            |        radius: 200000
            |        statistics:
            |          density: 660.381
            |          eccentricity: 0.087106
            |          escapeVelocity: 126.499
            |          fragmented: false
            |          life: 0.0
            |          locked: true
            |          massDust: 2.49315e+19
            |          massGas: 69777200.0
            |          orbitPeriod: 60260.3
            |          orbitRadius: 88613500.0
            |          pressure: 4.92924e-10
            |          radius: 207923.0
            |          rotationRate: 60260.3
            |          spectralClass: '0.0'
            |          surfaceGravity: 0.0383777
            |          temperature: 89.4062
            |        typeID: 14
            |      40012335:
            |        planetAttributes:
            |          heightMap1: 3903
            |          heightMap2: 3907
            |          population: false
            |          shaderPreset: 4214
            |        position:
            |          - -240494436540.0
            |          - 3112108033.0
            |          - -403034587901.0
            |        radius: 260000
            |        statistics:
            |          density: 736.887
            |          eccentricity: 0.0772752
            |          escapeVelocity: 172.572
            |          fragmented: false
            |          life: 0.0
            |          locked: true
            |          massDust: 5.99236e+19
            |          massGas: 630897000.0
            |          orbitPeriod: 107544.0
            |          orbitRadius: 130377000.0
            |          pressure: 3.85079e-09
            |          radius: 268524.0
            |          rotationRate: 107544.0
            |          spectralClass: '0.0'
            |          surfaceGravity: 0.0553053
            |          temperature: 89.4062
            |        typeID: 14
            |    planetAttributes:
            |      heightMap1: 3910
            |      heightMap2: 3914
            |      population: false
            |      shaderPreset: 3993
            |    position:
            |      - -240624185507.0
            |      - 3113787044.0
            |      - -403021917120.0
            |    radius: 24210000
            |    statistics:
            |      density: 1338.14983349291
            |      eccentricity: 0.0178412
            |      escapeVelocity: 20941.5428720949
            |      fragmented: false
            |      life: 0.0
            |      locked: false
            |      massDust: 7.95385596235274e+25
            |      massGas: 8.15197e+25
            |      orbitPeriod: 426152000.0
            |      orbitRadius: 469400000000.0
            |      pressure: 14244400.0
            |      radius: 24210000.0
            |      rotationRate: 40950.8
            |      spectralClass: '0.0'
            |      surfaceGravity: 9.05717095959911
            |      temperature: 89.4062
            |    typeID: 13
            |  40012349:
            |    asteroidBelts:
            |      40012351:
            |        position:
            |          - -645547991040.0
            |          - 8355471360.0
            |          - 1449576898560.0
            |        statistics:
            |          density: 339.394
            |          eccentricity: 0.027823
            |          escapeVelocity: 43.2191
            |          fragmented: false
            |          life: 0.0
            |          locked: true
            |          massDust: 1.38696e+18
            |          massGas: 168412.0
            |          orbitPeriod: 82800.5
            |          orbitRadius: 153063000.0
            |          pressure: 1.28296e-12
            |          radius: 99091.6
            |          rotationRate: 82800.5
            |          spectralClass: '0.0'
            |          surfaceGravity: 0.00939991
            |          temperature: 48.6248
            |        typeID: 15
            |      40012352:
            |        position:
            |          - -645815377920.0
            |          - 8355471360.0
            |          - 1449615237120.0
            |        statistics:
            |          density: 379.408
            |          eccentricity: 0.00777079
            |          escapeVelocity: 59.2673
            |          fragmented: false
            |          life: 0.0
            |          locked: true
            |          massDust: 3.38284e+18
            |          massGas: 1555220.0
            |          orbitPeriod: 103717.0
            |          orbitRadius: 177862000.0
            |          pressure: 1.02116e-11
            |          radius: 128521.0
            |          rotationRate: 103717.0
            |          spectralClass: '0.0'
            |          surfaceGravity: 0.013629
            |          temperature: 48.6248
            |        typeID: 15
            |    celestialIndex: 6
            |    moons:
            |      40012350:
            |        planetAttributes:
            |          heightMap1: 3907
            |          heightMap2: 3908
            |          population: false
            |          shaderPreset: 4222
            |        position:
            |          - -645770604320.0
            |          - 8356567054.0
            |          - 1449532975566.0
            |        radius: 270000
            |        statistics:
            |          density: 524.696
            |          eccentricity: 0.0354878
            |          escapeVelocity: 148.51
            |          fragmented: false
            |          life: 0.0
            |          locked: true
            |          massDust: 4.52582e+19
            |          massGas: 216599000.0
            |          orbitPeriod: 60038.6
            |          orbitRadius: 123538000.0
            |          pressure: 9.23047e-10
            |          radius: 273850.0
            |          rotationRate: 60038.6
            |          spectralClass: '0.0'
            |          surfaceGravity: 0.040161
            |          temperature: 48.6248
            |        typeID: 14
            |      40012354:
            |        planetAttributes:
            |          heightMap1: 3907
            |          heightMap2: 3907
            |          population: false
            |          shaderPreset: 4210
            |        position:
            |          - -645566976231.0
            |          - 8353932013.0
            |          - 1449830958284.0
            |        radius: 360000
            |        statistics:
            |          density: 595.945
            |          eccentricity: 0.0470371
            |          escapeVelocity: 213.026
            |          fragmented: false
            |          life: 0.0
            |          locked: true
            |          massDust: 1.25337e+20
            |          massGas: 1308310000.0
            |          orbitPeriod: 160683.0
            |          orbitRadius: 238138000.0
            |          pressure: 4.70487e-09
            |          radius: 368589.0
            |          rotationRate: 160683.0
            |          spectralClass: '0.0'
            |          surfaceGravity: 0.0613947
            |          temperature: 48.6248
            |        typeID: 14
            |      40012361:
            |        npcStations:
            |          60007585:
            |            graphicID: 20660
            |            isConquerable: false
            |            operationID: 1
            |            ownerID: 1000075
            |            position:
            |              - -644916633600.0
            |              - 8344903680.0
            |              - 1449925632000.0
            |            reprocessingEfficiency: 0.5
            |            reprocessingHangarFlag: 4
            |            reprocessingStationsTake: 0.05
            |            typeID: 1930
            |            useOperationName: true
            |        planetAttributes:
            |          heightMap1: 3907
            |          heightMap2: 3907
            |          population: false
            |          shaderPreset: 4207
            |        position:
            |          - -644916416574.0
            |          - 8345513474.0
            |          - 1449927763000.0
            |        radius: 900000
            |        statistics:
            |          density: 877.021
            |          eccentricity: 0.0195682
            |          escapeVelocity: 636.615
            |          fragmented: false
            |          life: 0.0
            |          locked: true
            |          massDust: 2.75749e+21
            |          massGas: 1088330000000.0
            |          orbitPeriod: 1048370.0
            |          orbitRadius: 831492000.0
            |          pressure: 2.33807e-06
            |          radius: 907998.0
            |          rotationRate: 1048370.0
            |          spectralClass: '0.0'
            |          surfaceGravity: 0.222576
            |          temperature: 48.6248
            |        typeID: 14
            |    planetAttributes:
            |      heightMap1: 3911
            |      heightMap2: 3913
            |      population: false
            |      shaderPreset: 3966
            |    position:
            |      - -645692052423.0
            |      - 8355550556.0
            |      - 1449628318126.0
            |    radius: 33010000
            |    statistics:
            |      density: 2048.04
            |      eccentricity: 0.0203545
            |      escapeVelocity: 35369.6
            |      fragmented: false
            |      life: 0.0
            |      locked: false
            |      massDust: 3.094623e+26
            |      massGas: 2.74622e+26
            |      orbitPeriod: 2649080000.0
            |      orbitRadius: 1586950000000.0
            |      pressure: 37894400.0
            |      radius: 33010000.0
            |      rotationRate: 33790.3
            |      spectralClass: '0.0'
            |      surfaceGravity: 18.8971
            |      temperature: 48.6248
            |    typeID: 13
            |radius: 2401618924792.0
            |regional: false
            |security: 0.38439987604487
            |securityClass: C1
            |solarSystemID: 30000194
            |solarSystemNameID: 269151
            |star:
            |  id: 40012323
            |  radius: 147250000
            |  statistics:
            |    age: 2.132e+17
            |    life: 6.104e+17
            |    locked: false
            |    luminosity: 0.1019
            |    radius: 147250000.0
            |    spectralClass: K1 V
            |    temperature: 4777.0
            |  typeID: 45041
            |stargates:
            |  50010753:
            |    destination: 50008925
            |    position:
            |      - -2545810513920.0
            |      - 32924098560.0
            |      - -1255776706560.0
            |    typeID: 16
            |  50013692:
            |    destination: 50013691
            |    position:
            |      - 1992600576000.0
            |      - 2643522232320.0
            |      - -2212887060480.0
            |    typeID: 29627
            |sunTypeID: 45041
            |wormholeClassID: 8
            |""".stripMargin

        for
          yamlObj     <- SDEParser.parseYaml[String](yaml)
          solarSystem <- SDEParser.parseSolarSystem("a-region", "a-constellation", "a-system", yamlObj)
        yield assertTrue(
          solarSystem == SolarSystem(
            name = "a-system",
            constellation = "a-constellation",
            region = "a-region",
            id = 30000194,
            nameId = 269151,
            star = Some(
              Star(
                id = 40012323,
                typeId = 45041
              )
            ),
            secondaryEffect = None,
            planets = Vector(
              Planet(
                id = 40012324,
                index = 1,
                typeId = 2016,
                moons = Vector.empty,
                asteroidBelts = Vector.empty
              ),
              Planet(
                id = 40012331,
                index = 5,
                typeId = 13,
                moons = Vector(
                  PlanetMoon(id = 40012333, npcStations = Vector.empty),
                  PlanetMoon(id = 40012335, npcStations = Vector.empty)
                ),
                asteroidBelts = Vector(
                  PlanetAsteroidBelt(id = 40012332),
                  PlanetAsteroidBelt(id = 40012334)
                )
              ),
              Planet(
                id = 40012349,
                index = 6,
                typeId = 13,
                moons = Vector(
                  PlanetMoon(id = 40012350, npcStations = Vector.empty),
                  PlanetMoon(id = 40012354, npcStations = Vector.empty),
                  PlanetMoon(
                    id = 40012361,
                    npcStations = Vector(
                      NpcStation(id = 60007585, ownerId = 1000075, typeId = 1930)
                    )
                  )
                ),
                asteroidBelts = Vector(
                  PlanetAsteroidBelt(id = 40012351),
                  PlanetAsteroidBelt(id = 40012352)
                )
              )
            ),
            stargates = Vector(
              Stargate(
                id = 50010753,
                destinationId = 50008925
              ),
              Stargate(
                id = 50013692,
                destinationId = 50013691
              )
            ),
            securityClass = Some("C1"),
            security = Some(0.38439987604487),
            border = true,
            corridor = false,
            fringe = false,
            hub = true,
            international = false,
            regional = false
          )
        )
      },
      test("can parse station services") {
        val yaml = """
            |1:
            |  serviceNameID:
            |    en: Bounty Missions
            |2:
            |  serviceNameID:
            |    en: Assassination Missions
            |3:
            |  serviceNameID:
            |    en: Courier Missions
            |""".stripMargin

        for
          yamlObj         <- SDEParser.parseYaml[Integer](yaml)
          stationServices <- SDEParser.parseStationServices(yamlObj)
        yield assertTrue(
          stationServices == Vector(
            StationService(id = 1, nameEn = "Bounty Missions"),
            StationService(id = 2, nameEn = "Assassination Missions"),
            StationService(id = 3, nameEn = "Courier Missions")
          )
        )
      },
      test("can parse item names") {
        val yaml = """
            |-   itemID: 0
            |    itemName: (none)
            |-   itemID: 1
            |    itemName: EVE System
            |""".stripMargin

        for
          yamlArr   <- SDEParser.parseYamlArray(yaml)
          itemNames <- SDEParser.parseItemNames(yamlArr)
        yield assertTrue(
          itemNames == Vector(
            ItemName(id = 0, name = "(none)"),
            ItemName(id = 1, name = "EVE System")
          )
        )
      }
    ).provideSomeShared(YAML.layer)

}
