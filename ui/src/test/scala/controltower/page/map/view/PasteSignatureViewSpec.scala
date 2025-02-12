package controltower.page.map.view

import zio.test.*

object PasteSignatureViewSpec extends ZIOSpecDefault:

  override def spec =
    suite("Signature parsing")(
      test("Works on all signature types"):
        val clipboard =
          """
            |WZB-024	Cosmic Signature	Combat Site		0.0%	32.13 AU
            |PIA-435	Cosmic Signature			0.0%	28.15 AU
            |JEY-515	Cosmic Signature	Data Site		0.0%	19.06 AU
            |THS-490	Cosmic Signature	Wormhole	Unstable Wormhole	100.0%	4.94 AU
            |ULL-880	Cosmic Signature	Gas Site		0.0%	9.93 AU
            |WFD-000	Cosmic Signature	Relic Site	Forgotten Frontier Evacuation Center	0.0%	9.93 AU
            |IXC-218	Ship	Mining Barge	Covetor	100.0%	4,710 km
            |JLH-924	Cosmic Anomaly	Ore Site	Ordinary Perimeter Deposit	100.0%	38.53 AU
            |""".stripMargin
        val expected = List(
          ParsedLine("WZB-024", "Cosmic Signature", "Combat Site", "", "0.0%", "32.13 AU"),
          ParsedLine("PIA-435", "Cosmic Signature", "", "", "0.0%", "28.15 AU"),
          ParsedLine("JEY-515", "Cosmic Signature", "Data Site", "", "0.0%", "19.06 AU"),
          ParsedLine("THS-490", "Cosmic Signature", "Wormhole", "Unstable Wormhole", "100.0%", "4.94 AU"),
          ParsedLine("ULL-880", "Cosmic Signature", "Gas Site", "", "0.0%", "9.93 AU"),
          ParsedLine(
            "WFD-000",
            "Cosmic Signature",
            "Relic Site",
            "Forgotten Frontier Evacuation Center",
            "0.0%",
            "9.93 AU"
          ),
          ParsedLine("IXC-218", "Ship", "Mining Barge", "Covetor", "100.0%", "4,710 km"),
          ParsedLine("JLH-924", "Cosmic Anomaly", "Ore Site", "Ordinary Perimeter Deposit", "100.0%", "38.53 AU")
        )
        val res = parseLines(clipboard)
        assertTrue(res == Right(expected))
    )
