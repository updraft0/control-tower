package org.updraft0.controltower.protocol

import org.updraft0.json.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.jsoncodec.given
import zio.test.*

object SystemProtocolSpec extends ZIOSpecDefault:
  def spec =
    suite("system protocol")(
      test("can encode/decode Station") {
        val value = Station(
          id = 60000361,
          name = "Jita IV - Moon 6 - Ytiri Storage",
          typeId = 1531,
          operationId = 26,
          factionId = Some(500001L),
          factionName = Some("Caldari State"),
          corporationId = CorporationId(1000004L),
          corporationName = "Ytiri"
        )

        val json = Json.Obj(
          "id"               -> Json.Num(60000361),
          "name"             -> Json.Str("Jita IV - Moon 6 - Ytiri Storage"),
          "type_id"          -> Json.Num(1531),
          "operation_id"     -> Json.Num(26),
          "faction_id"       -> Json.Num(500001),
          "faction_name"     -> Json.Str("Caldari State"),
          "corporation_id"   -> Json.Num(1000004),
          "corporation_name" -> Json.Str("Ytiri")
        )

        assertTrue(
          json.as[Station] == Right(value),
          value.toJsonAST == json
        )
      }
    )
