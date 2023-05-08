package org.updraft0.controltower.protocol

import org.updraft0.controltower.protocol.jsoncodec.given
import zio.json.*
import zio.json.ast.Json
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
          corporationId = 1000004L,
          corporationName = "Ytiri"
        )

        val json = Json.Obj(
          "id"              -> Json.Num(60000361),
          "name"            -> Json.Str("Jita IV - Moon 6 - Ytiri Storage"),
          "typeId"          -> Json.Num(1531),
          "operationId"     -> Json.Num(26),
          "factionId"       -> Json.Num(500001),
          "factionName"     -> Json.Str("Caldari State"),
          "corporationId"   -> Json.Num(1000004),
          "corporationName" -> Json.Str("Ytiri")
        )

        assertTrue(
          json.as[Station] == Right(value),
          value.toJsonAST == Right(json)
        )
      }
    )
