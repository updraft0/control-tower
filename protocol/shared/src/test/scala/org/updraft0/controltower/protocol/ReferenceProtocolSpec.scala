package org.updraft0.controltower.protocol

import org.updraft0.controltower.protocol.jsoncodec.given
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object ReferenceProtocolSpec extends ZIOSpecDefault:
  def spec =
    suite("reference protocol")(
      test("can encode/decode StationOperation") {
        val value = StationOperation(
          operationId = 42,
          operationName = "Law School",
          services = Array(
            StationService(id = 5, name = "Reprocessing Plant"),
            StationService(id = 9, name = "Stock Exchange")
          )
        )
        val json = Json.Obj(
          "operationId"   -> Json.Num(42),
          "operationName" -> Json.Str("Law School"),
          "services" -> Json.Arr(
            Json.Obj("id" -> Json.Num(5), "name" -> Json.Str("Reprocessing Plant")),
            Json.Obj("id" -> Json.Num(9), "name" -> Json.Str("Stock Exchange"))
          )
        )

        assertTrue(
          json.as[StationOperation] == Right(value),
          value.toJsonAST == Right(json)
        )
      }
    )
