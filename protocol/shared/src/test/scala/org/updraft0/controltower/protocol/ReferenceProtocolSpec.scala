package org.updraft0.controltower.protocol

import org.updraft0.json.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.jsoncodec.given
import zio.test.*

import scala.language.strictEquality

given CanEqual[StationService, StationService] = CanEqual.derived

object ReferenceProtocolSpec extends ZIOSpecDefault:
  def spec =
    suite("reference protocol")(
      test("can encode/decode StationOperation"):
        val value = StationOperation(
          operationId = 42,
          operationName = "Law School",
          services = Array(
            StationService(id = 5, name = "Reprocessing Plant"),
            StationService(id = 9, name = "Stock Exchange")
          )
        )
        val json = Json.Obj(
          "operation_id"   -> Json.Num(42),
          "operation_name" -> Json.Str("Law School"),
          "services" -> Json.Arr(
            Json.Obj("id" -> Json.Num(5), "name" -> Json.Str("Reprocessing Plant")),
            Json.Obj("id" -> Json.Num(9), "name" -> Json.Str("Stock Exchange"))
          )
        )

        val res = json.as[StationOperation]

        assertTrue(
          res.map(_.operationId) == Right(42),
          res.map(_.operationName) == Right("Law School"),
          res.map(_.services.toList) == Right(
            List(StationService(5, "Reprocessing Plant"), StationService(9, "Stock Exchange"))
          ),
          value.toJsonAST == json
        )
    )
