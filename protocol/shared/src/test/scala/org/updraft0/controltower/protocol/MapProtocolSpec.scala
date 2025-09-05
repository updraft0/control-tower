package org.updraft0.controltower.protocol

import org.updraft0.json.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.jsoncodec.given
import zio.test.*

import scala.language.strictEquality

import com.github.plokhotnyuk.jsoniter_scala.core.*

object MapProtocolSpec extends ZIOSpecDefault:
  def spec =
    suite("map protocol")(
      test("can reset system name to be unset"):
        val value: MapRequest = MapRequest.UpdateSystem(SystemId(1), name = Some(None))
        val json              = Json.Obj(
          "UpdateSystem" -> Json.Obj(
            "system_id" -> Json.Num(1),
            "name"      -> Json.Str("None")
          )
        )

        val res = json.as[MapRequest]

        assertTrue(
          res == Right(value),
          value.toJsonAST == json
        )
      ,
      test("can add an intel ping"):
        val value: MapRequest =
          MapRequest.AddIntelSystemPing(SystemId(1), pingTarget = IntelSystemPingTarget.Map, pingNote = Some("Test"))

        val json = Json.Obj(
          "AddIntelSystemPing" -> Json.Obj(
            "system_id"   -> Json.Num(1),
            "ping_target" -> Json.Str("Map"),
            "ping_note"   -> Json.Str("Test")
          )
        )

        val res = json.as[MapRequest]

        assertTrue(
          res == Right(value),
          value.toJsonAST == json
        )
    )
