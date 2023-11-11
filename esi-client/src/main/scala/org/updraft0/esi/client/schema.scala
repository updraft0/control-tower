package org.updraft0.esi.client

import sttp.tapir.*
import sttp.tapir.SchemaType.SString
import sttp.tapir.generic.Configuration
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

import java.util.Base64

object schema:
  given Configuration = Configuration.default.withSnakeCaseMemberNames

  // auth
  given Schema[JwtAuthResponse]   = Schema.derived
  given Schema[AuthErrorResponse] = Schema.derived
  given Schema[JwtString]         = Schema(SString().as[JwtString])

  // character
  given Schema[CharacterRoles] = Schema.derived
  given Schema[Character] = Schema.derived
  given Schema[CharacterAffiliation] = Schema.derived

object jsoncodec:
  // debug print codecs
  // given CodecMakerConfig.PrintCodec with {}

  private inline def config = CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case)

  given JsonValueCodec[JwtString] = new JsonValueCodec[JwtString] {
    override def decodeValue(in: JsonReader, default: JwtString): JwtString =
      val str = in.readString("")
      JwtString(str)

    override def encodeValue(x: JwtString, out: JsonWriter): Unit = out.writeNonEscapedAsciiVal(x.value)

    override def nullValue: JwtString = null
  }

  given JsonValueCodec[JwtAuthResponse]   = JsonCodecMaker.make(config)
  given JsonValueCodec[AuthErrorResponse] = JsonCodecMaker.make(config)


  // character
  given JsonValueCodec[CharacterRoles] = JsonCodecMaker.make(config)
  given JsonValueCodec[Character] = JsonCodecMaker.make(config)
  given listOfLong: JsonValueCodec[List[Long]] = JsonCodecMaker.make
  given JsonValueCodec[List[CharacterAffiliation]] = JsonCodecMaker.make(config)
