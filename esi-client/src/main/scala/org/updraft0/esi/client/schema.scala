package org.updraft0.esi.client

import org.updraft0.controltower.constant.*
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import sttp.tapir.*
import sttp.tapir.SchemaType.SString
import sttp.tapir.generic.Configuration

object schema:
  given Configuration = Configuration.default.withSnakeCaseMemberNames

  // auth
  given Schema[JwtAuthResponse]   = Schema.derived
  given Schema[AuthErrorResponse] = Schema.derived
  given Schema[JwtString]         = Schema(SString().as[JwtString])

  // character
  given Schema[CharacterRoles]            = Schema.derived
  given Schema[Character]                 = Schema.derived
  given Schema[CharacterAffiliation]      = Schema.derived
  given Schema[CharacterFleetResponse]    = Schema.derived
  given Schema[CharacterLocationResponse] = Schema.derived
  given Schema[CharacterOnlineResponse]   = Schema.derived
  given Schema[CharacterShipResponse]     = Schema.derived

  // error
  given Schema[EsiError.BadRequest]          = Schema.derived
  given Schema[EsiError.Unauthorized]        = Schema.derived
  given Schema[EsiError.Timeout]             = Schema.derived
  given Schema[EsiError.ServiceUnavailable]  = Schema.derived
  given Schema[EsiError.RateLimited]         = Schema.derived
  given Schema[EsiError.InternalServerError] = Schema.derived
  given Schema[EsiError.NotFound]            = Schema.derived
  given Schema[EsiError.Forbidden]           = Schema.derived

  // status
  given Schema[ServerStatusResponse] = Schema.derived

object jsoncodec:
  // debug print codecs
  // given CodecMakerConfig.PrintCodec with {}

  private inline def config = CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case)

  given JsonValueCodec[CharacterId] = new JsonValueCodec[CharacterId]:
    override def decodeValue(in: JsonReader, default: CharacterId): CharacterId = CharacterId(in.readLong())
    override def encodeValue(x: CharacterId, out: JsonWriter): Unit             = out.writeVal(x.value)
    override def nullValue: CharacterId                                         = CharacterId(-1)

  given JsonValueCodec[AllianceId] = new JsonValueCodec[AllianceId]:
    override def decodeValue(in: JsonReader, default: AllianceId): AllianceId = AllianceId(in.readLong())
    override def encodeValue(x: AllianceId, out: JsonWriter): Unit            = out.writeVal(x.value)
    override def nullValue: AllianceId                                        = AllianceId(-1)

  given JsonValueCodec[CorporationId] = new JsonValueCodec[CorporationId]:
    override def decodeValue(in: JsonReader, default: CorporationId): CorporationId = CorporationId(in.readLong())
    override def encodeValue(x: CorporationId, out: JsonWriter): Unit               = out.writeVal(x.value)
    override def nullValue: CorporationId                                           = CorporationId(-1)

  given JsonValueCodec[SystemId] = new JsonValueCodec[SystemId]:
    override def decodeValue(in: JsonReader, default: SystemId): SystemId = SystemId(in.readLong())
    override def encodeValue(x: SystemId, out: JsonWriter): Unit          = out.writeVal(x.value)
    override def nullValue: SystemId                                      = SystemId(-1)

  given JsonValueCodec[JwtString] = new JsonValueCodec[JwtString]:
    override def decodeValue(in: JsonReader, default: JwtString): JwtString =
      val str = in.readString("")
      JwtString(str)
    override def encodeValue(x: JwtString, out: JsonWriter): Unit = out.writeNonEscapedAsciiVal(x.value)
    override def nullValue: JwtString                             = null

  given JsonValueCodec[JwtAuthResponse]   = JsonCodecMaker.make(config)
  given JsonValueCodec[AuthErrorResponse] = JsonCodecMaker.make(config)

  given JsonValueCodec[EsiError.BadRequest]          = JsonCodecMaker.make(config)
  given JsonValueCodec[EsiError.Timeout]             = JsonCodecMaker.make(config)
  given JsonValueCodec[EsiError.ServiceUnavailable]  = JsonCodecMaker.make(config)
  given JsonValueCodec[EsiError.InternalServerError] = JsonCodecMaker.make(config)
  given JsonValueCodec[EsiError.RateLimited]         = JsonCodecMaker.make(config)
  given JsonValueCodec[EsiError.Unauthorized]        = JsonCodecMaker.make(config)
  given JsonValueCodec[EsiError.NotFound]            = JsonCodecMaker.make(config)
  given JsonValueCodec[EsiError.Forbidden]           = JsonCodecMaker.make(config)

  // character
  given JsonValueCodec[CharacterRoles]         = JsonCodecMaker.make(config)
  given JsonValueCodec[Character]              = JsonCodecMaker.make(config)
  given listOfLong: JsonValueCodec[List[Long]] = JsonCodecMaker.make
  given listOfCharacterId: JsonValueCodec[List[CharacterId]] =
    listOfLong.asInstanceOf[JsonValueCodec[List[CharacterId]]]
  given JsonValueCodec[List[CharacterAffiliation]] = JsonCodecMaker.make(config)
  given JsonValueCodec[CharacterFleetResponse]     = JsonCodecMaker.make(config)
  given JsonValueCodec[CharacterLocationResponse]  = JsonCodecMaker.make(config)
  given JsonValueCodec[CharacterOnlineResponse]    = JsonCodecMaker.make(config)
  given JsonValueCodec[CharacterShipResponse]      = JsonCodecMaker.make(config)

  // status
  given JsonValueCodec[ServerStatusResponse] = JsonCodecMaker.make(config)
