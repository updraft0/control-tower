package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.*
export org.updraft0.controltower.constant.SigId
export org.updraft0.controltower.constant.SigId.given
export org.updraft0.controltower.constant.SystemId.given
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import sttp.tapir.*
import sttp.tapir.SchemaType.SInteger
import sttp.tapir.generic.Configuration

object schema:
  given Configuration = Configuration.default.withSnakeCaseMemberNames

  given longMapSchema[V: Schema]: Schema[Map[Long, V]]               = Schema.schemaForMap[Long, V](_.toString)
  given characterMapSchema[V: Schema]: Schema[Map[CharacterId, V]]   = Schema.schemaForMap[CharacterId, V](_.toString)
  given connectionMapSchema[V: Schema]: Schema[Map[ConnectionId, V]] = Schema.schemaForMap[ConnectionId, V](_.toString)
  given systemMapSchema[V: Schema]: Schema[Map[SystemId, V]]         = Schema.schemaForMap[SystemId, V](_.toString)

  given unknownOrUnsetSchema[A: Schema]: Schema[UnknownOrUnset[A]] = Schema.derived

  // opaque
  given Schema[CharacterId]   = Schema(SInteger()).format("int64")
  given Schema[CorporationId] = Schema(SInteger()).format("int64")
  given Schema[AllianceId]    = Schema(SInteger()).format("int64")
  given Schema[SigId]         = Schema.string
  given Schema[SystemId]      = Schema(SInteger()).format("int64")
  given Schema[UserId]        = Schema(SInteger()).format("int64")
  given Schema[MapId]         = Schema(SInteger()).format("int64")
  given Schema[ConnectionId]  = Schema(SInteger()).format("int64")

  // auth

  // constant
  given Schema[SpaceType]      = Schema.derived
  given Schema[WormholeClass]  = Schema(SInteger[WormholeClass]()).format("int32")
  given Schema[WormholeEffect] = Schema(SInteger[WormholeEffect]()).format("int64")

  // reference
  given Schema[Faction]          = Schema.derived
  given Schema[ShipType]         = Schema.derived
  given Schema[StarType]         = Schema.derived
  given Schema[StationService]   = Schema.derived
  given Schema[StationOperation] = Schema.derived
  given Schema[WormholeType]     = Schema.derived
  given Schema[SignatureInGroup] = Schema.derived

  given Schema[Reference]             = Schema.derived
  given Schema[ReferenceSolarSystems] = Schema.derived
  given Schema[ReferenceVersion]      = Schema.derived

  // system
  given Schema[Planet]         = Schema.derived
  given Schema[Station]        = Schema.derived
  given Schema[Stargate]       = Schema.derived
  given Schema[SolarSystem]    = Schema.derived
  given Schema[WormholeStatic] = Schema.derived

  // user
  given Schema[MapRole]              = Schema.derived
  given Schema[UserCharacter]        = Schema.derived
  given Schema[UserCharacterMap]     = Schema.derived
  given Schema[UserInfo]             = Schema.derived
  given Schema[MapPreferences]       = Schema.derived
  given Schema[SignaturePreferences] = Schema.derived
  given Schema[UserPreferences]      = Schema.derived

  // map
  given Schema[MapInfo]                = Schema.derived
  given Schema[MapInfoWithPermissions] = Schema.derived
  given Schema[MapPolicyMember]        = Schema.derived
  given Schema[MapSettings]            = Schema.derived
  given Schema[NewMap]                 = Schema.derived
  given Schema[PolicyMemberType]       = Schema.derived
  given Schema[MapDisplayType]         = Schema.derived
  given Schema[CharacterLocation]      = Schema.derived

  given Schema[IntelStance]                   = Schema.derived
  given Schema[MapRequest]                    = Schema.derived
  given Schema[MapMessage]                    = Schema.derived
  given Schema[MapSystem]                     = Schema.derived
  given Schema[SystemDisplayData]             = Schema.derived
  given Schema[Corporation]                   = Schema.derived
  given Schema[MapSystemStructure]            = Schema.derived
  given Schema[MapSystemNote]                 = Schema.derived
  given Schema[MapWormholeConnection]         = Schema.derived
  given Schema[MapWormholeConnectionJump]     = Schema.derived
  given Schema[MapWormholeConnectionRank]     = Schema.derived
  given Schema[MapWormholeConnectionWithSigs] = Schema.derived
  given Schema[SignatureGroup]                = Schema.derived
  given Schema[WormholeMassSize]              = Schema.derived
  given Schema[WormholeMassStatus]            = Schema.derived
  given Schema[WormholeK162Type]              = Schema.derived
  given Schema[WormholeConnectionType]        = Schema.derived
  given Schema[MapSystemSignature]            = Schema.derived
  given Schema[MapSystemSignature.Wormhole]   = Schema.derived
  given Schema[MapSystemSnapshot]             = Schema.derived
  given Schema[MapServerStatus]               = Schema.derived
  given Schema[NewSystemSignature]            = Schema.derived

trait OpaqueCodecs:
  // currently, jsoniter is unable to derive codecs for opaque types

  given JsonValueCodec[CharacterId] = new JsonValueCodec[CharacterId]:
    override def decodeValue(in: JsonReader, default: CharacterId): CharacterId = CharacterId(in.readLong())
    override def encodeValue(x: CharacterId, out: JsonWriter): Unit             = out.writeVal(x.value)
    override def nullValue: CharacterId                                         = CharacterId.Invalid

  given JsonValueCodec[AllianceId] = new JsonValueCodec[AllianceId]:
    override def decodeValue(in: JsonReader, default: AllianceId): AllianceId = AllianceId(in.readLong())
    override def encodeValue(x: AllianceId, out: JsonWriter): Unit            = out.writeVal(x.value)
    override def nullValue: AllianceId                                        = AllianceId(-1)

  given JsonValueCodec[CorporationId] = new JsonValueCodec[CorporationId]:
    override def decodeValue(in: JsonReader, default: CorporationId): CorporationId = CorporationId(in.readLong())
    override def encodeValue(x: CorporationId, out: JsonWriter): Unit               = out.writeVal(x.value)
    override def nullValue: CorporationId                                           = CorporationId(-1)

  given JsonCodec[SystemId] = new JsonCodec[SystemId]:
    override def decodeKey(in: JsonReader): SystemId                      = SystemId(in.readKeyAsLong())
    override def encodeKey(x: SystemId, out: JsonWriter): Unit            = out.writeKey(x.value)
    override def decodeValue(in: JsonReader, default: SystemId): SystemId = SystemId(in.readLong())
    override def encodeValue(x: SystemId, out: JsonWriter): Unit          = out.writeVal(x.value)
    override def nullValue: SystemId                                      = SystemId(-1)

  given JsonValueCodec[MapId] = new JsonValueCodec[MapId]:
    override def decodeValue(in: JsonReader, default: MapId): MapId = MapId(in.readLong())
    override def encodeValue(x: MapId, out: JsonWriter): Unit       = out.writeVal(x.value)
    override def nullValue: MapId                                   = MapId.Invalid

  given JsonCodec[ConnectionId] = new JsonCodec[ConnectionId]:
    override def decodeKey(in: JsonReader): ConnectionId                          = ConnectionId(in.readKeyAsLong())
    override def encodeKey(x: ConnectionId, out: JsonWriter): Unit                = out.writeKey(x.value)
    override def decodeValue(in: JsonReader, default: ConnectionId): ConnectionId = ConnectionId(in.readLong())
    override def encodeValue(x: ConnectionId, out: JsonWriter): Unit              = out.writeVal(x.value)
    override def nullValue: ConnectionId                                          = ConnectionId.Invalid

  given JsonValueCodec[SigId] = new JsonValueCodec[SigId]:
    override def decodeValue(in: JsonReader, default: SigId): SigId = SigId(in.readString(null))
    override def encodeValue(x: SigId, out: JsonWriter): Unit       = out.writeVal(x.value)
    override def nullValue: SigId                                   = null.asInstanceOf[SigId]

  given JsonValueCodec[UserId] = new JsonValueCodec[UserId]:
    override def decodeValue(in: JsonReader, default: UserId): UserId = UserId(in.readLong())
    override def encodeValue(x: UserId, out: JsonWriter): Unit        = out.writeVal(x.value)
    override def nullValue: UserId                                    = UserId.Invalid

object jsoncodec extends OpaqueCodecs:
  // debug print codecs
  // given CodecMakerConfig.PrintCodec with {}

  inline def config: CodecMakerConfig =
    CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case).withDiscriminatorFieldName(None)

  // aux
  given [A <: AnyRef: JsonValueCodec: scala.reflect.ClassTag]: JsonValueCodec[Array[A]] = JsonCodecMaker.make

  // oops this seems to work for *any* long array?
  given connectionIdArray: JsonValueCodec[Array[ConnectionId]] =
    JsonCodecMaker.make[Array[Long]].asInstanceOf[JsonValueCodec[Array[ConnectionId]]]
  given systemIdArray: JsonValueCodec[Array[SystemId]] =
    JsonCodecMaker.make[Array[Long]].asInstanceOf[JsonValueCodec[Array[SystemId]]]

  // constant
  given JsonValueCodec[SpaceType] = JsonCodecMaker.make(config)

  given JsonValueCodec[WormholeClass] = new JsonValueCodec[WormholeClass]:
    override def decodeValue(in: JsonReader, default: WormholeClass): WormholeClass = WormholeClasses.ById(in.readInt())
    override def encodeValue(x: WormholeClass, out: JsonWriter): Unit               = out.writeVal(x.value)
    override def nullValue: WormholeClass                                           = null

  given JsonValueCodec[WormholeEffect] = new JsonValueCodec[WormholeEffect]:
    override def decodeValue(in: JsonReader, default: WormholeEffect): WormholeEffect =
      WormholeEffects.ById(in.readLong())
    override def encodeValue(x: WormholeEffect, out: JsonWriter): Unit = out.writeVal(x.typeId)
    override def nullValue: WormholeEffect                             = null

  // reference
  given JsonValueCodec[Faction]          = JsonCodecMaker.make(config)
  given JsonValueCodec[ShipType]         = JsonCodecMaker.make(config)
  given JsonValueCodec[StarType]         = JsonCodecMaker.make(config)
  given JsonValueCodec[StationService]   = JsonCodecMaker.make(config)
  given JsonValueCodec[StationOperation] = JsonCodecMaker.make(config)
  given JsonValueCodec[WormholeType]     = JsonCodecMaker.make(config)
  given JsonValueCodec[SignatureInGroup] = JsonCodecMaker.make(config)

  given JsonValueCodec[Reference]             = JsonCodecMaker.make(config)
  given JsonValueCodec[ReferenceSolarSystems] = JsonCodecMaker.make(config)
  given JsonValueCodec[ReferenceVersion]      = JsonCodecMaker.make(config)

  // system
  given JsonValueCodec[Planet]         = JsonCodecMaker.make(config)
  given JsonValueCodec[Station]        = JsonCodecMaker.make(config)
  given JsonValueCodec[Stargate]       = JsonCodecMaker.make(config)
  given JsonValueCodec[SolarSystem]    = JsonCodecMaker.make(config)
  given JsonValueCodec[WormholeStatic] = JsonCodecMaker.make(config)

  // user
  given JsonValueCodec[MapRole]              = JsonCodecMaker.make(config)
  given JsonValueCodec[UserCharacter]        = JsonCodecMaker.make(config)
  given JsonValueCodec[UserCharacterMap]     = JsonCodecMaker.make(config)
  given JsonValueCodec[UserInfo]             = JsonCodecMaker.make(config)
  given JsonValueCodec[MapPreferences]       = JsonCodecMaker.make(config)
  given JsonValueCodec[SignaturePreferences] = JsonCodecMaker.make(config)
  given JsonValueCodec[UserPreferences]      = JsonCodecMaker.make(config)

  // map
  given JsonValueCodec[MapInfo]                = JsonCodecMaker.make(config)
  given JsonValueCodec[MapInfoWithPermissions] = JsonCodecMaker.make(config)
  given JsonValueCodec[MapPolicyMember]        = JsonCodecMaker.make(config)
  given JsonValueCodec[MapSettings]            = JsonCodecMaker.make(config)
  given JsonValueCodec[NewMap]                 = JsonCodecMaker.make(config)
  given JsonValueCodec[PolicyMemberType]       = JsonCodecMaker.make(config)
  given JsonValueCodec[MapDisplayType]         = JsonCodecMaker.make(config)
  given JsonValueCodec[CharacterLocation]      = JsonCodecMaker.make(config)

  given JsonValueCodec[IntelStance]                   = JsonCodecMaker.make(config)
  given JsonValueCodec[MapRequest]                    = JsonCodecMaker.make(config)
  given JsonValueCodec[MapMessage]                    = JsonCodecMaker.make(config)
  given JsonValueCodec[MapSystem]                     = JsonCodecMaker.make(config)
  given JsonValueCodec[SystemDisplayData]             = JsonCodecMaker.make(config)
  given JsonValueCodec[Corporation]                   = JsonCodecMaker.make(config)
  given JsonValueCodec[MapSystemStructure]            = JsonCodecMaker.make(config)
  given JsonValueCodec[MapSystemNote]                 = JsonCodecMaker.make(config)
  given JsonValueCodec[MapWormholeConnection]         = JsonCodecMaker.make(config)
  given JsonValueCodec[MapWormholeConnectionJump]     = JsonCodecMaker.make(config)
  given JsonValueCodec[MapWormholeConnectionRank]     = JsonCodecMaker.make(config)
  given JsonValueCodec[MapWormholeConnectionWithSigs] = JsonCodecMaker.make(config)
  given JsonValueCodec[MapServerStatus]               = JsonCodecMaker.make(config)
  given JsonValueCodec[SignatureGroup]                = JsonCodecMaker.make(config)
  given JsonValueCodec[WormholeMassSize]              = JsonCodecMaker.make(config)
  given JsonValueCodec[WormholeMassStatus]            = JsonCodecMaker.make(config)
  given JsonValueCodec[WormholeK162Type]              = JsonCodecMaker.make(config)
  given JsonValueCodec[WormholeConnectionType]        = JsonCodecMaker.make(config)
  given JsonValueCodec[MapSystemSignature]            = JsonCodecMaker.make(config)
  given JsonValueCodec[MapSystemSignature.Wormhole]   = JsonCodecMaker.make(config)
  given JsonValueCodec[MapSystemSnapshot]             = JsonCodecMaker.make(config)
  given JsonValueCodec[NewSystemSignature]            = JsonCodecMaker.make(config)
