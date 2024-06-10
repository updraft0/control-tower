package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.*
export org.updraft0.controltower.constant.SigId
export org.updraft0.controltower.constant.SigId.given
export org.updraft0.controltower.constant.SystemId.given
import zio.json.*
import sttp.tapir.*
import sttp.tapir.SchemaType.SInteger

object schema:
  given longMapSchema[V: Schema]: Schema[Map[Long, V]]               = Schema.schemaForMap[Long, V](_.toString)
  given characterMapSchema[V: Schema]: Schema[Map[CharacterId, V]]   = Schema.schemaForMap[CharacterId, V](_.toString)
  given connectionMapSchema[V: Schema]: Schema[Map[ConnectionId, V]] = Schema.schemaForMap[ConnectionId, V](_.toString)
  given systemMapSchema[V: Schema]: Schema[Map[SystemId, V]]         = Schema.schemaForMap[SystemId, V](_.toString)

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

  // system
  given Schema[Planet]         = Schema.derived
  given Schema[Station]        = Schema.derived
  given Schema[Stargate]       = Schema.derived
  given Schema[SolarSystem]    = Schema.derived
  given Schema[WormholeStatic] = Schema.derived
  given Schema[NewSystemName]  = Schema.derived

  // user
  given Schema[MapRole]          = Schema.derived
  given Schema[UserCharacter]    = Schema.derived
  given Schema[UserCharacterMap] = Schema.derived
  given Schema[UserInfo]         = Schema.derived
  given Schema[UserPreferences]  = Schema.derived

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
  given Schema[NewSystemSignature]            = Schema.derived

object jsoncodec:

  // opaque
  given JsonFieldEncoder[SystemId] = JsonFieldEncoder.long.asInstanceOf[JsonFieldEncoder[SystemId]]
  given JsonFieldDecoder[SystemId] = JsonFieldDecoder.long.asInstanceOf[JsonFieldDecoder[SystemId]]

  given JsonFieldEncoder[ConnectionId] = JsonFieldEncoder.long.asInstanceOf[JsonFieldEncoder[ConnectionId]]
  given JsonFieldDecoder[ConnectionId] = JsonFieldDecoder.long.asInstanceOf[JsonFieldDecoder[ConnectionId]]

  given JsonFieldEncoder[CharacterId] = JsonFieldEncoder.long.asInstanceOf[JsonFieldEncoder[CharacterId]]
  given JsonFieldDecoder[CharacterId] = JsonFieldDecoder.long.asInstanceOf[JsonFieldDecoder[CharacterId]]

  given JsonCodec[CharacterId]   = JsonCodec.long.asInstanceOf[JsonCodec[CharacterId]]
  given JsonCodec[CorporationId] = JsonCodec.long.asInstanceOf[JsonCodec[CorporationId]]
  given JsonCodec[AllianceId]    = JsonCodec.long.asInstanceOf[JsonCodec[AllianceId]]
  given JsonCodec[SigId]         = JsonCodec.string.asInstanceOf[JsonCodec[SigId]]
  given JsonCodec[SystemId]      = JsonCodec.long.asInstanceOf[JsonCodec[SystemId]]
  given JsonCodec[UserId]        = JsonCodec.long.asInstanceOf[JsonCodec[UserId]]
  given JsonCodec[MapId]         = JsonCodec.long.asInstanceOf[JsonCodec[MapId]]
  given JsonCodec[ConnectionId]  = JsonCodec.long.asInstanceOf[JsonCodec[ConnectionId]]

  // auth

  // constant
  given JsonCodec[SpaceType]      = JsonCodec.derived
  given JsonCodec[WormholeClass]  = JsonCodec.int.transform(WormholeClasses.ById.apply, _.value)
  given JsonCodec[WormholeEffect] = JsonCodec.long.transform(WormholeEffects.ById.apply, _.typeId)

  // reference
  given JsonCodec[Faction]          = JsonCodec.derived
  given JsonCodec[ShipType]         = JsonCodec.derived
  given JsonCodec[StarType]         = JsonCodec.derived
  given JsonCodec[StationService]   = JsonCodec.derived
  given JsonCodec[StationOperation] = JsonCodec.derived
  given JsonCodec[WormholeType]     = JsonCodec.derived
  given JsonCodec[SignatureInGroup] = JsonCodec.derived

  given JsonCodec[Reference]             = JsonCodec.derived
  given JsonCodec[ReferenceSolarSystems] = JsonCodec.derived

  // system
  given JsonCodec[Planet]         = JsonCodec.derived
  given JsonCodec[Station]        = JsonCodec.derived
  given JsonCodec[Stargate]       = JsonCodec.derived
  given JsonCodec[SolarSystem]    = JsonCodec.derived
  given JsonCodec[WormholeStatic] = JsonCodec.derived
  given JsonCodec[NewSystemName]  = JsonCodec.derived

  // user
  given JsonCodec[MapRole]          = JsonCodec.string.transform(MapRole.valueOf, _.toString)
  given JsonCodec[UserCharacter]    = JsonCodec.derived
  given JsonCodec[UserCharacterMap] = JsonCodec.derived
  given JsonCodec[UserInfo]         = JsonCodec.derived
  given JsonCodec[UserPreferences]  = JsonCodec.derived

  // map
  given JsonCodec[MapInfo]                = JsonCodec.derived
  given JsonCodec[MapInfoWithPermissions] = JsonCodec.derived
  given JsonCodec[MapPolicyMember]        = JsonCodec.derived
  given JsonCodec[MapSettings]            = JsonCodec.derived
  given JsonCodec[NewMap]                 = JsonCodec.derived
  given JsonCodec[PolicyMemberType]       = JsonCodec.string.transform(PolicyMemberType.valueOf, _.toString)
  given JsonCodec[MapDisplayType]         = JsonCodec.string.transform(MapDisplayType.valueOf, _.toString)
  given JsonCodec[CharacterLocation]      = JsonCodec.derived

  given JsonCodec[IntelStance]                   = JsonCodec.string.transform(IntelStance.valueOf, _.toString)
  given JsonCodec[MapRequest]                    = JsonCodec.derived
  given JsonCodec[MapMessage]                    = JsonCodec.derived
  given JsonCodec[MapSystem]                     = JsonCodec.derived
  given JsonCodec[SystemDisplayData]             = JsonCodec.derived
  given JsonCodec[Corporation]                   = JsonCodec.derived
  given JsonCodec[MapSystemStructure]            = JsonCodec.derived
  given JsonCodec[MapSystemNote]                 = JsonCodec.derived
  given JsonCodec[MapWormholeConnection]         = JsonCodec.derived
  given JsonCodec[MapWormholeConnectionJump]     = JsonCodec.derived
  given JsonCodec[MapWormholeConnectionRank]     = JsonCodec.derived
  given JsonCodec[MapWormholeConnectionWithSigs] = JsonCodec.derived
  given JsonCodec[SignatureGroup]                = JsonCodec.string.transform(SignatureGroup.valueOf, _.toString)
  given JsonCodec[WormholeMassSize]              = JsonCodec.string.transform(WormholeMassSize.valueOf, _.toString)
  given JsonCodec[WormholeMassStatus]            = JsonCodec.string.transform(WormholeMassStatus.valueOf, _.toString)
  given JsonCodec[WormholeK162Type]              = JsonCodec.string.transform(WormholeK162Type.valueOf, _.toString)
  given JsonCodec[WormholeConnectionType]        = JsonCodec.derived
  given JsonCodec[MapSystemSignature]            = JsonCodec.derived
  given JsonCodec[MapSystemSignature.Wormhole]   = JsonCodec.derived
  given JsonCodec[MapSystemSnapshot]             = JsonCodec.derived
  given JsonCodec[NewSystemSignature]            = JsonCodec.derived
