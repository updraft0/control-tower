package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.*
import zio.json.*
import sttp.tapir.*
import sttp.tapir.SchemaType.SInteger

object schema:
  given longMapSchema[V: Schema]: Schema[Map[Long, V]] = Schema.schemaForMap[Long, V](_.toString)

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

  given Schema[Reference]             = Schema.derived
  given Schema[ReferenceSolarSystems] = Schema.derived

  // system
  given Schema[Planet]         = Schema.derived
  given Schema[Station]        = Schema.derived
  given Schema[SolarSystem]    = Schema.derived
  given Schema[WormholeStatic] = Schema.derived

  // user
  given Schema[MapRole]          = Schema.derived
  given Schema[UserCharacter]    = Schema.derived
  given Schema[UserCharacterMap] = Schema.derived
  given Schema[UserInfo]         = Schema.derived

  // map
  given Schema[MapInfo]          = Schema.derived
  given Schema[MapPolicyMember]  = Schema.derived
  given Schema[NewMap]           = Schema.derived
  given Schema[PolicyMemberType] = Schema.derived
  given Schema[MapDisplayType]   = Schema.derived

  given Schema[IntelStance]            = Schema.derived
  given Schema[MapRequest]             = Schema.derived
  given Schema[MapMessage]             = Schema.derived
  given Schema[MapSystem]              = Schema.derived
  given Schema[SystemDisplayData]      = Schema.derived
  given Schema[Corporation]            = Schema.derived
  given Schema[MapSystemStructure]     = Schema.derived
  given Schema[MapSystemNote]          = Schema.derived
  given Schema[MapWormholeConnection]  = Schema.derived
  given Schema[SignatureGroup]         = Schema.derived
  given Schema[WormholeMassSize]       = Schema.derived
  given Schema[WormholeMassStatus]     = Schema.derived
  given Schema[WormholeK162Type]       = Schema.derived
  given Schema[WormholeConnectionType] = Schema.derived
  given Schema[MapSystemSignature]     = Schema.derived
  given Schema[MapSystemSnapshot]      = Schema.derived

object jsoncodec:
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

  given JsonCodec[Reference]             = JsonCodec.derived
  given JsonCodec[ReferenceSolarSystems] = JsonCodec.derived

  // system
  given JsonCodec[Planet]         = JsonCodec.derived
  given JsonCodec[Station]        = JsonCodec.derived
  given JsonCodec[SolarSystem]    = JsonCodec.derived
  given JsonCodec[WormholeStatic] = JsonCodec.derived

  // user
  given JsonCodec[MapRole]          = JsonCodec.derived
  given JsonCodec[UserCharacter]    = JsonCodec.derived
  given JsonCodec[UserCharacterMap] = JsonCodec.derived
  given JsonCodec[UserInfo]         = JsonCodec.derived

  // map
  given JsonCodec[MapInfo]          = JsonCodec.derived
  given JsonCodec[MapPolicyMember]  = JsonCodec.derived
  given JsonCodec[NewMap]           = JsonCodec.derived
  given JsonCodec[PolicyMemberType] = JsonCodec.derived
  given JsonCodec[MapDisplayType]   = JsonCodec.derived

  given JsonCodec[IntelStance]            = JsonCodec.derived
  given JsonCodec[MapRequest]             = JsonCodec.derived
  given JsonCodec[MapMessage]             = JsonCodec.derived
  given JsonCodec[MapSystem]              = JsonCodec.derived
  given JsonCodec[SystemDisplayData]      = JsonCodec.derived
  given JsonCodec[Corporation]            = JsonCodec.derived
  given JsonCodec[MapSystemStructure]     = JsonCodec.derived
  given JsonCodec[MapSystemNote]          = JsonCodec.derived
  given JsonCodec[MapWormholeConnection]  = JsonCodec.derived
  given JsonCodec[SignatureGroup]         = JsonCodec.derived
  given JsonCodec[WormholeMassSize]       = JsonCodec.derived
  given JsonCodec[WormholeMassStatus]     = JsonCodec.derived
  given JsonCodec[WormholeK162Type]       = JsonCodec.derived
  given JsonCodec[WormholeConnectionType] = JsonCodec.derived
  given JsonCodec[MapSystemSignature]     = JsonCodec.derived
  given JsonCodec[MapSystemSnapshot]      = JsonCodec.derived
