package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.*
import zio.json.*
import sttp.tapir.*
import sttp.tapir.SchemaType.SInteger

object schema:
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

  given Schema[Reference] = Schema.derived

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

  given JsonCodec[Reference] = JsonCodec.derived

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
