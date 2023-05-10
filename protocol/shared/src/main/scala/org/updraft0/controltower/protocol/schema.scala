package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.*
import zio.json.*
import sttp.tapir.*
import sttp.tapir.SchemaType.SInteger

object schema:
  // constant
  given Schema[SpaceType]      = Schema.derived
  given Schema[WormholeClass]  = Schema(SInteger[WormholeClass]()).format("int32")
  given Schema[WormholeEffect] = Schema(SInteger[WormholeEffect]()).format("int64")

  // system
  given Schema[Planet]         = Schema.derived
  given Schema[Station]        = Schema.derived
  given Schema[SolarSystem]    = Schema.derived
  given Schema[WormholeStatic] = Schema.derived

  // reference
  given Schema[Faction]          = Schema.derived
  given Schema[ShipType]         = Schema.derived
  given Schema[StarType]         = Schema.derived
  given Schema[StationService]   = Schema.derived
  given Schema[StationOperation] = Schema.derived
  given Schema[WormholeType]     = Schema.derived

  given Schema[Reference] = Schema.derived

object jsoncodec:
  // constant
  given JsonCodec[SpaceType]      = JsonCodec.derived
  given JsonCodec[WormholeClass]  = JsonCodec.int.transform(WormholeClasses.ById.apply, _.value)
  given JsonCodec[WormholeEffect] = JsonCodec.long.transform(WormholeEffects.ById.apply, _.typeId)

  // system
  given JsonCodec[Planet]         = JsonCodec.derived
  given JsonCodec[Station]        = JsonCodec.derived
  given JsonCodec[SolarSystem]    = JsonCodec.derived
  given JsonCodec[WormholeStatic] = JsonCodec.derived

  // reference
  given JsonCodec[Faction]          = JsonCodec.derived
  given JsonCodec[ShipType]         = JsonCodec.derived
  given JsonCodec[StarType]         = JsonCodec.derived
  given JsonCodec[StationService]   = JsonCodec.derived
  given JsonCodec[StationOperation] = JsonCodec.derived
  given JsonCodec[WormholeType]     = JsonCodec.derived

  given JsonCodec[Reference] = JsonCodec.derived
