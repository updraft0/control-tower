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
  given Schema[Planet]      = Schema.derived
  given Schema[Station]     = Schema.derived
  given Schema[SolarSystem] = Schema.derived

  // reference
  given Schema[StationService]            = Schema.derived
  given Schema[StationOperationReference] = Schema.derived

object jsoncodec:
  // constant
  given JsonCodec[SpaceType]      = JsonCodec.derived
  given JsonCodec[WormholeClass]  = JsonCodec.int.transform(WormholeClasses.ById.apply, _.value)
  given JsonCodec[WormholeEffect] = JsonCodec.long.transform(WormholeEffects.ById.apply, _.typeId)

  // system
  given JsonCodec[Planet]      = JsonCodec.derived
  given JsonCodec[Station]     = JsonCodec.derived
  given JsonCodec[SolarSystem] = JsonCodec.derived

  // reference
  given JsonCodec[StationService]            = JsonCodec.derived
  given JsonCodec[StationOperationReference] = JsonCodec.derived
