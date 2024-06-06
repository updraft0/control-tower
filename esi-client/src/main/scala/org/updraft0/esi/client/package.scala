package org.updraft0.esi

import org.updraft0.controltower.constant.*
import sttp.tapir.{Codec, Schema}
import sttp.tapir.CodecFormat.TextPlain
import sttp.tapir.SchemaType.SInteger

package object client:
  given Schema[CharacterId]   = Schema(SInteger()).format("int64")
  given Schema[AllianceId]    = Schema(SInteger()).format("int64")
  given Schema[CorporationId] = Schema(SInteger()).format("int64")
  given Schema[SystemId]      = Schema(SInteger()).format("int64")

  given Codec[String, CharacterId, TextPlain] =
    Codec.string.map(s => CharacterId.apply(s.toLong))(_.value.toString)

  given Codec[String, AllianceId, TextPlain] =
    Codec.string.map(s => AllianceId.apply(s.toLong))(_.value.toString)

  given Codec[String, CorporationId, TextPlain] =
    Codec.string.map(s => CorporationId.apply(s.toLong))(_.value.toString)
