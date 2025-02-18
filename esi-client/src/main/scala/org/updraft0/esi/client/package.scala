package org.updraft0.esi

import org.updraft0.controltower.constant.*
import sttp.tapir.{Codec, Schema}
import sttp.tapir.CodecFormat.TextPlain
import sttp.tapir.SchemaType.SInteger

import scala.annotation.nowarn
import scala.util.matching.Regex

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

  given Codec[String, SearchCategory, TextPlain] =
    Codec.string.map(s => SearchCategory.valueOf(Casing.pascalCase(s)))(c => Casing.snakeCase(c.toString))

// TODO move?
private[esi] object Casing:
  private val LowerUpperRegex: Regex       = "(\\p{Ll})(\\p{Lu})".r
  private val UpperUpperLowerRegex: Regex  = "(\\p{Lu})(\\p{Lu}\\p{Ll})".r
  private val NonAlphaRegex: Regex         = "[^\\p{L}\\d]+".r
  private val Delimiter                    = "\u0000"
  private val ReplacementSeparator: String = s"$$1$Delimiter$$2"

  @nowarn
  private def split(in: String): Seq[String] =
    NonAlphaRegex
      .replaceAllIn(
        UpperUpperLowerRegex.replaceAllIn(LowerUpperRegex.replaceAllIn(in, ReplacementSeparator), ReplacementSeparator),
        Delimiter
      )
      .split(Delimiter)
      .filter(_.nonEmpty)

  def snakeCase(in: String): String =
    split(in).map(_.toLowerCase).mkString("_")

  def pascalCase(in: String): String =
    split(in).map(_.toLowerCase.capitalize).mkString
