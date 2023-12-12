package org.updraft0.controltower.protocol
import sttp.tapir.Schema
import zio.json.{JsonDecoder, JsonEncoder}
import zio.json.ast.Json
import zio.json.internal.Write

/** Wrapper for emitting nulls for options
  * @see
  *   https://github.com/zio/zio-json/issues/638
  */
case class OptionNull[+A](value: Option[A]) extends AnyVal

object OptionNull:
  given [A]: Conversion[Option[A], OptionNull[A]] with
    override def apply(x: Option[A]): OptionNull[A] = OptionNull(x)

  given [A]: Conversion[OptionNull[A], Option[A]] with
    override def apply(x: OptionNull[A]): Option[A] = x.value

  given [A](using enc: JsonEncoder[A]): JsonEncoder[OptionNull[A]] with
    override def unsafeEncode(a: OptionNull[A], indent: Option[Int], out: Write): Unit = a.value match
      case None    => out.write("null")
      case Some(a) => enc.unsafeEncode(a, indent, out)
    override def isNothing(a: OptionNull[A]): Boolean = a.value match
      case None    => false
      case Some(a) => enc.isNothing(a)
    override def toJsonAST(a: OptionNull[A]): Either[String, Json] = a.value match
      case None    => Right(Json.Null)
      case Some(a) => enc.toJsonAST(a)

  given [A](using dec: JsonDecoder[A]): JsonDecoder[OptionNull[A]] = JsonDecoder.option[A].map(OptionNull(_))

  given [A](using schema: Schema[A]): Schema[OptionNull[A]] = Schema.schemaForOption[A].as[OptionNull[A]]
