package org.updraft0.controltower.db

import io.getquill.*
import io.getquill.context.jdbc.{Decoders, Encoders, JdbcContextTypes, SqliteJdbcTypes}
import io.getquill.context.qzio.{ZioJdbcContext, ZioJdbcUnderlyingContext}
import zio.json.ast.Json
import zio.json.{JsonDecoder, JsonEncoder}

import java.sql.Types

trait StringJsonExtensions extends Encoders with Decoders:
  this: JdbcContextTypes[_, _] =>

  given [T](using enc: JsonEncoder[T]): MappedEncoding[JsonValue[T], String] =
    MappedEncoding(v => enc.encodeJson(v.value, None).toString)

  given [T](using dec: JsonDecoder[T]): MappedEncoding[String, JsonValue[T]] =
    MappedEncoding(s =>
      dec.decodeJson(s) match {
        case Left(message) => throw new IllegalArgumentException(s"Could not decode string to JSON: $message")
        case Right(value)  => JsonValue(value)
      }
    )

  // json_group_array
  inline def jsonGroupArray[R](inline v: Any) =
    sql"json_group_array(${v})".pure.as[JsonValue[Vector[R]]]

  inline def jsonGroupArrayFilterNull[R](inline v: Any, inline c: Any) =
    sql"json_group_array(${v}) filter (where ${c} is not null)".pure.as[JsonValue[Vector[R]]]

  inline def jsonGroupArrayDistinct[R](inline v: Any) =
    sql"json_group_array(distinct ${v})".pure.as[JsonValue[Vector[R]]]

  inline def jsonGroupArrayFilterNullDistinct[R](inline v: Any, inline c: Any) =
    sql"json_group_array(distinct ${v}) filter (where ${c} is not null)".pure.as[JsonValue[Vector[R]]]

  // json_group_object
  inline def jsonGroupObject[R](inline k1: Any, inline v1: Any) =
    sql"json_group_object(${k1}, ${v1})".pure.as[JsonValue[Map[String, R]]]

  // json_object
  inline def jsonObject1[R](inline k1: Any, inline v1: Any) =
    sql"json_object($k1, $v1)".pure.as[JsonValue[R]]

  inline def jsonObject2[R](inline k1: Any, inline v1: Any, inline k2: Any, inline v2: Any) =
    sql"json_object($k1, $v1, $k2, $v2)".pure.as[JsonValue[R]]

  inline def jsonObject3[R](
      inline k1: Any,
      inline v1: Any,
      inline k2: Any,
      inline v2: Any,
      inline k3: Any,
      inline v3: Any
  ) =
    sql"json_object($k1, $v1, $k2, $v2, $k3, $v3)".pure.as[JsonValue[R]]

  inline def jsonObject4[R](
      inline k1: Any,
      inline v1: Any,
      inline k2: Any,
      inline v2: Any,
      inline k3: Any,
      inline v3: Any,
      inline k4: Any,
      inline v4: Any
  ) =
    sql"json_object($k1, $v1, $k2, $v2, $k3, $v3, $k4, $v4)".pure.as[JsonValue[R]]

  inline def jsonObject5[R](
      inline k1: Any,
      inline v1: Any,
      inline k2: Any,
      inline v2: Any,
      inline k3: Any,
      inline v3: Any,
      inline k4: Any,
      inline v4: Any,
      inline k5: Any,
      inline v5: Any
  ) =
    sql"json_object($k1, $v1, $k2, $v2, $k3, $v3, $k4, $v4, $k5, $v5)".pure.as[JsonValue[R]]

  inline def jsonObject6[R](
      inline k1: Any,
      inline v1: Any,
      inline k2: Any,
      inline v2: Any,
      inline k3: Any,
      inline v3: Any,
      inline k4: Any,
      inline v4: Any,
      inline k5: Any,
      inline v5: Any,
      inline k6: Any,
      inline v6: Any
  ) =
    sql"json_object($k1, $v1, $k2, $v2, $k3, $v3, $k4, $v4, $k5, $v5, $k6, $v6)".pure.as[JsonValue[R]]

  inline def jsonObject7[R](
      inline k1: Any,
      inline v1: Any,
      inline k2: Any,
      inline v2: Any,
      inline k3: Any,
      inline v3: Any,
      inline k4: Any,
      inline v4: Any,
      inline k5: Any,
      inline v5: Any,
      inline k6: Any,
      inline v6: Any,
      inline k7: Any,
      inline v7: Any
  ) =
    sql"json_object($k1, $v1, $k2, $v2, $k3, $v3, $k4, $v4, $k5, $v5, $k6, $v6, $k7, $v7)".pure.as[JsonValue[R]]

  inline def jsonObject8[R](
      inline k1: Any,
      inline v1: Any,
      inline k2: Any,
      inline v2: Any,
      inline k3: Any,
      inline v3: Any,
      inline k4: Any,
      inline v4: Any,
      inline k5: Any,
      inline v5: Any,
      inline k6: Any,
      inline v6: Any,
      inline k7: Any,
      inline v7: Any,
      inline k8: Any,
      inline v8: Any
  ) =
    sql"json_object($k1, $v1, $k2, $v2, $k3, $v3, $k4, $v4, $k5, $v5, $k6, $v6, $k7, $v7, $k8, $v8)".pure
      .as[JsonValue[R]]

/** sqlite dialect with json support
  */
class SqliteJsonZioJdbcContext[+N <: NamingStrategy](val naming: N)
    extends ZioJdbcContext[SqliteDialect, N]
    with SqliteJdbcTypes[SqliteDialect, N]
    with StringJsonExtensions:
  val idiom: SqliteDialect = SqliteDialect

  val connDelegate: ZioJdbcUnderlyingContext[SqliteDialect, N] = new SqliteJsonZioJdbcContext.Underlying[N](naming)

object SqliteJsonZioJdbcContext:
  class Underlying[+N <: NamingStrategy](val naming: N)
      extends ZioJdbcUnderlyingContext[SqliteDialect, N]
      with SqliteJdbcTypes[SqliteDialect, N]
      with StringJsonExtensions:
    val idiom: SqliteDialect = SqliteDialect
