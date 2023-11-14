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

  inline def jsonObject9[R](
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
      inline v8: Any,
      inline k9: Any,
      inline v9: Any
  ) =
    sql"json_object($k1, $v1, $k2, $v2, $k3, $v3, $k4, $v4, $k5, $v5, $k6, $v6, $k7, $v7, $k8, $v8, $k9, $v9)".pure
      .as[JsonValue[R]]

  inline def jsonObject10[R](
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
      inline v8: Any,
      inline k9: Any,
      inline v9: Any,
      inline k10: Any,
      inline v10: Any
  ) =
    sql"json_object($k1, $v1, $k2, $v2, $k3, $v3, $k4, $v4, $k5, $v5, $k6, $v6, $k7, $v7, $k8, $v8, $k9, $v9, $k10, $v10)".pure
      .as[JsonValue[R]]

  inline def jsonObject11[R](
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
      inline v8: Any,
      inline k9: Any,
      inline v9: Any,
      inline k10: Any,
      inline v10: Any,
      inline k11: Any,
      inline v11: Any
  ) =
    sql"json_object($k1, $v1, $k2, $v2, $k3, $v3, $k4, $v4, $k5, $v5, $k6, $v6, $k7, $v7, $k8, $v8, $k9, $v9, $k10, $v10, $k11, $v11)".pure
      .as[JsonValue[R]]

  inline def jsonObject17[R](
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
      inline v8: Any,
      inline k9: Any,
      inline v9: Any,
      inline k10: Any,
      inline v10: Any,
      inline k11: Any,
      inline v11: Any,
      inline k12: Any,
      inline v12: Any,
      inline k13: Any,
      inline v13: Any,
      inline k14: Any,
      inline v14: Any,
      inline k15: Any,
      inline v15: Any,
      inline k16: Any,
      inline v16: Any,
      inline k17: Any,
      inline v17: Any
  ) =
    sql"json_object($k1, $v1, $k2, $v2, $k3, $v3, $k4, $v4, $k5, $v5, $k6, $v6, $k7, $v7, $k8, $v8, $k9, $v9, $k10, $v10, $k11, $v11, $k12, $v12, $k13, $v13, $k14, $v14, $k15, $v15, $k16, $v16, $k17, $v17)".pure
      .as[JsonValue[R]]

/** sqlite dialect with json support
  */
class SqliteJsonZioJdbcContext[+N <: NamingStrategy](val naming: N)
    extends ZioJdbcContext[SqliteModifiedDialect, N]
    with SqliteJdbcTypes[SqliteModifiedDialect, N]
    with StringJsonExtensions:
  val idiom: SqliteModifiedDialect = SqliteModifiedDialect

  val connDelegate: ZioJdbcUnderlyingContext[SqliteModifiedDialect, N] =
    new SqliteJsonZioJdbcContext.Underlying[N](naming)

object SqliteJsonZioJdbcContext:
  class Underlying[+N <: NamingStrategy](val naming: N)
      extends ZioJdbcUnderlyingContext[SqliteModifiedDialect, N]
      with SqliteJdbcTypes[SqliteModifiedDialect, N]
      with StringJsonExtensions:
    val idiom: SqliteModifiedDialect = SqliteModifiedDialect

trait SqliteModifiedDialect extends SqliteDialect { self =>
  import io.getquill.ast.Ast
  import io.getquill.idiom.StatementInterpolator.*
  import io.getquill.context.sql.{FlattenSqlQuery, SetOperationSqlQuery, SqlQuery, UnaryOperationSqlQuery}

  def parentTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy, idiomContext: IdiomContext) =
    super.sqlQueryTokenizer

  // NOTE: sqlite does not support encasing UNION operations with parens so don't do that
  override implicit def sqlQueryTokenizer(implicit
      astTokenizer: Tokenizer[Ast],
      strategy: NamingStrategy,
      idiomContext: IdiomContext
  ): Tokenizer[SqlQuery] = Tokenizer[SqlQuery] {
    case SetOperationSqlQuery(a, op, b) =>
      stmt"${a.token} ${op.token} ${b.token}"
    case other =>
      parentTokenizer.token(other)
  }
}

object SqliteModifiedDialect extends SqliteModifiedDialect
