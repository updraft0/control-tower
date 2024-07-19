package org.updraft0.json

import com.github.plokhotnyuk.jsoniter_scala.core.*

import scala.collection.mutable
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.util.Try

// Json AST - jsoniter does not ship one but having one is nice for tests

enum Json derives CanEqual:
  case Obj(value: Map[String, Json]) extends Json
  case Arr(value: Vector[Json])      extends Json
  case Num(value: BigDecimal)        extends Json
  case Bool(value: Boolean)          extends Json
  case Str(value: String)            extends Json
  case Null                          extends Json

object Json:
  object Obj:
    def apply(values: (String, Json)*): Json.Obj = new Json.Obj(Map.from(values))
  object Arr:
    def apply(values: Array[Json]): Json.Arr = new Json.Arr(values.toVector)
    def apply(values: Json*): Json.Arr       = new Json.Arr(Vector.from(values))
  object Num:
    def apply(value: Short)  = new Json.Num(BigDecimal(value))
    def apply(value: Int)    = new Json.Num(BigDecimal(value))
    def apply(value: Long)   = new Json.Num(BigDecimal(value))
    def apply(value: Float)  = new Json.Num(BigDecimal(value))
    def apply(value: Double) = new Json.Num(BigDecimal(value))

  given JsonValueCodec[Json] = new JsonValueCodec[Json]:
    override def decodeValue(in: JsonReader, default: Json): Json =
      in.nextToken() match
        case '"' =>
          in.rollbackToken()
          Json.Str(in.readString(null))
        case 'f' | 't' =>
          in.rollbackToken()
          Json.Bool(in.readBoolean())
        case 'n' =>
          in.rollbackToken()
          in.readNullOrError(default, "expected `null` value")
          Json.Null
        case b if b == '-' || (b >= '0' && b <= '9') =>
          in.rollbackToken()
          Json.Num(in.readBigDecimal(null))
        case '[' =>
          val arr = new mutable.ArrayBuffer[Json]
          if (!in.isNextToken(']'))
            in.rollbackToken()
            arr += decodeValue(in, default)
            while in.isNextToken(',')
            do arr += decodeValue(in, default)
            if (!in.isCurrentToken(']')) in.arrayEndOrCommaError()
          new Json.Arr(arr.toVector)
        case '{' =>
          val obj = new java.util.LinkedHashMap[String, Json]
          if (!in.isNextToken('}'))
            in.rollbackToken()
            obj.put(in.readKeyAsString(), decodeValue(in, default))
            while in.isNextToken(',')
            do obj.put(in.readKeyAsString(), decodeValue(in, default))
            if (!in.isCurrentToken('}')) in.objectEndOrCommaError()
          new Json.Obj(obj.asScala.toMap)
        case _ => in.decodeError("expected JSON value")

    override def encodeValue(x: Json, out: JsonWriter): Unit =
      x match
        case Json.Obj(m) =>
          out.writeObjectStart()
          for ((k, v) <- m) {
            out.writeKey(k)
            encodeValue(v, out)
          }
          out.writeObjectEnd()
        case Json.Arr(xs) =>
          out.writeArrayStart()
          for (x <- xs) encodeValue(x, out)
          out.writeArrayEnd()
        case Json.Null =>
          out.writeNull()
        case Json.Str(value) =>
          out.writeVal(value)
        case Json.Bool(value) =>
          out.writeVal(value)
        case Json.Num(value) =>
          out.writeVal(value)

    override def nullValue: Json = Json.Null

  extension (j: Json)
    def as[A: JsonValueCodec]: Either[Throwable, A] =
      Try(readFromString[A](writeToString(j))).toEither

extension [A](a: A) def toJsonAST(using JsonValueCodec[A]): Json = readFromString[Json](writeToString(a))
