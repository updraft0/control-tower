package org.updraft0.controltower.sde.yaml

import org.snakeyaml.engine.v2.api.{ConstructNode, Load, LoadSettings}
import org.snakeyaml.engine.v2.constructor.ConstructYamlNull
import org.snakeyaml.engine.v2.constructor.core.{ConstructYamlCoreBool, ConstructYamlCoreInt}
import org.snakeyaml.engine.v2.constructor.json.{ConstructYamlJsonBool, ConstructYamlJsonFloat, ConstructYamlJsonInt}
import org.snakeyaml.engine.v2.nodes
import org.snakeyaml.engine.v2.nodes.Tag
import org.snakeyaml.engine.v2.resolver.ModifiedFloatSchema
import org.snakeyaml.engine.v2.schema.{FailsafeSchema, Schema}
import zio.{Tag as _, *}

import java.util
import scala.jdk.CollectionConverters.*

enum Error:
  case Parsing(cause: Throwable)
  case MissingValue(typ: String)
  case InvalidType(expected: String, got: String)
  case Cursor[K <: KeyType](path: Vector[K], cause: Error)
  case Raw(cause: Throwable)

object YAML:

  def layer: ZLayer[Any, Nothing, LoadSettings] =
    ZLayer.fromZIO(
      ZIO.succeed(LoadSettings.builder().setSchema(ModifiedFloatSchema).setCodePointLimit(Int.MaxValue).build())
    )

  def cursor[K <: KeyType](s: String): ZIO[LoadSettings, Error, Cursor[K]] =
    parse(s).map(toCursor[K])

  def cursor[K <: KeyType](o: YamlObject[K]): UIO[Cursor[K]] = ZIO.succeed(toCursor(o))

  def parse[K <: KeyType](bytes: Array[Byte]): ZIO[LoadSettings, Error, YamlObject[K]] =
    ZIO
      .attempt(new String(bytes))
      .mapError(Error.Parsing.apply)
      .flatMap(parse)

  def parse[K <: KeyType](s: String): ZIO[LoadSettings, Error, YamlObject[K]] =
    ZIO.serviceWithZIO(settings =>
      ZIO
        .attempt(new Load(settings).loadFromString(s).asInstanceOf[YamlObject[K]])
        .mapError(Error.Parsing.apply)
    )

  def parseArray(bytes: Array[Byte]): ZIO[LoadSettings, Error, YamlArray] =
    ZIO
      .attempt(new String(bytes))
      .mapError(Error.Parsing.apply)
      .flatMap(parseArray)

  def parseArray(s: String): ZIO[LoadSettings, Error, YamlArray] =
    ZIO.serviceWithZIO(settings =>
      ZIO
        .attempt(new Load(settings).loadFromString(s).asInstanceOf[YamlArray])
        .mapError(Error.Parsing.apply)
    )

  // this is where the simplistic design breaks down
  def mapArrayCursor[K <: KeyType, T](a: YamlArray, f: Cursor[K] => YamlValue[T]): YamlValue[Vector[T]] =
    ZIO.foreach(a.asScala)(el => f(ObjectCursor[K](el.asInstanceOf[YamlObject[K]], Vector.empty))).map(_.toVector)
