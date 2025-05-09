package org.updraft0.controltower.sde.yaml

// Very basic YAML cursor for extracting typed values

import zio.*

import java.{lang as jl, util as ju}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.collection.mutable

type YamlValue[T] = IO[Error, T]

private[sde] type YamlObject[K <: KeyType] = ju.LinkedHashMap[K, AnyRef]
private[sde] type YamlArray                = ju.ArrayList[AnyRef]
type KeyType                               = String | Integer

private[yaml] def toCursor[K <: KeyType](yaml: YamlObject[K]): Cursor[K] = ObjectCursor(yaml, Vector.empty)

// note: this isn't really symmetric but this is meant to be minimal not a YAML optics library
trait Cursor[K <: KeyType]:
  // scalar ops
  def downField(name: K): Cursor[K]
  def as[T](using FromYaml[T]): YamlValue[T]

  // optional opts
  def mapOptional[T](f: Cursor[K] => YamlValue[T]): YamlValue[Option[T]]

  // array ops
  def mapArray[K2 <: KeyType, T](f: Cursor[K2] => YamlValue[T]): YamlValue[Vector[T]]

  // object ops
  def mapObject[K2 <: KeyType, K3 <: KeyType, T](f: (K2, Cursor[K3]) => YamlValue[T]): YamlValue[Iterable[(K2, T)]]

private[yaml] case class ObjectCursor[K <: KeyType](v: YamlObject[K], path: Vector[K]) extends Cursor[K]:
  override def downField(name: K): Cursor[K] = copy(path = path.appended(name))
  override def as[T](using from: FromYaml[T]): YamlValue[T] =
    ZIO
      .attempt(downObjectRaw(v, path))
      .mapError(ex => Error.Cursor(path, Error.Raw(ex)))
      .flatMap(from.from(_).mapError(Error.Cursor(path, _)))

  override def mapOptional[T](f: Cursor[K] => YamlValue[T]): YamlValue[Option[T]] =
    ZIO
      .attempt(downObjectRaw(v, path))
      .mapError(ex => Error.Cursor(path, Error.Raw(ex)))
      .flatMap {
        case null => ZIO.succeed(None)
        case obj: ju.LinkedHashMap[_, _] =>
          f(ObjectCursor[K](obj.asInstanceOf[YamlObject[K]], Vector.empty[K])).map(Some(_))
        case other => ZIO.fail(Error.InvalidType("Optional(Object)", other.getClass.getName))
      }
      .mapError(Error.Cursor(path, _))

  override def mapArray[K2 <: KeyType, T](f: Cursor[K2] => YamlValue[T]): YamlValue[Vector[T]] =
    ZIO
      .attempt(downObjectRaw(v, path))
      .mapError(ex => Error.Cursor(path, Error.Raw(ex)))
      .flatMap {
        case null => ZIO.succeed(Vector.empty[T])
        case arr: ju.ArrayList[_] =>
          ZIO
            .foreach(arr.asScala) {
              case obj: ju.LinkedHashMap[_, _] =>
                f(ObjectCursor[K2](obj.asInstanceOf[YamlObject[K2]], Vector.empty[K2]))
              case other => ZIO.fail(Error.InvalidType("Object", other.getClass.getName))
            }
            .mapBoth(Error.Cursor(path, _), _.toVector)
        case other => ZIO.fail(Error.Cursor(path, Error.InvalidType("Array", other.getClass.getName)))
      }

  override def mapObject[K2 <: KeyType, K3 <: KeyType, T](
      f: (K2, Cursor[K3]) => YamlValue[T]
  ): YamlValue[Iterable[(K2, T)]] =
    ZIO
      .attempt(downObjectRaw(v, path))
      .mapError(ex => Error.Cursor(path, Error.Raw(ex)))
      .flatMap {
        case null => ZIO.succeed(Map.empty[K2, T])
        case map: ju.LinkedHashMap[_, _] =>
          ZIO
            .foreach(asScala(map.asInstanceOf[YamlObject[K2]])) {
              case ((k, v: ju.LinkedHashMap[_, _])) =>
                f(k, ObjectCursor(v.asInstanceOf[YamlObject[K3]], Vector.empty)).map(fv => k -> fv)
              case _ => ??? // unreachable
            }
            .mapError(Error.Cursor(path, _))
        case other => ZIO.fail(Error.Cursor(path, Error.InvalidType("Object", other.getClass.getName)))
      }

@tailrec
private def downObjectRaw[K <: KeyType](v: AnyRef, path: Vector[K]): AnyRef =
  if (v == null || path.isEmpty) v
  else {
    val map = v.asInstanceOf[YamlObject[K]]
    if (!map.containsKey(path.head)) null
    else downObjectRaw(map.get(path.head), path.tail)
  }

// need to preserve order...
private def asScala[K, V](jmap: ju.LinkedHashMap[K, V]): Vector[(K, V)] =
  val res = mutable.ArrayBuffer.empty[(K, V)]
  jmap.forEach((k, v) => res.addOne(k, v))
  res.toVector

private def destroyOrderMap[K, V](values: Iterable[(K, V)]): Map[K, V] =
  val res = mutable.LinkedHashMap.empty[K, V]
  res.addAll(values)
  res.toMap

trait FromYaml[T]:
  def from(yaml: AnyRef): YamlValue[T]

given FromYaml[String] =
  case s: String => ZIO.succeed(s)
  case null      => ZIO.fail(Error.MissingValue("String"))
  case other     => ZIO.fail(Error.InvalidType(s"String $other" /* FIXME */, other.getClass.getName))

given FromYaml[Int] =
  case i: jl.Integer => ZIO.succeed(i)
  case s: jl.Short   => ZIO.succeed(s.toInt)
  case l: jl.Long    => ZIO.attempt(l.toInt).orElseFail(Error.InvalidType("Int", "Long"))
  case null          => ZIO.fail(Error.MissingValue("Int"))
  case other         => ZIO.fail(Error.InvalidType("Int", other.getClass.getName))

given FromYaml[Long] =
  case l: jl.Long    => ZIO.succeed(l)
  case i: jl.Integer => ZIO.succeed(i.toLong)
  case s: jl.Short   => ZIO.succeed(s.toLong)
  case null          => ZIO.fail(Error.MissingValue("Long"))
  case other         => ZIO.fail(Error.InvalidType("Long", other.getClass.getName))

given FromYaml[Boolean] =
  case b: jl.Boolean => ZIO.succeed(b)
  case null          => ZIO.fail(Error.MissingValue("Bool"))
  case other         => ZIO.fail(Error.InvalidType("Bool", other.getClass.getName))

given FromYaml[Double] =
  case d: jl.Double => ZIO.succeed(d)
  case f: jl.Float  => ZIO.succeed(f.toDouble)
  case null         => ZIO.fail(Error.MissingValue("Double"))
  case other        => ZIO.fail(Error.InvalidType("Double", other.getClass.getName))

given [T: {FromYaml as from}] => FromYaml[Vector[T]] =
  case al: ju.ArrayList[_] => ZIO.foreach(al.asScala)(from.from).map(_.toVector)
  case null                => ZIO.fail(Error.MissingValue("Array"))
  case other               => ZIO.fail(Error.InvalidType("Array", other.getClass.getName))

given [T: {FromYaml as from}] => FromYaml[Option[T]] =
  case null  => ZIO.none
  case other => from.from(other).map(Some.apply)

given [K: {FromYaml as fk}, V: {FromYaml as fv}] => FromYaml[Map[K, V]] =
  case m: ju.LinkedHashMap[_, _] =>
    ZIO
      .foreach(asScala(m))((k, v) => fk.from(k).flatMap(key => fv.from(v).map(value => key -> value)))
      .map(destroyOrderMap)
  case null  => ZIO.succeed(Map.empty[K, V])
  case other => ZIO.fail(Error.InvalidType("Map", other.getClass.getName))
