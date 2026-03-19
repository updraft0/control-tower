package org.updraft0.controltower.sde

import zio.*
import zio.stream.*

import java.io.*
import java.nio.file.Path
import java.util.zip as jzip
import java.util.zip.ZipInputStream

object zip:
  /** @note
    *   The stream is only valid once for a single entry so it *must* be used
    */
  case class ZipEntry(name: String, size: Long, directory: Boolean, stream: ZipInputStream)

  enum Error:
    case IOError(e: IOException)
    case Unknown(cause: Throwable)

  def readEntries(
      path: Path
  ): ZStream[Any, Error, ZipEntry] =
    ZStream
      .scoped(zipInputStream(path.toFile))
      .flatMap(zis => ZStream.repeatZIOOption(nextOrFail(zis.getNextEntry)).map(e => e -> zis))
      .map(toEntry.tupled)
      .mapError:
        case ioe: IOException => Error.IOError(ioe)
        case other            => Error.Unknown(other)

  private def zipInputStream(file: File) =
    ZIO
      .fromAutoCloseable(ZIO.attempt(new FileInputStream(file)))
      .flatMap(fis => ZIO.fromAutoCloseable(ZIO.attempt(new BufferedInputStream(fis))))
      .flatMap(bis => ZIO.fromAutoCloseable(ZIO.attempt(new jzip.ZipInputStream(bis))))

  private def toEntry(entry: jzip.ZipEntry, zis: jzip.ZipInputStream) =
    ZipEntry(entry.getName, entry.getSize, entry.isDirectory, zis)

  private def nextOrFail[A <: AnyRef](action: => A): ZIO[Any, Option[Throwable], A] =
    ZIO
      .attempt(action)
      .mapError(Some(_))
      .flatMap:
        case null => ZIO.fail[Option[Throwable]](None)
        case a    => ZIO.succeed(a)
