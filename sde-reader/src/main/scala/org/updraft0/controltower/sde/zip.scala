package org.updraft0.controltower.sde

import zio.*
import zio.stream.*

import java.io.*
import java.nio.file.Path
import java.util.zip as jzip

object zip:
  case class ZipEntry(name: String, size: Long, directory: Boolean, bytes: Array[Byte])

  enum Error:
    case IOError(e: IOException)
    case Unknown(cause: Throwable)

  def readEntries(path: Path): ZStream[Any, Error, ZipEntry] =
    ZStream
      .scoped(zipInputStream((path.toFile)))
      .flatMap(zis => ZStream.repeatZIOOption(nextOrFail(zis.getNextEntry)).zip(ZStream.repeat(zis)))
      .mapZIO(toEntry.tupled)
      .mapError {
        case ioe: IOException => Error.IOError(ioe)
        case other            => Error.Unknown(other)
      }

  private def zipInputStream(file: File) =
    ZIO
      .fromAutoCloseable(ZIO.attempt(new FileInputStream(file)))
      .flatMap(fis => ZIO.fromAutoCloseable(ZIO.attempt(new BufferedInputStream(fis))))
      .flatMap(bis => ZIO.fromAutoCloseable(ZIO.attempt(new jzip.ZipInputStream(bis))))

  private def toEntry(entry: jzip.ZipEntry, zis: jzip.ZipInputStream) =
    ZIO
      .attempt(zis.readNBytes(entry.getSize.toInt))
      .flatMap(bytes => ZIO.attempt(ZipEntry(entry.getName, entry.getSize, entry.isDirectory, bytes)))

  private def nextOrFail[A](action: => A): ZIO[Any, Option[Throwable], A] =
    ZIO
      .attempt(action)
      .mapError(e => Some(e))
      .flatMap {
        case null => ZIO.fail[Option[Throwable]](None)
        case a    => ZIO.succeed(a)
      }
