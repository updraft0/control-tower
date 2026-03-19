package org.updraft0.controltower.sde

import com.github.plokhotnyuk.jsoniter_scala.core.{
  JsonReader,
  JsonReaderException,
  JsonValueCodec,
  JsonWriter,
  ReaderConfig,
  scanJsonValuesFromStream
}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.updraft0.controltower.constant.*
import zio.*
import zio.stream.*

import java.util.zip.ZipInputStream

/** Parser of SDE data export types
  */
object parser:

  // limitation of how we're handling parsing of individual files
  // TODO: figure out how to get rid of this
  private val MaxEntitiesInFile: Int = 64_000

  enum Error:
    case Json(cause: JsonReaderException)
    case Zip(error: zip.Error)
    case Unknown(cause: Throwable)

  // constant
  private given JsonValueCodec[WormholeClass] = new JsonValueCodec[WormholeClass]:
    override def decodeValue(in: JsonReader, default: WormholeClass): WormholeClass = WormholeClasses.ById(in.readInt())
    override def encodeValue(x: WormholeClass, out: JsonWriter): Unit               = out.writeVal(x.value)
    override def nullValue: WormholeClass                                           = null
  private given JsonValueCodec[WormholeEffect] = new JsonValueCodec[WormholeEffect]:
    override def decodeValue(in: JsonReader, default: WormholeEffect): WormholeEffect =
      WormholeEffects.ById(in.readInt())
    override def encodeValue(x: WormholeEffect, out: JsonWriter): Unit = out.writeVal(x.typeId.value)
    override def nullValue: WormholeEffect                             = null

  // codecs
  private given JsonValueCodec[CategoryId]               = JsonCodecMaker.make
  private given JsonValueCodec[DogmaAttributeCategory]   = JsonCodecMaker.make
  private given JsonValueCodec[DogmaAttribute]           = JsonCodecMaker.make
  private given JsonValueCodec[Faction]                  = JsonCodecMaker.make
  private given JsonValueCodec[GroupId]                  = JsonCodecMaker.make
  private given JsonValueCodec[Constellation]            = JsonCodecMaker.make
  private given JsonValueCodec[NpcCorporation]           = JsonCodecMaker.make
  private given JsonValueCodec[NpcStation]               = JsonCodecMaker.make
  private given JsonValueCodec[ExportedData.Planet]      = JsonCodecMaker.make
  private given JsonValueCodec[Region]                   = JsonCodecMaker.make
  private given JsonValueCodec[ExportedData.SolarSystem] = JsonCodecMaker.make
  private given JsonValueCodec[ExportedData.Stargate]    = JsonCodecMaker.make
  private given JsonValueCodec[ExportedData.Star]        = JsonCodecMaker.make
  private given JsonValueCodec[StationOperation]         = JsonCodecMaker.make
  private given JsonValueCodec[StationService]           = JsonCodecMaker.make
  private given JsonValueCodec[TypeDogma]                = JsonCodecMaker.make
  private given JsonValueCodec[ExportedData.TypeId]      = JsonCodecMaker.make
  private given JsonValueCodec[SecondarySun]             = JsonCodecMaker.make

  def parse(entry: zip.ZipEntry): ZStream[Any, Error, ExportedData] =
    parseRaw(entry).mapError:
      case e: JsonReaderException => Error.Json(e)
      case x                      => Error.Unknown(x)

  private[sde] def parseRaw(entry: zip.ZipEntry) =
    entry.name match
      case "_sde.jsonl" => ZStream.empty // ignore
      // combined feeds
      case "categories.jsonl" =>
        combineAll(parseJsonLines[CategoryId](entry.stream), ExportedData.CategoryIds(_))
      case "dogmaAttributeCategories.jsonl" =>
        combineAll(parseJsonLines[DogmaAttributeCategory](entry.stream), ExportedData.DogmaAttributeCategories(_))
      case "dogmaAttributes.jsonl" =>
        combineAll(parseJsonLines[DogmaAttribute](entry.stream), ExportedData.DogmaAttributes(_))
      case "factions.jsonl" =>
        combineAll(parseJsonLines[Faction](entry.stream), ExportedData.Factions(_))
      case "groups.jsonl" =>
        combineAll(parseJsonLines[GroupId](entry.stream), ExportedData.GroupIds(_))
      case "mapConstellations.jsonl" =>
        combineAll(parseJsonLines[Constellation](entry.stream), ExportedData.Constellations(_))
      case "mapRegions.jsonl" =>
        combineAll(parseJsonLines[Region](entry.stream), ExportedData.Regions(_))
      case "mapSecondarySuns.jsonl" =>
        combineAll(parseJsonLines[SecondarySun](entry.stream), ExportedData.SecondarySuns(_))
      case "npcCorporations.jsonl" =>
        combineAll(parseJsonLines[NpcCorporation](entry.stream), ExportedData.NpcCorporations(_))
      case "npcStations.jsonl" =>
        combineAll(parseJsonLines[NpcStation](entry.stream), ExportedData.NpcStations(_))
      case "stationOperations.jsonl" =>
        combineAll(parseJsonLines[StationOperation](entry.stream), ExportedData.StationOperations(_))
      case "stationServices.jsonl" =>
        combineAll(parseJsonLines[StationService](entry.stream), ExportedData.StationServices(_))
      case "typeDogma.jsonl" =>
        combineAll(parseJsonLines[TypeDogma](entry.stream), ExportedData.TypeDogmas(_))
      // individual feeds
      case "mapPlanets.jsonl" =>
        parseJsonLines[ExportedData.Planet](entry.stream)
      case "mapSolarSystems.jsonl" =>
        parseJsonLines[ExportedData.SolarSystem](entry.stream)
      case "mapStargates.jsonl" =>
        parseJsonLines[ExportedData.Stargate](entry.stream)
      case "mapStars.jsonl" =>
        parseJsonLines[ExportedData.Star](entry.stream)
      case "types.jsonl" =>
        parseJsonLines[ExportedData.TypeId](entry.stream)
      // unmapped
      case other =>
        ZStream.logWarning(s"Ignoring unmapped SDE entry $other") &> ZStream.empty

  private[sde] def parseJsonLines[A: JsonValueCodec](
      in: ZipInputStream,
      chunkSize: Int = 128
  ): ZStream[Any, Throwable, A] =
    ZStream
      .asyncZIO[Any, Throwable, A](cb => ZIO.attempt(scanJsonValuesStreamZIO(in, chunkSize, cb)), MaxEntitiesInFile)

  private[sde] def scanJsonValuesStreamZIO[R, E, A: JsonValueCodec](
      zis: ZipInputStream,
      chunkSize: Int,
      cb: ZStream.Emit[R, E, A, Unit],
      config: ReaderConfig = ReaderConfig
  ): Unit =
    val chunker = ChunkBuilder.make[A](chunkSize)
    var count   = 0
    scanJsonValuesFromStream[A](zis, config): a =>
      chunker.addOne(a)
      count += 1
      val isEnd = zis.available() == 0
      if isEnd || count >= chunkSize then
        cb(ZIO.succeed(chunker.result()))
        chunker.clear()
        count = 0
      !isEnd
    if count > 0 then
      cb(ZIO.succeed(chunker.result()))
      chunker.clear()
    cb(ZIO.fail(Option.empty[E]))

  private[sde] def combineAll[A, B <: ExportedData](
      in: ZStream[Any, Throwable, A],
      f: Chunk[A] => B
  ): ZStream[Any, Throwable, B] =
    ZStream.fromZIO(in.runCollect.map(f))
