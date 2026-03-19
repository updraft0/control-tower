package org.updraft0.controltower.server

import org.updraft0.controltower.db.model.SdeLoadMeta
import org.updraft0.controltower.db.query
import zio.*
import org.updraft0.esi.client.{SdeClient, SdeVersion}
import org.updraft0.controltower.{db, sde, sdeloader}
import org.updraft0.controltower.sde.read as readSdeZip
import zio.stream.ZSink

import java.time.Instant
import java.nio.file.{Files, Path}
import javax.sql.DataSource

def updateReferenceData =
  ZIO.logSpan("updateReferenceData")(
    checkSdeAndReload().unit *> query.map.vacuumMap *> query.transaction(sdeloader.loadDerivedData) *>
      ZIO.logInfo("Refreshed WH static data")
  )

/** Load while checking if up to date
  */
private[server] def checkSdeAndReload(): RIO[SdeConfig & DataSource & SdeClient, (Int, Chunk[Long])] =
  (query.transaction(query.sde.getLatestVersion) <&> loadSdeLatestVersion).flatMap:
    case (None, latest) => ZIO.logInfo("SDE not loaded, loading") *> loadSde(latest)
    case (Some(version), latest) if version.buildNumber == latest.buildNumber =>
      ZIO.logInfo(s"SDE up to date with build ${version.buildNumber}").as(version.id -> Chunk.empty)
    case (Some(version), latest) =>
      ZIO.logInfo(s"SDE checksum changed from ${version.buildNumber} to ${latest.buildNumber}, updating") *> loadSde(
        latest
      )

// checksums contains a newline-delimited list of checksums and we got to find the one for sde.zip
private def loadSdeLatestVersion =
  ZIO.serviceWithZIO[SdeClient](_.latestVersion)

// TODO: add query.transaction() here
private[server] def loadSde(latest: SdeVersion): RIO[SdeConfig & DataSource & SdeClient, (Int, Chunk[Long])] =
  for
    _        <- query.transaction(deleteAllSde()).tap(c => ZIO.logDebug(s"deleted ${c} records"))
    _        <- query.sde.vacuumSde
    stream   <- ZIO.serviceWithZIO[SdeClient](_.sdeZip(latest))
    tempFile <- ZIO.attemptBlockingIO(Files.createTempFile("sde", "zip"))
    fileSink = ZSink.fromFile(tempFile.toFile)
    savedBytes <- stream.run(fileSink)
    meta = SdeLoadMeta(savedBytes, checksum = "")
    _ <- ZIO.logDebug(s"SDE zip downloaded with version=${latest.buildNumber} and $meta")
    _ <- query.transaction(sdeloader.intoDb(readSdeZip(tempFile), latest.releaseDate, latest.buildNumber, meta))
    _ <- ZIO.attemptBlockingIO(Files.delete(tempFile))
    _ <- ZIO.logDebug("SDE load complete")
  yield (0, Chunk.empty)

private[server] def deleteAllSde(): RIO[DataSource, Long] =
  import query.sde.*

  (deleteConstellation <*> deleteDogmaAttributeCategory <*> deleteDogmaAttributeType <*> deleteFaction <*>
    deleteItemCategory <*> deleteItemDogmaAttribute <*> deleteItemGroup <*> deleteItemType <*>
    deleteNpcCorporation <*> deleteNpcStation <*> deleteRegion <*> deleteSolarSystem <*> deleteSolarSystemEffect <*>
    deleteSolarSystemPlanet <*> deleteSolarSystemStar <*> deleteStargate <*>
    deleteStationOperation <*> deleteStationOperationService <*> deleteStationService)
    .map(xs => (xs.productIterator.asInstanceOf[Iterator[Long]]))
    .map(_.sum)

object SdeLoaderTest extends ZIOAppDefault:

  def special =
    for
      _ <- query.transaction(deleteAllSde()).tap(c => ZIO.log(s"deleted ${c} records"))
      _ <- query.sde.vacuumSde
      sdeEntries = sde.read(
        Path.of("/Users/user/proj/evedev/control-tower/temp/eve-online-static-data-3253748-jsonl.zip")
      )
      meta = SdeLoadMeta(0, "")
      loaded <- query.transaction(sdeloader.intoDb(sdeEntries, Instant.EPOCH, 0, meta))
      _      <- ZIO.log("special SDE load complete")
    yield loaded

  override val bootstrap =
    Runtime.disableFlags(RuntimeFlag.FiberRoots) >>> Runtime.enableRuntimeMetrics >>> desktopLogger

  override def run =
    special
      .provide(
        ZLayer.succeed(db.Config(Path.of("/Users/user/proj/evedev/control-tower/sde-temp"))),
        db.postMigrationLayer
      )
