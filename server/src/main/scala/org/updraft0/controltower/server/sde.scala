package org.updraft0.controltower.server

import org.updraft0.controltower.db.model.SdeLoadMeta
import org.updraft0.controltower.db.query
import zio.*
import org.updraft0.esi.client.SdeClient
import org.updraft0.controltower.sdeloader
import org.updraft0.controltower.sde.{read => readSdeZip, group => groupEntries}
import zio.stream.ZSink

import java.nio.file.Files
import javax.sql.DataSource

def updateReferenceData =
  ZIO.logSpan("updateReferenceData")(
    checkSdeAndReload().unit *> query.map.vacuumMap *> query.transaction(sdeloader.loadDerivedData) *>
      ZIO.logInfo("Refreshed WH static data")
  )

/** Load while checking if up to date
  */
private[server] def checkSdeAndReload(): RIO[SdeConfig & DataSource & SdeClient, (Int, Chunk[Long])] =
  (query.transaction(query.sde.getLatestVersion) <&> ZIO.serviceWithZIO[SdeClient](_.latestChecksum)).flatMap:
    case (None, checksum) => ZIO.logInfo("SDE not loaded, loading") *> loadSde(checksum)
    case (Some(version), checksum) if version.meta.checksum == checksum =>
      ZIO.logInfo(s"SDE up to date with ${version}").as(version.id -> Chunk.empty)
    case (Some(version), checksum) =>
      ZIO.logInfo(s"SDE checksum changed from ${version.meta.checksum} to ${checksum}, updating") *> loadSde(checksum)

// TODO: add query.transaction() here
private[server] def loadSde(checksum: String): RIO[SdeConfig & DataSource & SdeClient, (Int, Chunk[Long])] =
  for
    parallel <- ZIO.serviceWith[SdeConfig](_.parallel)
    _        <- query.transaction(deleteAllSde()).flatMap(c => ZIO.logDebug(s"deleted ${c} records"))
    _        <- query.sde.vacuumSde
    stream   <- ZIO.serviceWithZIO[SdeClient](_.sdeZip)
    tempFile <- ZIO.attemptBlockingIO(Files.createTempFile("sde", "zip"))
    fileSink = ZSink.fromFile(tempFile.toFile)
    savedBytes <- stream.run(fileSink)
    meta = SdeLoadMeta(savedBytes, checksum)
    _      <- ZIO.logDebug(s"SDE zip downloaded with $meta")
    loaded <- query.transaction(sdeloader.intoDb(groupEntries(readSdeZip(tempFile, parallel)), meta))
    _      <- ZIO.attemptBlockingIO(Files.delete(tempFile))
    _      <- ZIO.logDebug("SDE load complete")
  yield loaded

private[server] def deleteAllSde(): RIO[DataSource, Long] =
  import query.sde.*

  (deleteConstellation <*> deleteDogmaAttributeCategory <*> deleteDogmaAttributeType <*> deleteFaction <*>
    deleteItemCategory <*> deleteItemDogmaAttribute <*> deleteItemGroup <*> deleteItemName <*> deleteItemType <*>
    deleteNpcCorporation <*> deleteNpcStation <*> deleteRegion <*> deleteSolarSystem <*> deleteSolarSystemAsteroidBelt
    <*> deleteSolarSystemPlanet <*> deleteSolarSystemMoon <*> deleteSolarSystemStar <*> deleteStargate <*>
    deleteStationOperation <*> deleteStationOperationService <*> deleteStationService)
    .map(xs => (xs.productIterator.asInstanceOf[Iterator[Long]]))
    .map(_.sum)
