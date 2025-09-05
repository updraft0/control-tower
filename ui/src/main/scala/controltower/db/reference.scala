package controltower.db

import controltower.backend.ControlTowerBackend
import org.getshaka.nativeconverter.{NativeConverter, fromNative}
import org.scalajs.dom.idb.*
import org.updraft0.controltower.build.BuildInfo
import org.updraft0.controltower.constant.SystemId
import org.updraft0.controltower.protocol.*
import org.updraft0.controltower.protocol.native.given

import scala.annotation.unused
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.util.{Failure, Success, Try}

trait ReferenceDataStore:

  def systemForId(systemId: SystemId): Future[Option[SolarSystem]]
  def systemForName(name: String): Future[Option[SolarSystem]]

  // TODO: add fuzzies
  def searchSystemName(value: String): Future[List[SolarSystem]]

  def wormholeTypeByName(value: String): Future[Option[WormholeType]] =
    referenceAll().map(r => r.wormholeTypes.find(_.name == value))

  def referenceAll(): Future[Reference]

object ReferenceDataStore:

  def usingBackend()(using ct: ControlTowerBackend): Future[ReferenceDataStore] =
    for
      version <- ct.getVersion()
      _       <- refreshIfVersionMismatch(version)
      ds      <- IdbReferenceDataStore(version.data, version.code)
    yield ds

class IdbReferenceDataStore(db: Database, maxSearchHits: Int = 10) extends ReferenceDataStore:
  import IdbReferenceDataStore.{AllReferenceKey, ByNameIndex, ReferenceAll, SolarSystem}

  override def systemForId(systemId: SystemId): Future[Option[SolarSystem]] =
    for
      trx <- solarSystemTx
      res <- inTransaction(trx, SolarSystem, _.get(systemId.toNative)).map(optOr[SolarSystem](_))
      _   <- onFinished(trx)
    yield res

  override def systemForName(name: String): Future[Option[SolarSystem]] =
    for
      trx <- solarSystemTx
      res <- inTransaction(trx, SolarSystem, _.index(ByNameIndex).get(name)).map(optOr[SolarSystem](_))
      _   <- onFinished(trx)
    yield res

  // TODO: add fuzzies
  override def searchSystemName(value: String): Future[List[SolarSystem]] =
    for
      trx <- solarSystemTx
      nextValue = if (value.nonEmpty) s"${value.dropRight(1)}${(value.charAt(value.length - 1) + 1).toChar}" else value
      values <- readCursor[SolarSystem, Index](
        trx,
        SolarSystem,
        _.index(ByNameIndex).openCursor(
          KeyRange.bound(
            value,
            nextValue,
            lowerOpen = false,
            upperOpen = true
          )
        ),
        Some(maxSearchHits)
      )
      _ <- onFinished(trx)
    yield values

  override def referenceAll(): Future[Reference] =
    for
      trx <- referenceTx
      res <- inTransaction(trx, ReferenceAll, _.get(AllReferenceKey)).map(_.asInstanceOf[js.Any].fromNative[Reference])
      _   <- onFinished(trx)
    yield res

  inline private def solarSystemTx = Future.fromTry(Try(db.transaction(SolarSystem, TransactionMode.readonly)))
  inline private def referenceTx   = Future.fromTry(Try(db.transaction(ReferenceAll, TransactionMode.readonly)))
  inline private def optOr[A: NativeConverter](valueOr: Any): Option[A] =
    if (js.isUndefined(valueOr)) None else Some(valueOr.asInstanceOf[js.Any].fromNative[A])

  inline private def readCursor[A: NativeConverter, S <: org.scalajs.dom.IDBStoreLike[S]](
      tx: Transaction,
      storeName: String,
      getCursor: ObjectStore => Request[S, CursorWithValue[S]],
      limit: Option[Int]
  ): Future[List[A]] =
    // 🤯 when you call cursor.continue() it re-fires the callback given
    val res   = Promise[List[A]]
    val state = js.Array[A]()
    val req   = getCursor(tx.objectStore(storeName))
    req.onerror = { e =>
      res.complete(Failure(IndexedDbError("failed in transaction", e)))
    }
    req.onsuccess = { ev =>
      val cursor = ev.target.result
      if (cursor == null || js.isUndefined(cursor.value) || limit.exists(_ <= state.length))
        res.complete(Success(state.toList))
      else {
        state.push(cursor.value.asInstanceOf[js.Any].fromNative[A])
        cursor.continue()
      }
    }
    res.future

object IdbReferenceDataStore:
  private val SolarSystem     = "solar_system"
  private val ReferenceAll    = "reference_all"
  private val VersionInfo     = "version_info"
  private val ByNameIndex     = "byName"
  private val AllReferenceKey = "all"
  private val CodeVersionKey  = "code_version"

  // TODO: think of a better algorithm for determining what needs to be updated in the db - maybe a last_updated
  //        timestamp?
  private val NumSolarSystemsApprox = 8_000

  given CanEqual[Any, String] = CanEqual.derived

  private[db] def apply(dbVersion: Int, codeVersion: String, forceRefresh: Boolean = false)(using
      ControlTowerBackend
  ): Future[IdbReferenceDataStore] =
    for
      factory <- Future.fromTry(indexedDb)
      db      <- openIndexedDb(factory, "controltower_ref", dbVersion, createTables)
      // check version
      trx0 = db.transaction(VersionInfo, TransactionMode.readonly)
      currDbVersion <- inTransaction(trx0, VersionInfo, _.get(CodeVersionKey))
      _             <- onFinished(trx0)
      doForce = (forceRefresh || currDbVersion != codeVersion)
      // first transaction - make sure solar system table is populated
      trx1 = db.transaction(SolarSystem, TransactionMode.readonly)
      solarSystemKeys <- inTransaction(trx1, SolarSystem, _.getAllKeys())
      _               <- onFinished(trx1)
      _               <-
        if (doForce || solarSystemKeys.length < NumSolarSystemsApprox) populateAllSolarSystems(db)
        else Future.successful(())
      // second transaction - make sure reference table is populated
      trx2 = db.transaction(ReferenceAll, TransactionMode.readonly)
      referenceKeys <- inTransaction(trx2, ReferenceAll, _.getAllKeys())
      _             <- onFinished(trx2)
      _             <- if (doForce || referenceKeys.length != 1) populateAllReference(db) else Future.successful(())
      // last transaction - update version
      _ <- populateCodeVersion(db, codeVersion)
    yield new IdbReferenceDataStore(db)

  private def populateAllSolarSystems(db: Database)(using ct: ControlTowerBackend): Future[Unit] =
    for
      all <- ct.getSolarSystemsAll()
      trx = db.transaction(SolarSystem, TransactionMode.readwrite)
      _ <- inTransaction(trx, SolarSystem, _.clear())
      _ <- Future.sequence(all.solarSystems.map(ss => inTransaction(trx, SolarSystem, _.add(ss.toNative))))
      _ <- onFinished(trx)
    yield ()

  private def populateAllReference(db: Database)(using ct: ControlTowerBackend): Future[Unit] =
    for
      all <- ct.getReferenceAll()
      trx = db.transaction(ReferenceAll, TransactionMode.readwrite)
      _ <- inTransaction(trx, ReferenceAll, _.clear())
      _ <- inTransaction(trx, ReferenceAll, _.put(all.toNative, AllReferenceKey))
      _ <- onFinished(trx)
    yield ()

  private def populateCodeVersion(db: Database, codeVersion: String): Future[Unit] =
    val trx = db.transaction(VersionInfo, TransactionMode.readwrite)
    for
      _ <- inTransaction(trx, VersionInfo, _.put(codeVersion, CodeVersionKey))
      _ <- onFinished(trx)
    yield ()

  private def createTables(e: VersionChangeEvent): Unit =
    val db               = e.target.result
    val objectStoreNames = db.objectStoreNames
    if (!objectStoreNames.contains(SolarSystem)) {
      val os = db.createObjectStore(
        SolarSystem,
        new CreateObjectStoreOptions {
          override val keyPath       = "id"
          override val autoIncrement = js.undefined
        }
      )
      os.createIndex(
        ByNameIndex,
        "name",
        new CreateIndexOptions {
          override val unique = true
        }
      )
    }
    if (!objectStoreNames.contains(ReferenceAll)) {
      val _ = db.createObjectStore(ReferenceAll)
    }
    if (!objectStoreNames.contains(VersionInfo)) {
      val _ = db.createObjectStore(VersionInfo)
    }

class IndexedDbError(msg: String, @unused cause: org.scalajs.dom.Event) extends RuntimeException(msg)

private def indexedDb: Try[org.scalajs.dom.idb.Factory] =
  org.scalajs.dom.window.indexedDB.toRight(new UnsupportedOperationException("window.indexedDB not set")).toTry

private def openIndexedDb(
    factory: Factory,
    name: String,
    version: Int,
    onUpgrade: VersionChangeEvent => Unit
): Future[Database] =
  val res = Promise[Database]
  val req = factory.open(name, version.toDouble)
  req.onblocked = { e =>
    res.complete(Failure(IndexedDbError(s"IDB blocked", e)))
  }
  req.onupgradeneeded = onUpgrade
  req.onerror = { e =>
    res.complete(Failure(IndexedDbError(e.message, e)))
  }
  req.onsuccess = { _ =>
    res.complete(Success(req.result))
  }
  res.future

private def inTransaction[A](
    trx: Transaction,
    name: String,
    f: ObjectStore => Request[org.scalajs.dom.IDBStoreLike[?], A]
): Future[A] =
  val res = Promise[A]
  val req = f(trx.objectStore(name))
  req.onerror = { e =>
    res.complete(Failure(IndexedDbError("failed in transaction", e)))
  }
  req.onsuccess = { _ =>
    res.complete(Success(req.result))
  }
  res.future

private def onFinished(trx: Transaction): Future[Unit] =
  val res = Promise[Unit]
  trx.oncomplete = { _ =>
    res.complete(Success(()))
  }
  trx.onerror = { e =>
    res.complete(Failure(IndexedDbError("transaction errored", e)))
  }
  trx.onabort = { e =>
    res.complete(Failure(IndexedDbError("transaction aborted", e)))
  }
  res.future

// arguably the wrong place for this but
private def refreshIfVersionMismatch(version: ReferenceVersion) =
  if (version.code != BuildInfo.gitHash)
    org.scalajs.dom.console.log(s"Reloading due to server version ${version.code} != ${BuildInfo.gitHash}")
    org.scalajs.dom.window.location.reload()
    Future.never
  else Future.unit
