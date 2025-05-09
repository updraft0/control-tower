package org.updraft0.controltower.db

import org.slf4j.LoggerFactory
import org.sqlite.SQLiteConfig
import org.sqlite.javax.SQLiteConnectionPoolDataSource
import zio.{ZIO, ZLayer}

import java.io.PrintWriter
import java.sql.Connection
import java.util.logging.Logger
import javax.sql.DataSource

/** Custom DataSource that attaches multiple SQLite databases into one connection
  */
object datasource:
  def layer: ZLayer[Config, Nothing, DataSource] = ZLayer(ZIO.serviceWith[Config](c => apply(c, false)))
  // TODO: remove this special layer
  def layerSde: ZLayer[Config, Nothing, DataSource] = ZLayer(ZIO.serviceWith[Config](c => apply(c, true)))

  def apply(c: Config, inSdeLoad: Boolean): DataSource =
    val cfg = new SQLiteConfig()
    cfg.setJournalMode(SQLiteConfig.JournalMode.WAL)
    val ds = new SQLiteConnectionPoolDataSource(cfg)
    ds.setUrl(s"${org.sqlite.JDBC.PREFIX}${c.optParams}")
    new MultiDbDatasource(c, ds, inSdeLoad)

private[db] class MultiDbDatasource(cfg: Config, orig: DataSource, inSdeLoad: Boolean) extends DataSource:
  private val logger = LoggerFactory.getLogger(getClass)

  private inline def executeLogged(s: java.sql.Statement, q: String) =
    if logger.isTraceEnabled then logger.trace(s"exec: ${q}")
    s.execute(q)

  override def getConnection: Connection =
    val c = orig.getConnection
    val s = c.createStatement()

    cfg.flywayConfig.databases.foreach:
      case (name, (path, _)) => executeLogged(s, s"ATTACH DATABASE '$path' AS $name;")

    // FIXME reenable foreign keys
    if false && !inSdeLoad then
      executeLogged(s, "PRAGMA foreign_keys = ON;") // FIXME figure out why SDE load is violating FK
    s.close()
    c

  override def getConnection(username: String, password: String): Connection = getConnection
  override def getLogWriter: PrintWriter                                     = orig.getLogWriter
  override def setLogWriter(out: PrintWriter): Unit                          = orig.setLogWriter(out)
  override def setLoginTimeout(seconds: Int): Unit                           = orig.setLoginTimeout(seconds)
  override def getLoginTimeout: Int                                          = orig.getLoginTimeout
  override def unwrap[T](iface: Class[T]): T                                 = orig.unwrap(iface)
  override def isWrapperFor(iface: Class[?]): Boolean                        = orig.isWrapperFor(iface)
  override def getParentLogger: Logger                                       = orig.getParentLogger
