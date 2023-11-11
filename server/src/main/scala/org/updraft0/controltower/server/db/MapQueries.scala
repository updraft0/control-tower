package org.updraft0.controltower.server.db

import io.getquill.*
import io.getquill.extras.*
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.query.*
import org.updraft0.controltower.protocol
import org.updraft0.controltower.protocol.jsoncodec.given
import zio.*

import java.nio.file.Paths
import java.util.UUID
import javax.sql.DataSource

/** Queries for map information
  */
object MapQueries:
  import ctx.{*, given}
  import map.given
  import map.schema.*

  type MapId = Long

  def getMap(id: MapId): Result[Option[model.MapModel]] =
    run(quote {
      map.filter(_.id == lift(id))
    }).map(_.headOption)

  def getMapsById(mapIds: List[MapId]): Result[List[model.MapModel]] =
    run(quote {
      map.filter(m => liftQuery(mapIds).contains(m.id))
    })

  def getMapByUserAndName(userId: Long, name: String): Result[Option[model.MapModel]] =
    run(quote {
      map
        .filter(_.name == lift(name))
        .filter(_.creatorUserId == lift(userId))
    })
      .map(_.headOption)

  def createMap(userId: Long, name: String): Result[MapId] =
    run(quote {
      map
        .insert(_.creatorUserId -> lift(userId), _.name -> lift(name))
        .returning(_.id)
    })
