package org.updraft0.controltower.db.query

import io.getquill.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model.*
import org.updraft0.controltower.db.query.ctx.*
import zio.ZIO

object map:
  import schema.*
  import ctx.{*, given}
  import zio.json.{*, given}

  private val WormholeGroupId = 988L

  given MappedEncoding[Int, WormholeClass] = MappedEncoding(WormholeClasses.ById.apply)
  given MappedEncoding[WormholeClass, Int] = MappedEncoding(_.value)

  /** Each table lives in the `map` schema, but Quill has no config annotation/etc. for that
    */
  object schema:
    inline def map = quote(querySchema[MapModel]("map.map"))
    inline def alliance = quote(querySchema[Alliance]("map.alliance"))
    inline def corporation = quote(querySchema[Corporation]("map.corporation"))
    inline def systemStaticWormhole = quote(querySchema[SystemStaticWormhole]("map.ref_system_static_wormhole"))
    inline def wormhole             = quote(querySchema[Wormhole]("map.ref_wormhole"))

  // queries
  def getWormholesUsingTypeDogma: DbOperation[List[(Long, String, JsonValue[Map[String, Double]])]] =
    ctx.run(quote {
      (for
        it  <- sde.schema.itemType.filter(_.groupId == lift(WormholeGroupId))
        ida <- sde.schema.itemDogmaAttribute.join(_.itemId == it.id)
        dat <- sde.schema.dogmaAttributeType.join(_.id == ida.attributeId)
      yield (it, ida, dat)) // NOTE: the group by it.name is important because Wormhole C729 has multiple entries
        .groupByMap((it, _, _) => (it.id, it.name))((it, ida, dat) =>
          (it.id, it.name, jsonGroupObject(dat.name, ida.value))
        )
    })

  def getWormholeSystemNames: DbOperation[Map[String, Long]] =
    ctx.run(sde.schema.solarSystem.filter(_.name like "J%").map(ss => ss.name -> ss.id)).map(_.toMap)

  def getWormholeTypeNames: DbOperation[List[(String, Long)]] =
    ctx.run(sde.schema.itemType.filter(_.groupId == lift(WormholeGroupId)).map(it => it.name -> it.id))

  // upserts
  def upsertSystemStatic(value: SystemStaticWormhole): DbOperation[Long] =
    ctx.run(systemStaticWormhole.insertValue(lift(value)).onConflictIgnore(_.systemId, _.staticTypeId, _.validFrom))

  def upsertWormhole(value: Wormhole): DbOperation[Long] =
    ctx.run(wormhole.insertValue(lift(value)).onConflictIgnore(_.name))
