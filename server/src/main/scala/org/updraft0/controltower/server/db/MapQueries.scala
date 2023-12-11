package org.updraft0.controltower.server.db

import io.getquill.*
import io.getquill.extras.*
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.model.MapDisplayType
import org.updraft0.controltower.db.query.*
import zio.*

import javax.sql.DataSource

case class MapSystemWithAll(
    sys: model.MapSystem,
    display: Option[model.SystemDisplayData],
    structures: Vector[model.MapSystemStructure],
    notes: Vector[model.MapSystemNote],
    signatures: Vector[model.MapSystemSignature],
    connections: Vector[model.MapWormholeConnection]
)

/** Queries for map information
  */
object MapQueries:
  import ctx.{*, given}
  import map.given
  import map.schema.*
  import zio.json.*

  // json codecs for json_array_agg usage (some logic duplicated between the MappedEntity and the codec here)
  private given JsonCodec[model.MapSystemStructure] = JsonCodec.derived
  private given JsonCodec[model.MapSystemNote]      = JsonCodec.derived
  private given JsonCodec[model.SignatureGroup] = JsonCodec.int.transform(model.SignatureGroup.fromOrdinal, _.ordinal())
  private given JsonCodec[model.WormholeMassSize] =
    JsonCodec.int.transform(model.WormholeMassSize.fromOrdinal, _.ordinal())
  private given JsonCodec[model.WormholeMassStatus] =
    JsonCodec.int.transform(model.WormholeMassStatus.fromOrdinal, _.ordinal())
  private given JsonCodec[model.WormholeK162Type] =
    JsonCodec.int.transform(model.WormholeK162Type.fromOrdinal, _.ordinal())
  private given JsonCodec[model.MapSystemSignature]    = JsonCodec.derived
  private given JsonCodec[model.MapWormholeConnection] = JsonCodec.derived

  type MapId = Long

  def getMap(id: MapId): Result[Option[model.MapModel]] =
    run(quote {
      mapModel.filter(_.id == lift(id))
    }).map(_.headOption)

  def getMapsById(mapIds: List[MapId]): Result[List[model.MapModel]] =
    run(quote {
      mapModel.filter(m => liftQuery(mapIds).contains(m.id))
    })

  def getMapByCreatorUserAndName(userId: Long, name: String): Result[Option[model.MapModel]] =
    run(quote {
      mapModel
        .filter(_.name == lift(name))
        .filter(_.creatorUserId == lift(userId))
    })
      .map(_.headOption)

  def createMap(userId: Long, name: String, displayType: MapDisplayType): Result[MapId] =
    run(quote {
      mapModel
        .insert(_.creatorUserId -> lift(userId), _.name -> lift(name), _.displayType -> lift(displayType))
        .returning(_.id)
    })

  def getMapNamesByIds(ids: List[MapId]): Result[Map[MapId, String]] =
    run(quote {
      mapModel
        .filter(m => liftQuery(ids).contains(m.id))
        .map(m => (m.id, m.name))
    }).map(_.toMap)

  def getMapSystemAll(mapId: MapId, systemId: Option[Long] = None): Result[List[MapSystemWithAll]] =
    run(quote {
      (for
        map <- mapModel.filter(_.id == lift(mapId))
        sys <- mapSystem.join(ms => ms.mapId == map.id && lift(systemId).forall(_ == ms.systemId))
        dis <- mapSystemDisplay.leftJoin(sd =>
          sd.systemId == sys.systemId && sd.mapId == sys.mapId && sd.displayType == map.displayType
        )
        mss <- mapSystemStructure.leftJoin(ss => ss.systemId == sys.systemId && ss.mapId == sys.mapId && !ss.isDeleted)
        msn <- mapSystemNote.leftJoin(sn => sn.systemId == sys.systemId && sn.mapId == sys.mapId && !sn.isDeleted)
        msi <- mapSystemSignature.leftJoin(si => si.systemId == sys.systemId && si.mapId == sys.mapId && !si.isDeleted)
        mhc <- mapWormholeConnection.leftJoin(wc =>
          Option(wc.id) == msi.flatMap(_.wormholeConnectionId) && !wc.isDeleted
        )
      yield (sys, dis.map(_.data), mss, msn, msi, mhc)).groupByMap((ms, _, _, _, _, _) => (ms))(
        (ms, dis, mss, msn, msi, mhc) =>
          (
            ms,
            dis,
            jsonGroupArrayFilterNullDistinct[model.MapSystemStructure](
              jsonObject11(
                "mapId",
                mss.map(_.mapId),
                "systemId",
                mss.map(_.systemId),
                "name",
                mss.map(_.name),
                "isDeleted",
                mss.map(_.isDeleted),
                "ownerCorporationId",
                mss.map(_.ownerCorporationId),
                "structureType",
                mss.map(_.structureType),
                "location",
                mss.map(_.location),
                "createdAt",
                mss.map(_.createdAt),
                "createdByCharacterId",
                mss.map(_.createdByCharacterId),
                "updatedAt",
                mss.map(_.updatedAt),
                "updatedByCharacterId",
                mss.map(_.updatedByCharacterId)
              ),
              mss.map(_.name)
            ),
            jsonGroupArrayFilterNullDistinct[model.MapSystemNote](
              jsonObject9(
                "id",
                msn.map(_.id),
                "mapId",
                msn.map(_.mapId),
                "systemId",
                msn.map(_.systemId),
                "note",
                msn.map(_.note),
                "isDeleted",
                msn.map(_.isDeleted),
                "createdAt",
                msn.map(_.createdAt),
                "createdByCharacterId",
                msn.map(_.createdByCharacterId),
                "updatedAt",
                msn.map(_.updatedAt),
                "updatedByCharacterId",
                msn.map(_.updatedByCharacterId)
              ),
              msn.map(_.id)
            ),
            jsonGroupArrayFilterNullDistinct[model.MapSystemSignature](
              jsonObject17(
                "mapId",
                msi.map(_.mapId),
                "systemId",
                msi.map(_.systemId),
                "signatureId",
                msi.map(_.signatureId),
                "isDeleted",
                msi.map(_.isDeleted),
                "signatureGroup",
                msi.map(_.signatureGroup),
                "signatureTypeName",
                msi.map(_.signatureTypeName),
                "wormholeIsEol",
                msi.map(_.wormholeIsEol),
                "wormholeEolAt",
                msi.map(_.wormholeEolAt),
                "wormholeTypeId",
                msi.map(_.wormholeTypeId),
                "wormholeMassSize",
                msi.map(_.wormholeMassSize),
                "wormholeMassStatus",
                msi.map(_.wormholeMassStatus),
                "wormholeK162Type",
                msi.map(_.wormholeK162Type),
                "wormholeConnectionId",
                msi.map(_.wormholeConnectionId),
                "createdAt",
                msi.map(_.createdAt),
                "createdByCharacterId",
                msi.map(_.createdByCharacterId),
                "updatedAt",
                msi.map(_.updatedAt),
                "updatedByCharacterId",
                msi.map(_.updatedByCharacterId)
              ),
              msi.map(_.signatureId)
            ),
            jsonGroupArrayFilterNullDistinct[model.MapWormholeConnection](
              jsonObject8(
                "id",
                mhc.map(_.id),
                "fromSystemId",
                mhc.map(_.fromSystemId),
                "toSystemId",
                mhc.map(_.toSystemId),
                "isDeleted",
                mhc.map(_.isDeleted),
                "createdAt",
                mhc.map(_.createdAt),
                "createdByCharacterId",
                mhc.map(_.createdByCharacterId),
                "updatedAt",
                mhc.map(_.updatedAt),
                "updatedByCharacterId",
                mhc.map(_.updatedByCharacterId)
              ),
              mhc.map(_.id)
            )
          )
      )
    }).map(
      _.map((mss, dis, structures, notes, signatures, connections) =>
        MapSystemWithAll(mss, dis, structures.value, notes.value, signatures.value, connections.value)
      )
    )
