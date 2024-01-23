package org.updraft0.controltower.server.map

import org.updraft0.controltower.db.model.displayType
import org.updraft0.controltower.db.{model, query}
import org.updraft0.controltower.server.db.{MapQueries, MapSystemWithAll}
import org.updraft0.minireactive.*
import zio.*

import java.util.UUID
import org.updraft0.controltower.server.Log

import java.time.Instant
import scala.annotation.unused

type MapId    = Long
type MapEnv   = javax.sql.DataSource
type SystemId = Long

private type MapState = Map[SystemId, MapSystemWithAll]

case class MapSessionId(characterId: Long, sessionId: UUID)
case class Identified[T](sessionId: Option[MapSessionId], value: T)

case class NewMapSystemSignature(
    signatureId: String,
    signatureGroup: model.SignatureGroup,
    signatureTypeName: Option[String] = None,
    wormholeIsEol: Option[Boolean] = None,
    wormholeTypeId: Option[Long] = None,
    wormholeMassSize: model.WormholeMassSize = model.WormholeMassSize.Unknown,
    wormholeMassStatus: model.WormholeMassStatus = model.WormholeMassStatus.Unknown,
    wormholeK162Type: Option[model.WormholeK162Type] = None,
    wormholeConnectionId: Option[Long] = None
)

enum MapRequest:
  case MapSnapshot
  case AddSystem(
      systemId: SystemId,
      name: Option[String],
      isPinned: Boolean,
      displayData: model.SystemDisplayData,
      stance: Option[model.IntelStance]
  )
  case AddSystemSignature(
      systemId: SystemId,
      signature: NewMapSystemSignature
  )
  case UpdateSystemAttribute(systemId: SystemId, pinned: Option[Boolean], intelStance: Option[model.IntelStance])
  case UpdateSystemDisplay(systemId: SystemId, displayData: model.SystemDisplayData)
  case UpdateSystemSignatures(systemId: SystemId, replaceAll: Boolean, scanned: List[NewMapSystemSignature])
  case RenameSystem(systemId: SystemId, name: Option[String])
  case RemoveSystem(systemId: SystemId)
  case RemoveSystemSignatures(systemId: SystemId, signatures: Option[NonEmptyChunk[String]])

enum MapResponse:
  case Error(message: String)
  case MapSnapshot(all: Map[SystemId, MapSystemWithAll])
  case SystemSnapshot(systemId: SystemId, sys: MapSystemWithAll)
  case SystemDisplayUpdate(systemId: SystemId, name: Option[String], displayData: model.SystemDisplayData)
  case SystemRemoved(systemId: SystemId)

/** Mini-reactive/lightweight actor that has a state of the whole map in memory and makes corresponding db changes
  */
object MapEntity extends ReactiveEntity[MapEnv, MapId, MapState, Identified[MapRequest], Identified[MapResponse]]:
  override def tag = "Map"

  override def hydrate(key: MapId): URIO[MapEnv, MapState] =
    query
      .transaction(MapQueries.getMapSystemAll(key))
      .orDie
      .map(allToState)

  override def handle(
      mapId: MapId,
      state: MapState,
      in: Identified[MapRequest]
  ): URIO[MapEnv, (MapState, Chunk[Identified[MapResponse]])] =
    ZIO
      .clockWith(_.instant)
      .flatMap(now =>
        (query
          .transaction(
            in match
              case Identified(id, MapRequest.MapSnapshot) =>
                ZIO.succeed(state -> Chunk.single(Identified(id, MapResponse.MapSnapshot(state))))
              case Identified(Some(sid), add: MapRequest.AddSystem)
                  if state.get(add.systemId).forall(_.display.isEmpty) =>
                identified(sid, "add", addSystem(mapId, state, sid, now, add))
              case Identified(_, add: MapRequest.AddSystem) =>
                ZIO.logDebug(s"no-op adding existing system $add").as(state -> Chunk.empty)
              case Identified(Some(sid), addSig: MapRequest.AddSystemSignature) =>
                whenSystemExists(addSig.systemId, state)(
                  identified(sid, "addSystemSignature", upsertSystemSignature(mapId, state, sid, now, addSig))
                )
              case Identified(Some(sid), usd: MapRequest.UpdateSystemDisplay) =>
                whenSystemExists(usd.systemId, state)(
                  identified(sid, "updateDisplay", updateSystemDisplay(mapId, state, usd))
                )
              case Identified(Some(sid), usa: MapRequest.UpdateSystemAttribute) =>
                whenSystemExists(usa.systemId, state)(
                  identified(sid, "updateAttribute", updateSystemAttribute(mapId, state, sid, now, usa))
                )
              case Identified(Some(sid), uss: MapRequest.UpdateSystemSignatures) =>
                whenSystemExists(uss.systemId, state)(
                  identified(sid, "updateSystemSignatures", updateSystemSignatures(mapId, state, sid, now, uss))
                )
              case Identified(Some(sid), rs: MapRequest.RemoveSystem) =>
                whenSystemExists(rs.systemId, state)(
                  identified(sid, "removeFromDisplay", removeSystemFromDisplay(mapId, state, sid, rs))
                )
              case Identified(Some(sid), rss: MapRequest.RemoveSystemSignatures) =>
                whenSystemExists(rss.systemId, state)(
                  identified(sid, "removeSystemSignatures", removeSystemSignatures(mapId, state, sid, now, rss))
                )
              case Identified(Some(sid), rs: MapRequest.RenameSystem) =>
                whenSystemExists(rs.systemId, state)(
                  identified(sid, "renameSystem", renameSystem(mapId, state, sid, now, rs))
                )
              // fall-through case
              case Identified(None, _) =>
                ZIO.logWarning("non-identified request not processed").as(state -> Chunk.empty)
          )) @@ Log.MapId(mapId)
      )
      .orDie

  private inline def identified[R, E, A](sid: MapSessionId, op: String, f: ZIO[R, E, A]): ZIO[R, E, A] =
    f @@ Log.SessionId(sid.sessionId) @@ Log.CharacterId(sid.characterId) @@ Log.MapOperation(op)

  private def allToState(all: List[MapSystemWithAll]): MapState =
    all.map(ms => ms.sys.systemId -> ms).toMap

  private def addSystem(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      add: MapRequest.AddSystem
  ) =
    for
      curr <- query.map.getMapSystem(mapId, add.systemId) // need to get system to not override existing params
      _ <- query.map.upsertMapSystem(
        model.MapSystem(
          mapId = mapId,
          systemId = add.systemId,
          name = add.name.orElse(curr.flatMap(_.name)),
          isPinned = add.isPinned,
          chainNamingStrategy = curr.map(_.chainNamingStrategy).getOrElse(model.ChainNamingStrategy.Manual),
          description = curr.flatMap(_.description),
          stance = add.stance.orElse(curr.map(_.stance)).getOrElse(model.IntelStance.Unknown),
          updatedByCharacterId = sessionId.characterId,
          updatedAt = now
        )
      )
      _ <- query.map.upsertMapSystemDisplay(
        model.MapSystemDisplay(mapId, add.systemId, add.displayData.displayType, add.displayData)
      )
      sys <- loadSingleSystem(mapId, add.systemId)
    yield state.updated(sys.sys.systemId, sys) -> Chunk.single(
      Identified(None, MapResponse.SystemSnapshot(sys.sys.systemId, sys))
    )

  private def upsertSystemSignature(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      addSig: MapRequest.AddSystemSignature
  ) =
    for
      _ <- query.map.upsertMapSystemSignature(
        newModelSignature(now, sessionId, (mapId, addSig.systemId), addSig.signature)
      )
      sys <- loadSingleSystem(mapId, addSig.systemId)
    yield state.updated(sys.sys.systemId, sys) -> Chunk.single(
      Identified(None, MapResponse.SystemSnapshot(sys.sys.systemId, sys))
    )

  private def updateSystemDisplay(mapId: MapId, state: MapState, usd: MapRequest.UpdateSystemDisplay) =
    for
      _ <- query.map.upsertMapSystemDisplay(
        model.MapSystemDisplay(
          mapId = mapId,
          systemId = usd.systemId,
          displayType = usd.displayData.displayType,
          data = usd.displayData
        )
      )
      sys <- loadSingleSystem(mapId, usd.systemId)
      update =
        if (state(usd.systemId).display.isEmpty) MapResponse.SystemSnapshot(sys.sys.systemId, sys)
        else MapResponse.SystemDisplayUpdate(sys.sys.systemId, sys.sys.name, sys.display.get)
    yield state.updated(sys.sys.systemId, sys) -> Chunk.single(Identified(None, update))

  private def updateSystemAttribute(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      usa: MapRequest.UpdateSystemAttribute
  ) =
    for
      _   <- query.map.updateMapAttribute(mapId, usa.systemId, usa.pinned, usa.intelStance, now, sessionId.characterId)
      sys <- loadSingleSystem(mapId, usa.systemId)
    yield state.updated(sys.sys.systemId, sys) -> sys.display
      .map(displayData => Chunk.single(Identified(None, MapResponse.SystemSnapshot(sys.sys.systemId, sys))))
      .getOrElse(Chunk.empty)

  private def updateSystemSignatures(
      mapId: MapId,
      @unused state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      uss: MapRequest.UpdateSystemSignatures
  ) =
    // note: signature updates cannot currently change connection ids so only a single system needs to be reloaded
    for
      _ <-
        if (uss.replaceAll) query.map.deleteMapSystemSignatures(mapId, uss.systemId, now, sessionId.characterId)
        else ZIO.succeed(0)
      mapSystemId = (mapId, uss.systemId)
      mapSystem   = state(uss.systemId)
      _ <- ZIO.foreach(
        uss.scanned
          .map(lookupExisting(mapSystem, _))
          .map((prevOpt, newSig) => toModelSignature(now, sessionId, mapSystemId, prevOpt, newSig))
      )(query.map.upsertMapSystemSignature)
      sys <- loadSingleSystem(mapId, uss.systemId)
    yield state.updated(sys.sys.systemId, sys) -> Chunk.single(
      Identified(None, MapResponse.SystemSnapshot(sys.sys.systemId, sys))
    )

  private def removeSystemFromDisplay(
      mapId: MapId,
      state: MapState,
      @unused sessionId: MapSessionId,
      rs: MapRequest.RemoveSystem
  ) =
    for
      _   <- query.map.deleteMapSystemDisplay(mapId, rs.systemId)
      sys <- loadSingleSystem(mapId, rs.systemId)
    yield state.updated(sys.sys.systemId, sys) -> Chunk.single(
      Identified(None, MapResponse.SystemRemoved(sys.sys.systemId))
    )

  private def renameSystem(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      rs: MapRequest.RenameSystem
  ) =
    for
      _   <- query.map.updateMapSystemName(mapId, rs.systemId, rs.name, now, sessionId.characterId)
      sys <- loadSingleSystem(mapId, rs.systemId)
    yield state.updated(sys.sys.systemId, sys) -> sys.display
      .map(displayData =>
        Chunk.single(Identified(None, MapResponse.SystemDisplayUpdate(sys.sys.systemId, sys.sys.name, displayData)))
      )
      .getOrElse(Chunk.empty)

  private def removeSystemSignatures(
      mapId: MapId,
      state: MapState,
      sessionId: MapSessionId,
      now: Instant,
      rss: MapRequest.RemoveSystemSignatures
  ) =
    for
      _ <- rss.signatures match
        case None => query.map.deleteMapSystemSignatures(mapId, rss.systemId, now, sessionId.characterId)
        case Some(ids) =>
          ZIO.foreach(ids)(id => query.map.deleteMapSystemSignature(mapId, rss.systemId, id, sessionId.characterId))
      sys <- loadSingleSystem(mapId, rss.systemId)
    yield state.updated(sys.sys.systemId, sys) -> Chunk.single(
      Identified(None, MapResponse.SystemSnapshot(sys.sys.systemId, sys))
    )

  private inline def loadSingleSystem(mapId: MapId, systemId: SystemId) =
    MapQueries
      .getMapSystemAll(mapId, Some(systemId))
      .filterOrDieMessage(_.size == 1)(s"BUG: expected exactly 1 system to be returned")
      .map(_.head)

  private inline def whenSystemExists(systemId: SystemId, state: MapState)(
      f: RIO[MapEnv, (MapState, Chunk[Identified[MapResponse]])]
  ) =
    ZIO.when(state.contains(systemId))(f).map(_.getOrElse(state -> Chunk.empty)) @@ Log.SystemId(systemId)

  private inline def lookupExisting(
      prev: MapSystemWithAll,
      newSig: NewMapSystemSignature
  ): (Option[model.MapSystemSignature], NewMapSystemSignature) =
    prev.signatures.find(_.signatureId == newSig.signatureId) -> newSig

object MapReactive:
  private val MailboxSize = 128 // TODO - configurable?

  type Service = MiniReactive[MapId, Identified[MapRequest], Identified[MapResponse]]

  def layer: ZLayer[MapEnv, Nothing, Service] =
    MiniReactive.layer(MapEntity, MiniReactiveConfig(MailboxSize, 10.seconds))

private[map] def toModelSignature(
    now: Instant,
    sessionId: MapSessionId,
    mapSystem: (MapId, SystemId),
    prevOpt: Option[model.MapSystemSignature],
    newSig: NewMapSystemSignature
): model.MapSystemSignature =
  prevOpt match
    case Some(prevSig) => mergeModelSignature(now, sessionId, mapSystem, prevSig, newSig)
    case None          => newModelSignature(now, sessionId, mapSystem, newSig)

private def newModelSignature(
    now: Instant,
    sessionId: MapSessionId,
    mapSystem: (MapId, SystemId),
    newSig: NewMapSystemSignature
): model.MapSystemSignature =
  model.MapSystemSignature(
    mapId = mapSystem._1,
    systemId = mapSystem._2,
    signatureId = newSig.signatureId,
    isDeleted = false,
    signatureGroup = newSig.signatureGroup,
    signatureTypeName = newSig.signatureTypeName,
    wormholeIsEol = newSig.wormholeIsEol,
    wormholeTypeId = newSig.wormholeTypeId,
    wormholeEolAt = newSig.wormholeIsEol.filter(_ == true).map(_ => now),
    wormholeMassSize = Option.when(newSig.signatureGroup == model.SignatureGroup.Wormhole)(newSig.wormholeMassSize),
    wormholeMassStatus = Option.when(newSig.signatureGroup == model.SignatureGroup.Wormhole)(newSig.wormholeMassStatus),
    wormholeK162Type = newSig.wormholeK162Type,
    wormholeConnectionId = newSig.wormholeConnectionId,
    createdAt = now,
    createdByCharacterId = sessionId.characterId,
    updatedAt = now,
    updatedByCharacterId = sessionId.characterId
  )

private def mergeModelSignature(
    now: Instant,
    sessionId: MapSessionId,
    mapSystem: (MapId, SystemId),
    prev: model.MapSystemSignature,
    newSig: NewMapSystemSignature
): model.MapSystemSignature =
  if (newSig.signatureGroup == model.SignatureGroup.Unknown)
    prev.copy(isDeleted = false, updatedAt = now, updatedByCharacterId = sessionId.characterId)
  else if (prev.signatureGroup != newSig.signatureGroup)
    newModelSignature(now, sessionId, mapSystem, newSig).copy(
      createdAt = prev.createdAt,
      createdByCharacterId = prev.createdByCharacterId
    )
  else
    val k162Changed =
      (prev.wormholeK162Type.isDefined && prev.wormholeTypeId.isEmpty && newSig.wormholeK162Type.isEmpty && newSig.wormholeTypeId.isDefined) || (
        prev.wormholeK162Type.isEmpty && prev.wormholeTypeId.isDefined && newSig.wormholeK162Type.isDefined && newSig.wormholeTypeId.isEmpty
      )

    prev.copy(
      isDeleted = false,
      signatureTypeName = newSig.signatureTypeName.orElse(prev.signatureTypeName),
      wormholeIsEol = newSig.wormholeIsEol.orElse(prev.wormholeIsEol),
      wormholeTypeId =
        if (k162Changed) newSig.wormholeTypeId
        else newSig.wormholeTypeId.orElse(prev.wormholeTypeId) /* TODO how to represent removing the type id? */,
      wormholeEolAt =
        if (newSig.wormholeIsEol.isEmpty) prev.wormholeEolAt
        else newSig.wormholeIsEol.filter(_ == true).map(_ => now),
      wormholeMassSize =
        if (newSig.wormholeMassSize == model.WormholeMassSize.Unknown) prev.wormholeMassSize
        else Option.when(newSig.signatureGroup == model.SignatureGroup.Wormhole)(newSig.wormholeMassSize),
      wormholeMassStatus =
        if (newSig.wormholeMassStatus == model.WormholeMassStatus.Unknown) prev.wormholeMassStatus
        else Option.when(newSig.signatureGroup == model.SignatureGroup.Wormhole)(newSig.wormholeMassStatus),
      wormholeK162Type =
        if (k162Changed) newSig.wormholeK162Type
        else newSig.wormholeK162Type.orElse(prev.wormholeK162Type),
      wormholeConnectionId = newSig.wormholeConnectionId.orElse(prev.wormholeConnectionId),
      updatedAt = now,
      updatedByCharacterId = sessionId.characterId
    )
