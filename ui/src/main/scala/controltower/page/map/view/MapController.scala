package controltower.page.map.view

import com.raquo.airstream.state.Var.{VarModTuple, VarTuple}
import com.raquo.laminar.api.L.*
import controltower.db.ReferenceDataStore
import controltower.page.map.{Coord, MapAction, VarPositionController}
import controltower.ui.{HVar, writeChangesTo}
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.{SystemId => _, *}

import java.time.{Duration, Instant}
import scala.annotation.unused
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success}

/** Single place where the state of the map is tracked (without rendering)
  */
class MapController(rds: ReferenceDataStore, val clock: Signal[Instant])(using Owner):

  // cached static data that is not assumed to change
  private val cacheSolarSystem = mutable.Map.empty[Long, SolarSystem]
  private var cacheReference   = Option.empty[Reference]

  // TODO: this needs to be always in sync with css :/
  val BoxSize = Coord(140, 40)

  val pos = new VarPositionController(mutable.Map.empty, BoxSize)

  val requestBus  = EventBus[MapRequest]()
  val responseBus = EventBus[MapMessage]()

  val mapMeta      = Var[Option[MapMessage.MapMeta]](None)
  val serverStatus = Var[MapServerStatus](MapServerStatus.Error)

  val allSystems     = HVar[Map[Long, MapSystemSnapshot]](Map.empty)
  val allConnections = HVar[Map[ConnectionId, MapWormholeConnectionWithSigs]](Map.empty)
  val allLocations   = HVar[Map[SystemId, Array[CharacterLocation]]](Map.empty)

  val selectedSystemId      = Var[Option[Long]](None)
  val bulkSelectedSystemIds = Var[Array[SystemId]](Array.empty[SystemId])
  val selectedConnectionId  = Var[Option[ConnectionId]](None)

  val lastError = Var[Option[String]](None)

  // derived data
  val mapMetaSignal: Signal[MapMessage.MapMeta] = mapMeta.signal.map(_.get).recoverIgnoreErrors

  val mapRole: Signal[MapRole] = mapMeta.signal.map(_.map(_.role).getOrElse(MapRole.Viewer))
  val mapId: Signal[MapId]     = mapMeta.signal.map(_.map(_.info.id).getOrElse(MapId.Invalid))
  val userPreferences: Signal[UserPreferences] =
    mapMeta.signal.map(_.map(_.preferences).getOrElse(UserPreferences.Default))
  val characterId: Signal[CharacterId] =
    mapMeta.signal.map(_.map(_.character.characterId).getOrElse(CharacterId.Invalid))
  val mapSettings: Signal[MapSettings] =
    mapMeta.signal.map(_.map(_.info.settings).getOrElse(MapController.DefaultMapSettings))

  val selectedSystem: Signal[Option[MapSystemSnapshot]] =
    selectedSystemId.signal
      .combineWith(allSystems.signal)
      .map:
        case (Some(systemId), map) => map.get(systemId)
        case (None, _)             => None

  val selectedConnection: Signal[Option[MapWormholeConnectionWithSigs]] =
    selectedConnectionId.signal
      .combineWith(allConnections.signal)
      .map:
        case (Some(connectionId), conns) => conns.get(connectionId)
        case (None, _)                   => None

  private val allSystemChanges     = EventBus[CollectionCommand[MapSystemSnapshot]]()
  private val allConnectionChanges = EventBus[CollectionCommand[MapWormholeConnectionWithSigs]]()

  val allSystemChangesStream     = allSystemChanges.events
  val allConnectionChangesStream = allConnectionChanges.events

  // observers etc:
  val actionsBus: WriteBus[MapAction] = requestBus.writer.contracomposeWriter[MapAction](handleMapAction)

  rds
    .referenceAll()
    .onComplete:
      case Success(refAll) => cacheReference = Some(refAll)
      case Failure(ex) =>
        lastError.set(Some("Failed to lookup reference values"))
        org.scalajs.dom.console.error(s"Failed to lookup reference values: ${ex}")

  responseBus.events.addObserver(Observer(handleIncomingMessage(_)))
  allSystems.writeChangesTo(allSystemChanges.writer)
  allConnections.writeChangesTo(allConnectionChanges.writer)

  // impls
  def context: MapViewContext =
    val self = this
    new MapViewContext:
      override def actions         = self.actionsBus
      override def characterId     = self.characterId
      override def mapRole         = self.mapRole
      override def staticData      = SystemStaticData(cacheSolarSystem.view, cacheReference.get)
      override def now             = self.clock
      override def userPreferences = self.userPreferences

      override def systemName(systemId: Long)   = self.allSystems.signal.map(_.get(systemId).flatMap(_.system.name))
      override def connection(id: ConnectionId) = self.allConnections.signal.map(_.get(id))

  def clear(): Unit =
    Var.set(
      allSystems.current     -> Map.empty,
      allConnections.current -> Map.empty,
      allLocations.current   -> Map.empty,
      selectedSystemId       -> Option.empty,
      selectedConnectionId   -> Option.empty,
      serverStatus           -> MapServerStatus.Error
    )
    pos.clear()

  private def handleMapAction(op: EventStream[MapAction]): EventStream[MapRequest] =
    op.withCurrentValueOf(allSystems.current)
      .collectOpt:
        case (MapAction.AddSignature(systemId, newSig), _) => Some(MapRequest.AddSystemSignature(systemId, newSig))
        case (MapAction.AddConnection(fromSystemId, toSystemId), _) =>
          Some(MapRequest.AddSystemConnection(SystemId(fromSystemId), SystemId(toSystemId)))
        case (MapAction.Direct(req), _) => Some(req)
        case (MapAction.IntelChange(systemId, newStance), allSystems) =>
          allSystems
            .get(systemId)
            .flatMap(mss =>
              Option.when(mss.system.stance != newStance)(MapRequest.UpdateSystem(systemId, stance = Some(newStance)))
            )
        case (MapAction.Rename(systemId, newName), _) =>
          Some(
            MapRequest
              .UpdateSystem(systemId, name = Some(newName.map(NewSystemName.Name(_)).getOrElse(NewSystemName.None)))
          )
        case (MapAction.Remove(systemId), _) =>
          Some(MapRequest.RemoveSystem(systemId))
        case (MapAction.RemoveMultiple(systemIds), _) =>
          // reset selection
          bulkSelectedSystemIds.set(Array.empty)
          Some(MapRequest.RemoveSystems(systemIds))
        case (MapAction.RemoveConnection(connectionId), _) =>
          Some(MapRequest.RemoveSystemConnection(connectionId))
        case (MapAction.RemoveSignatures(systemId, sigIds), _) =>
          Some(MapRequest.RemoveSystemSignatures(systemId, sigIds.toList))
        case (MapAction.RemoveAllSignatures(systemId), _) =>
          Some(MapRequest.RemoveAllSystemSignatures(systemId))
        case (MapAction.Reposition(systemId, x, y), allSystemsNow) =>
          val displayData = pos.systemDisplayData(systemId).now()
          Some(MapRequest.UpdateSystem(systemId, displayData))
        case (MapAction.UpdateSignatures(systemId, replaceAll, scanned), _) =>
          Some(MapRequest.UpdateSystemSignatures(systemId, replaceAll, scanned))
        case (MapAction.Select(systemIdOpt), _) =>
          Var.set(
            (selectedSystemId, systemIdOpt),
            (selectedConnectionId, None)
          )
          None
        case (MapAction.ToggleBulkSelection(systemId), _) =>
          bulkSelectedSystemIds.update(arr =>
            arr.indexOf(SystemId(systemId)) match
              case -1  => arr.appended(SystemId(systemId))
              case idx => arr.filterNot(_ == SystemId(systemId))
          )
          None
        case (MapAction.TogglePinned(systemId), allSystems) =>
          allSystems.get(systemId).map(sys => MapRequest.UpdateSystem(systemId, isPinned = Some(!sys.system.isPinned)))

  private def handleIncomingMessage(msg: MapMessage): Unit =
    inline def updateInMap(systemId: Long, inline upd: MapSystemSnapshot => MapSystemSnapshot)(
        systems: Map[Long, MapSystemSnapshot]
    ): Map[Long, MapSystemSnapshot] = systems.updatedWith(systemId)(_.map(upd))

    msg match
      case MapMessage.ConnectionSnapshot(whc) =>
        Var.update(
          allSystems.current -> ((map: Map[Long, MapSystemSnapshot]) =>
            map
              .updatedWith(whc.connection.fromSystemId)(
                _.map(mss => mss.copy(connections = updateConnectionById(mss.connections, whc.connection)))
              )
              .updatedWith(whc.connection.toSystemId)(
                _.map(mss => mss.copy(connections = updateConnectionById(mss.connections, whc.connection)))
              )
          ),
          allConnections.current -> ((conns: Map[ConnectionId, MapWormholeConnectionWithSigs]) =>
            conns.updated(whc.connection.id, whc)
          )
        )

      case MapMessage.ConnectionsRemoved(whcs) =>
        Var.update(
          allSystems.current -> ((map: Map[Long, MapSystemSnapshot]) =>
            whcs.foldLeft(map): (m, whc) =>
              m
                .updatedWith(whc.fromSystemId)(
                  _.map(mss => mss.copy(connections = mss.connections.filterNot(_.id == whc.id)))
                )
                .updatedWith(whc.toSystemId)(
                  _.map(mss => mss.copy(connections = mss.connections.filterNot(_.id == whc.id)))
                )
          ),
          allConnections.current -> ((conns: Map[ConnectionId, MapWormholeConnectionWithSigs]) =>
            conns.removedAll(whcs.map(_.id))
          )
        )
      case MapMessage.Error(text)   => lastError.set(Some(text))
      case meta: MapMessage.MapMeta => mapMeta.set(Some(meta))
      case MapMessage.MapSnapshot(systems, connections) =>
        inline def doUpdate() =
          Var.set(
            allSystems.current     -> systems,
            allConnections.current -> connections
          )
          Var.set(
            systems.view.values
              .map(mss => (pos.systemDisplayData(mss.system.systemId) -> mss.display): VarTuple[_])
              .toSeq*
          )

        // get the reference data from the system in cache (manually using future)
        val missingSystemIds = systems.keySet -- cacheSolarSystem.keys

        if (missingSystemIds.nonEmpty)
          Future
            .sequence(missingSystemIds.map(rds.systemForId))
            .map { resolvedSystems =>
              resolvedSystems.foreach {
                case Some(ss) => cacheSolarSystem.update(ss.id, ss)
                case _        => // no-op
              }
            }
            .onComplete(_ => doUpdate())
        else doUpdate()
      case MapMessage.SystemsRemoved(removedSystemIds, removedConnectionIds, updatedSystems, updatedConnections) =>
        Var.update(
          allSystems.current -> ((map: Map[Long, MapSystemSnapshot]) =>
            updatedSystems.foldLeft(map.removedAll(removedSystemIds)) { case (st, mss) =>
              st.updated(mss.system.systemId, mss)
            }
          ),
          selectedSystemId      -> ((sOpt: Option[Long]) => sOpt.filterNot(removedSystemIds.contains)),
          bulkSelectedSystemIds -> ((selection: Array[SystemId]) => selection.filterNot(removedSystemIds.contains)),
          allConnections.current -> ((conns: Map[ConnectionId, MapWormholeConnectionWithSigs]) =>
            updatedConnections.foldLeft(conns.removedAll(removedConnectionIds)) { case (conns, (cid, whc)) =>
              conns.updated(cid, whc)
            }
          )
        )
        Var.update(
          removedSystemIds.toSeq.map(systemId =>
            pos.systemDisplayData(systemId) -> ((_: Option[SystemDisplayData]) => None)
          )*
        )
      case MapMessage.SystemSnapshot(systemId, system, connections) =>
        inline def doUpdate(@unused solarSystem: SolarSystem) =
          Var.update(
            allSystems.current -> ((map: Map[Long, MapSystemSnapshot]) => map.updated(systemId, system)),
            allConnections.current -> ((conns: Map[ConnectionId, MapWormholeConnectionWithSigs]) =>
              connections.foldLeft(conns) { case (conns, (cid, whc)) =>
                conns.updated(cid, whc)
              }
            ),
            pos.systemDisplayData(systemId) -> ((_: Option[SystemDisplayData]) => system.display)
          )

        if (!cacheSolarSystem.contains(systemId))
          rds.systemForId(systemId).onComplete {
            case Success(Some(solarSystem)) =>
              cacheSolarSystem.update(systemId, solarSystem)
              doUpdate(solarSystem)
            case _ =>
              lastError.set(Some(s"Internal error updating solar system $systemId static data"))
              org.scalajs.dom.console.error(s"unable to fetch solar system $systemId from cache")
          }
        else doUpdate(cacheSolarSystem(systemId))
      case MapMessage.SystemDisplayUpdate(systemId, name, displayData) =>
        inline def updateDisplay(mss: MapSystemSnapshot) =
          mss.copy(system = mss.system.copy(name = name), display = Some(displayData))
        Var.update(
          (allSystems.current, updateInMap(systemId, updateDisplay(_))(_)),
          (pos.systemDisplayData(systemId), (_: Option[SystemDisplayData]) => Some(displayData))
        )
      case MapMessage.CharacterLocations(locations) =>
        Var.update(
          (allLocations.current, (_: Map[SystemId, Array[CharacterLocation]]) => locations)
        )
      case MapMessage.ConnectionJumped(jumpInfo) =>
        Var.update(
          (
            allConnections.current,
            (conns: Map[ConnectionId, MapWormholeConnectionWithSigs]) =>
              conns.updatedWith(jumpInfo.connectionId) {
                case None       => None // this should not happen
                case Some(whcs) => Some(whcs.copy(jumps = whcs.jumps :+ jumpInfo))
              }
          )
        )
      case MapMessage.ServerStatus(status) => serverStatus.set(status)

object MapController:
  private val DefaultMapSettings = MapSettings(staleScanThreshold = Duration.ofHours(6))

private inline def updateConnectionById(
    connections: Array[MapWormholeConnection],
    whc: MapWormholeConnection
): Array[MapWormholeConnection] =
  connections.indexWhere(_.id == whc.id) match
    case -1 => connections.appended(whc)
    case idx =>
      connections.update(idx, whc)
      connections
