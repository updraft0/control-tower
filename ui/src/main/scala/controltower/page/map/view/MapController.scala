package controltower.page.map.view

import com.raquo.airstream.state.Var.{VarModTuple, VarTuple}
import com.raquo.laminar.api.L.*
import controltower.db.ReferenceDataStore
import controltower.page.map.{MapAction, MapActionError, RoleController, VarPositionController}
import controltower.ui.{Coord, HVar, writeChangesTo}
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.*

import java.time.{Duration, Instant}
import scala.annotation.unused
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success}

sealed trait ContextMenuRequestPosition:
  val mousePosition: Coord

enum ContextMenuRequest extends ContextMenuRequestPosition:
  case System(mousePosition: Coord, systemId: SystemId)
  case Connection(mousePosition: Coord, connectionId: ConnectionId)
  case Generic(mousePosition: Coord)

/** Map system selections tate
  */
enum SystemSelectionState derives CanEqual:
  case None
  case Single(system: MapSystemSnapshot)
  case Multiple(ids: Array[SystemId])

/** Events that only impact the UI state rather than the map itself
  */
enum MapUiEvent derives CanEqual:
  case ContextMenu(menu: ContextMenuRequest)
  // bring up dialogs
  case AddSystemDialog
  case RemoveSystemSelectionDialog(selection: SystemSelectionState)
  case RemoveConnectionDialog(connection: MapWormholeConnectionWithSigs)

/** Signal-based version of [[controltower.page.map.RoleController]]
  */
trait ActionRoleController:
  val canAddSystem: Signal[Boolean]
  val canEditSignatures: Signal[Boolean]
  val canRemoveSystem: Signal[Boolean]
  val canRenameSystem: Signal[Boolean]
  val canRepositionSystem: Signal[Boolean]
  val canPinUnpinSystem: Signal[Boolean]
  val canUpdateIntelStance: Signal[Boolean]
  val canChangeConnections: Signal[Boolean]

/** Single place where the state of the map is tracked (without rendering)
  */
final class MapController(val rds: ReferenceDataStore, val clock: Signal[Instant], val isConnected: Signal[Boolean])(
    using Owner
):

  // cached static data that is not assumed to change
  private val cacheSolarSystem = mutable.Map.empty[SystemId, SolarSystem]
  private var cacheReference   = Option.empty[Reference]

  // TODO: this needs to be always in sync with css :/
  val BoxSize = Coord(MagicConstant.SystemBoxSizeX, MagicConstant.SystemBoxSizeY)

  val pos = new VarPositionController(mutable.Map.empty, BoxSize)

  val requestBus  = EventBus[MapRequest]()
  val responseBus = EventBus[MapMessage]()

  val mapMeta      = Var[Option[MapMessage.MapMeta]](None)
  val serverStatus = Var[MapServerStatus](MapServerStatus.Error)

  val allSystems     = HVar[Map[SystemId, MapSystemSnapshot]](Map.empty)
  val allConnections = HVar[Map[ConnectionId, MapWormholeConnectionWithSigs]](Map.empty)
  val allLocations   = HVar[Map[SystemId, Array[CharacterLocation]]](Map.empty)

  val selectedSystemId      = Var[Option[SystemId]](None)
  val bulkSelectedSystemIds = Var[Array[SystemId]](Array.empty[SystemId])
  val selectedConnectionId  = Var[Option[ConnectionId]](None)

  val mapUiEvents = EventBus[MapUiEvent]()

  // TODO: display errors somewhere
  val actionErrors = EventBus[MapActionError]()
  val lastError    = Var[Option[String]](None)

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

  val roleController: ActionRoleController = new ActionRoleController:
    override val canAddSystem: Signal[Boolean] =
      mapRole.map(RoleController.canAddSystem).combineWith(isConnected).map(_ && _)
    override val canEditSignatures: Signal[Boolean] =
      mapRole.map(RoleController.canEditSignatures).combineWith(isConnected).map(_ && _)
    override val canRemoveSystem: Signal[Boolean] =
      mapRole.map(RoleController.canRemoveSystem).combineWith(isConnected).map(_ && _)
    override val canRenameSystem: Signal[Boolean] =
      mapRole.map(RoleController.canRenameSystem).combineWith(isConnected).map(_ && _)
    override val canRepositionSystem: Signal[Boolean] =
      mapRole.map(RoleController.canRepositionSystem).combineWith(isConnected).map(_ && _)
    override val canPinUnpinSystem: Signal[Boolean] =
      mapRole.map(RoleController.canPinUnpinSystem).combineWith(isConnected).map(_ && _)
    override val canUpdateIntelStance: Signal[Boolean] =
      mapRole.map(RoleController.canUpdateIntelStance).combineWith(isConnected).map(_ && _)
    override val canChangeConnections: Signal[Boolean] =
      mapRole.map(RoleController.canChangeConnections).combineWith(isConnected).map(_ && _)

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

  // TODO: use this instead of the multiple selection support
  val systemSelectionState = selectedSystem.signal
    .combineWith(bulkSelectedSystemIds)
    .map:
      case (_, arr) if arr.nonEmpty => SystemSelectionState.Multiple(arr)
      case (Some(system), _)        => SystemSelectionState.Single(system)
      case (_, _)                   => SystemSelectionState.None

  private val allSystemChanges     = EventBus[CollectionCommand[MapSystemSnapshot]]()
  private val allConnectionChanges = EventBus[CollectionCommand[MapWormholeConnectionWithSigs]]()

  val allSystemChangesStream     = allSystemChanges.events
  val allConnectionChangesStream = allConnectionChanges.events

  def connectionSignal(connectionId: ConnectionId): Signal[Option[MapWormholeConnectionWithSigs]] =
    allConnections.signal.map(_.get(connectionId))

  def systemSignal(systemId: SystemId): Signal[Option[MapSystemSnapshot]] =
    allSystems.signal.map(_.get(systemId))

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

      override def systemName(systemId: SystemId) = self.allSystems.signal.map(_.get(systemId).flatMap(_.system.name))
      override def connection(id: ConnectionId)   = self.allConnections.signal.map(_.get(id))

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

  private inline def updateConnectionSignatureAttr(
      all: Map[ConnectionId, MapWormholeConnectionWithSigs],
      cId: ConnectionId,
      f: MapSystemSignature.Wormhole => MapSystemSignature.Wormhole
  ) =
    all
      .get(cId)
      .flatMap: wss =>
        (wss.fromSignature, wss.toSignature) match
          case (Some(from), _) =>
            Some(MapRequest.AddSystemSignature(wss.connection.fromSystemId, f(from).asNew))
          case (_, Some(to)) =>
            Some(MapRequest.AddSystemSignature(wss.connection.toSystemId, f(to).asNew))
          case (None, None) =>
            actionErrors.writer.onNext(MapActionError.UnableToChangeConnectionNoLinkedSignature)
            None

  private def handleMapAction(op: EventStream[MapAction]): EventStream[MapRequest] =
    op.withCurrentValueOf(allSystems.current, allConnections.current)
      .collectOpt:
        case (MapAction.AddSignature(systemId, newSig), _, _) =>
          Some(MapRequest.AddSystemSignature(systemId, newSig))
        case (MapAction.AddConnection(fromSystemId, toSystemId), _, _) =>
          Some(MapRequest.AddSystemConnection(SystemId(fromSystemId), SystemId(toSystemId)))
        case (MapAction.ConnectionEolToggle(connectionId), _, allConnections) =>
          updateConnectionSignatureAttr(
            allConnections,
            connectionId,
            mss => if (mss.eolAt.isEmpty) mss.copy(eolAt = Some(Instant.now())) else mss.copy(eolAt = None)
          )
        case (MapAction.ConnectionMassStatusChange(connectionId, massStatus), _, allConnections) =>
          updateConnectionSignatureAttr(
            allConnections,
            connectionId,
            _.copy(massStatus = massStatus)
          )
        case (MapAction.ConnectionMassSizeChange(connectionId, massSize), _, allConnections) =>
          updateConnectionSignatureAttr(
            allConnections,
            connectionId,
            _.copy(massSize = massSize)
          )
        case (MapAction.Direct(req), _, _) =>
          Some(req)
        case (MapAction.IntelChange(systemId, newStance), allSystems, _) =>
          allSystems
            .get(systemId)
            .flatMap(mss =>
              Option.when(mss.system.stance != newStance)(MapRequest.UpdateSystem(systemId, stance = Some(newStance)))
            )
        case (MapAction.Rename(systemId, newName), _, _) =>
          Some(MapRequest.UpdateSystem(systemId, name = Some(newName)))
        case (MapAction.Remove(systemId), _, _) =>
          Some(MapRequest.RemoveSystem(systemId))
        case (MapAction.RemoveMultiple(systemIds), _, _) =>
          // reset selection
          bulkSelectedSystemIds.set(Array.empty)
          Some(MapRequest.RemoveSystems(systemIds))
        case (MapAction.RemoveConnection(connectionId), _, _) =>
          Some(MapRequest.RemoveSystemConnection(connectionId))
        case (MapAction.RemoveSignatures(systemId, sigIds), _, _) =>
          Some(MapRequest.RemoveSystemSignatures(systemId, sigIds.toList))
        case (MapAction.RemoveAllSignatures(systemId), _, _) =>
          Some(MapRequest.RemoveAllSystemSignatures(systemId))
        case (MapAction.Reposition(systemId, x, y), allSystemsNow, _) =>
          val displayData = pos.systemDisplayData(systemId).now()
          Some(MapRequest.UpdateSystem(systemId, displayData))
        case (MapAction.UpdateSignatures(systemId, replaceAll, scanned), _, _) =>
          Some(MapRequest.UpdateSystemSignatures(systemId, replaceAll, scanned))
        case (MapAction.Select(systemIdOpt), _, _) =>
          Var.set(
            (selectedSystemId, systemIdOpt),
            (selectedConnectionId, None)
          )
          None
        case (MapAction.SelectUnpinned, allSystems, _) =>
          bulkSelectedSystemIds.set(allSystems.values.filterNot(_.system.isPinned).map(_.system.systemId).toArray)
          None
        case (MapAction.ToggleBulkSelection(systemId), _, _) =>
          bulkSelectedSystemIds.update(arr =>
            arr.indexOf(SystemId(systemId)) match
              case -1  => arr.appended(SystemId(systemId))
              case idx => arr.filterNot(_ == SystemId(systemId))
          )
          None
        case (MapAction.TogglePinned(systemId), allSystems, _) =>
          allSystems.get(systemId).map(sys => MapRequest.UpdateSystem(systemId, isPinned = Some(!sys.system.isPinned)))

  private def handleIncomingMessage(msg: MapMessage): Unit =
    inline def updateInMap(systemId: SystemId, inline upd: MapSystemSnapshot => MapSystemSnapshot)(
        systems: Map[SystemId, MapSystemSnapshot]
    ): Map[SystemId, MapSystemSnapshot] = systems.updatedWith(systemId)(_.map(upd))

    msg match
      case MapMessage.ConnectionSnapshot(whc) =>
        Var.update(
          allSystems.current -> ((map: Map[SystemId, MapSystemSnapshot]) =>
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
          allSystems.current -> ((map: Map[SystemId, MapSystemSnapshot]) =>
            whcs.foldLeft(map): (m, whc) =>
              m
                .updatedWith(whc.fromSystemId)(
                  _.map(mss =>
                    mss.copy(
                      signatures = mss.signatures.filterNot:
                        case w: MapSystemSignature.Wormhole => w.connectionId.contains(whc.id)
                        case _                              => false
                      ,
                      connections = mss.connections.filterNot(_.id == whc.id)
                    )
                  )
                )
                .updatedWith(whc.toSystemId)(
                  _.map(mss =>
                    mss.copy(
                      signatures = mss.signatures.filterNot:
                        case w: MapSystemSignature.Wormhole => w.connectionId.contains(whc.id)
                        case _                              => false
                      ,
                      connections = mss.connections.filterNot(_.id == whc.id)
                    )
                  )
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
          allSystems.current -> ((map: Map[SystemId, MapSystemSnapshot]) =>
            updatedSystems.foldLeft(map.removedAll(removedSystemIds)) { case (st, mss) =>
              st.updated(mss.system.systemId, mss)
            }
          ),
          selectedSystemId      -> ((sOpt: Option[SystemId]) => sOpt.filterNot(removedSystemIds.contains)),
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
            allSystems.current -> ((map: Map[SystemId, MapSystemSnapshot]) => map.updated(systemId, system)),
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
