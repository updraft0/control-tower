package controltower.page.map.view

import com.raquo.airstream.state.Var.{VarModTuple, VarTuple}
import com.raquo.laminar.api.L.*
import controltower.db.ReferenceDataStore
import controltower.page.map.{Coord, MapAction, VarPositionController}
import controltower.ui.{HVar, writeChangesTo}
import org.updraft0.controltower.protocol.*

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

  val mapMeta = Var[Option[(MapInfo, MapRole)]](None)

  val allSystems     = HVar[Map[Long, MapSystemSnapshot]](Map.empty)
  val allConnections = HVar[Map[Long, MapWormholeConnectionWithSigs]](Map.empty)

  val selectedSystemId     = Var[Option[Long]](None)
  val selectedConnectionId = Var[Option[Long]](None)

  val lastError = Var[Option[String]](None)

  // derived data
  val mapRole: Signal[MapRole] = mapMeta.signal.map(_.map(_._2).getOrElse(MapRole.Viewer))
  val mapSettings: Signal[MapSettings] =
    mapMeta.signal.map(_.map(_._1.settings).getOrElse(MapController.DefaultMapSettings))

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
      override def actions    = self.actionsBus
      override def mapRole    = self.mapRole
      override def staticData = SystemStaticData(cacheSolarSystem.view, cacheReference.get)
      override def now        = self.clock

  def clear(): Unit =
    Var.set(
      allSystems.current     -> Map.empty,
      allConnections.current -> Map.empty,
      selectedSystemId       -> Option.empty,
      selectedConnectionId   -> Option.empty
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
          selectedSystemId.set(systemIdOpt)
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
          allConnections.current -> ((conns: Map[Long, MapWormholeConnectionWithSigs]) =>
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
          allConnections.current -> ((conns: Map[Long, MapWormholeConnectionWithSigs]) =>
            conns.removedAll(whcs.map(_.id))
          )
        )
      case MapMessage.Error(text)         => lastError.set(Some(text))
      case MapMessage.MapMeta(info, role) => mapMeta.set(Some(info -> role))
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
      case MapMessage.SystemRemoved(systemId) =>
        // TODO: what happens to the connections in this case :/
        Var.update(
          allSystems.current              -> ((map: Map[Long, MapSystemSnapshot]) => map.removed(systemId)),
          selectedSystemId                -> ((sOpt: Option[Long]) => sOpt.filterNot(_ == systemId)),
          pos.systemDisplayData(systemId) -> ((_: Option[SystemDisplayData]) => None)
        )
      case MapMessage.SystemSnapshot(systemId, system, connections) =>
        inline def doUpdate(@unused solarSystem: SolarSystem) =
          Var.update(
            allSystems.current -> ((map: Map[Long, MapSystemSnapshot]) => map.updated(systemId, system)),
            allConnections.current -> ((conns: Map[Long, MapWormholeConnectionWithSigs]) =>
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

object MapController:
  private val DefaultMapSettings = MapSettings(staleScanThreshold = Duration.ofHours(24))

private inline def updateConnectionById(
    connections: Array[MapWormholeConnection],
    whc: MapWormholeConnection
): Array[MapWormholeConnection] =
  connections.indexWhere(_.id == whc.id) match
    case -1 => connections.appended(whc)
    case idx =>
      connections.update(idx, whc)
      connections