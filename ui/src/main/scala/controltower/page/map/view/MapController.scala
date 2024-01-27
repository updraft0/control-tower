package controltower.page.map.view

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import controltower.db.ReferenceDataStore
import controltower.page.map.{Coord, MapAction, PositionController}
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
class MapController(rds: ReferenceDataStore, val pos: PositionController, val clock: Signal[Instant])(using Owner):

  // cached static data that is not assumed to change
  private val cacheSolarSystem = mutable.Map.empty[Long, SolarSystem]
  private var cacheReference   = Option.empty[Reference]

  // the main channels in/out for map updates etc.
  // TODO unsure if necessary
  val requestBus  = EventBus[MapRequest]()
  val responseBus = EventBus[MapMessage]()

  val mapMeta = Var[Option[(MapInfo, MapRole)]](None)

  val allSystems       = HVar[Map[Long, MapSystemSnapshot]](Map.empty)
  val allConnections   = HVar[Map[Long, MapWormholeConnection]](Map.empty)
  val selectedSystemId = Var[Option[Long]](None)

  val lastError = Var[Option[String]](None)

  // derived data
  val mapRole: Signal[MapRole] = mapMeta.signal.map(_.map(_._2).getOrElse(MapRole.Viewer))
  val mapSettings: Signal[MapSettings] =
    mapMeta.signal.map(_.map(_._1.settings).getOrElse(MapController.DefaultMapSettings))
  val selectedSystem: Signal[Option[MapSystemSnapshot]] = selectedSystemId.signal.combineWith(allSystems.signal).map {
    case (Some(systemId), map) => map.get(systemId)
    case (None, _)             => None
  }

  private val allSystemsChanges    = EventBus[CollectionCommand[MapSystemSnapshot]]()
  private val allConnectionChanges = EventBus[CollectionCommand[MapWormholeConnection]]()

  val allSystemsChangesStream    = allSystemsChanges.events
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
  allSystems.writeChangesTo(allSystemsChanges.writer)
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
    Var.update(
      allSystems.current     -> Map.empty,
      allConnections.current -> Map.empty
    )

  private def handleMapAction(op: EventStream[MapAction]): EventStream[MapRequest] =
    op.withCurrentValueOf(allSystems.current)
      .collectOpt:
        case (MapAction.AddSignature(systemId, newSig), _) => Some(MapRequest.AddSystemSignature(systemId, newSig))
        case (MapAction.Direct(req), _)                    => Some(req)
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
        case (MapAction.RemoveSignatures(systemId, sigIds), _) =>
          Some(MapRequest.RemoveSystemSignatures(systemId, sigIds.toList))
        case (MapAction.RemoveAllSignatures(systemId), _) =>
          Some(MapRequest.RemoveAllSystemSignatures(systemId))
        case (MapAction.Reposition(systemId, x, y), allSystemsNow) =>
          val newDisplayData =
            pos.updateDisplayFromPosition(systemId, allSystemsNow.get(systemId).flatMap(_.display), Coord(x, y))
          allSystems.current
            .update(map => map.updatedWith(systemId)(_.map(mss => mss.copy(display = Some(newDisplayData)))))
          Some(MapRequest.UpdateSystem(systemId, displayData = Some(newDisplayData)))
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
      case MapMessage.Error(text)         => lastError.set(Some(text))
      case MapMessage.MapMeta(info, role) => mapMeta.set(Some(info -> role))
      case MapMessage.MapSnapshot(systems, connections) =>
        inline def doUpdate() =
          Var.set(
            allSystems.current     -> systems.map(mss => mss.system.systemId -> mss).toMap,
            allConnections.current -> connections
          )

        // get the reference data from the system in cache (manually using future)
        val systemIds        = systems.map(_.system.systemId).toSet
        val missingSystemIds = systemIds -- cacheSolarSystem.keys

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
          allSystems.current -> ((map: Map[Long, MapSystemSnapshot]) => map.removed(systemId)),
          selectedSystemId   -> ((sOpt: Option[Long]) => sOpt.filterNot(_ == systemId))
        )
      case MapMessage.SystemSnapshot(systemId, system, connections) =>
        inline def doUpdate(@unused solarSystem: SolarSystem) =
          Var.update(
            allSystems.current -> ((map: Map[Long, MapSystemSnapshot]) => map.updated(systemId, system)),
            allConnections.current -> ((conns: Map[Long, MapWormholeConnection]) =>
              connections.foldLeft(conns) { case (conns, (cid, whc)) =>
                conns.updated(cid, whc)
              }
            )
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
          (allSystems.current, updateInMap(systemId, updateDisplay(_))(_))
        )

object MapController:
  private val DefaultMapSettings = MapSettings(staleScanThreshold = Duration.ofHours(24))
