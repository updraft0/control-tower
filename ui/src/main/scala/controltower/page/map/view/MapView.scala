package controltower.page.map.view

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.{ReactiveElement, ReactiveHtmlElement}
import controltower.Page
import controltower.backend.ControlTowerBackend
import controltower.db.ReferenceDataStore
import controltower.page.map.*
import controltower.page.map.view.*
import controltower.ui.*
import io.laminext.websocket.*
import org.updraft0.controltower.protocol.*

import scala.annotation.unused
import scala.collection.immutable.SeqMap
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success
import scala.language.implicitConversions

private class MapView(
    id: Int,
    @unused ct: ControlTowerBackend,
    rds: ReferenceDataStore,
    ws: WebSocket[MapMessage, MapRequest],
    @unused mapName: String,
    @unused characterId: Long
) extends ViewController:

  private val cacheSolarSystem = mutable.Map.empty[Long, SolarSystem]
  private var cacheReference   = Option.empty[Reference]

  private val allSystems     = FakeMapVar[Long, MapSystemSnapshot]()
  private val requestBus     = EventBus[MapRequest]()
  private val mapTop         = Var[Option[Element]](None)
  private val selectedSystem = Var[Option[Long]](None)
  private val mapMeta        = Var[Option[(MapInfo, MapRole)]](None)

  override def view: Element = div(
    idAttr := s"map-controller-view-${id}",
    child.maybe <-- mapTop,
    connect,
    ws.connect
  )

  private def connect[El <: HtmlElement]: Binder[El] =
    (element: El) =>
      ReactiveElement.bindSubscriptionUnsafe(element) { ctx =>
        binderStarted(ctx.owner)
        new Subscription(ctx.owner, cleanup = () => binderStopped)
      }

  private def handleIncoming(msg: WebSocketEvent[MapMessage]): Unit =
    msg match
      case WebSocketEvent.Received(msg) => handleIncomingMessage(msg)
      case WebSocketEvent.Error(ex)     => org.scalajs.dom.console.error(s"Unhandled error in MapControllerView: ${ex}")
      case _                            => // no-op
  /* */
  private def handleIncomingMessage(msg: MapMessage): Unit =
    msg match
      case MapMessage.MapMeta(info, role)     => mapMeta.set(Some(info -> role))
      case MapMessage.MapSnapshot(systems, _) =>
        // get the reference data from the system in cache (manually using future)
        val systemIds        = systems.map(_.system.systemId).toSet
        val missingSystemIds = systemIds -- cacheSolarSystem.keys

        Future
          .sequence(missingSystemIds.map(rds.systemForId))
          .map { resolvedSystems =>
            resolvedSystems.foreach {
              case Some(ss) => cacheSolarSystem.update(ss.id, ss)
              case _        => // no-op
            }
          }
          .onComplete(_ => allSystems.update(_ => SeqMap.from(systems.map(s => s.system.systemId -> s))))
      case MapMessage.SystemRemoved(systemId) =>
        allSystems.removeKey(systemId)
        // can't update in transaction because not real var but ok whatever
        selectedSystem.update {
          case Some(`systemId`) => None
          case other            => other
        }

      case MapMessage.SystemSnapshot(systemId, system, _) =>
        if (!cacheSolarSystem.contains(systemId))
          rds.systemForId(systemId).onComplete {
            case Success(Some(solarSystem)) =>
              cacheSolarSystem.update(systemId, solarSystem)
              allSystems.update(_.updated(systemId, system))
            case _ => org.scalajs.dom.console.error(s"unable to fetch solar system $systemId from cache")
          }
        else allSystems.update(_.updated(systemId, system))
      case MapMessage.SystemDisplayUpdate(systemId, name, displayData) =>
        allSystems.update(
          _.updatedWith(systemId)(
            _.map(mss => mss.copy(system = mss.system.copy(name = name), display = Some(displayData)))
          )
        )
  /* */
  private def binderStarted(owner: Owner): Unit =
    // subscribe output and input
    requestBus.events.addObserver(ws.send)(using owner)
    ws.events.foreach(handleIncoming)(using owner)
    // get the static reference data
    rds.referenceAll().foreach { ref =>
      cacheReference = Some(ref)

      // render the top element (FUGLY FIXME)
      mapTop.set(Some(renderTop(using owner)))
    }

    // get the initial map snapshot
    requestBus.emit(MapRequest.GetSnapshot)

  private def binderStopped: Unit =
    org.scalajs.dom.console.debug("stopped map view controller")
    allSystems.update(_ => SeqMap.empty)
    mapTop.set(None)
    // do not clear the caches of static data?

  private def renderTop(using Owner) =
    val mapRole = mapMeta.signal.map(_.map(_._2).getOrElse(MapRole.Viewer))
    val static = SystemStaticData(
      cacheSolarSystem.view,
      cacheReference.map(_.wormholeTypes.map(wt => wt.typeId -> wt).toMap).getOrElse(Map.empty)
    )
    val selectedSystemSnapshot =
      selectedSystem.signal.distinct.combineWithFn(allSystems.signal) {
        case (Some(systemId), allMap) => allMap.get(systemId)
        case _                        => None
      }

    val actions = requestBus.writer.contracomposeWriter[MapAction](op =>
      op.withCurrentValueOf(allSystems.signal).collectOpt {
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
        case (MapAction.Reposition(systemId, x, y), allSystems) =>
          val newDisplayData = SystemDisplayData.Manual(x.toInt, y.toInt) // TODO: position controller
          Some(MapRequest.UpdateSystem(systemId, displayData = Some(newDisplayData)))
        case (MapAction.Select(systemIdOpt), _) =>
          selectedSystem.set(systemIdOpt)
          None
        case (MapAction.TogglePinned(systemId), allSystems) =>
          allSystems.get(systemId).map(sys => MapRequest.UpdateSystem(systemId, isPinned = Some(!sys.system.isPinned)))
      }
    )

    val toolbarView = ToolbarView(selectedSystemSnapshot, actions, mapRole, rds)

    val systemInfoView =
      SolarSystemInfoView(
        static,
        selectedSystemSnapshot.map(_.map(mss => SystemInfo(mss.system.systemId, mss.system.name)))
      )
    val systemSignatureView = SystemSignatureView(static, selectedSystemSnapshot)

    div(
      idAttr := "map-view-inner",
      div(
        idAttr := "map-parent",
        cls    := "grid",
        cls    := "g-20px",
        inContext(self => onClick --> (ev => if (ev.currentTarget == self.ref) selectedSystem.set(None))),
        toolbarView.view,
        div(
          idAttr := "map-inner",
          children <-- allSystems
            .splitByKey((k, v) => SystemView(actions, static, selectedSystem.signal, mapRole)(k, v).view)
            .map(_.toSeq)
        )
      ),
      div(
        idAttr := "map-left-sidebar",
        systemInfoView.view,
        systemSignatureView.view
      )
    )

object MapView:
  import org.updraft0.controltower.protocol.jsoncodec.given
  import sttp.client3.UriContext
  import zio.json.*
  private var counter = 0

  def apply(map: Page.Map)(using ct: ControlTowerBackend): Future[MapView] =
    for
      rds <- ReferenceDataStore.usingBackend()
      _ = (counter += 1)
    yield new MapView(counter, ct, rds, ws(map), map.name, map.characterId)

  private def ws(map: Page.Map)(using ct: ControlTowerBackend) =
    WebSocket
      .url(
        uri"${ct.wsUrlOpt.get}/api/map/${map.name}/${map.characterId.toString}/ws".toString,
        "ws"
      ) // FIXME - use the other constructor
      .receiveText(_.fromJson[MapMessage].left.map(new RuntimeException(_)))
      .sendText[MapRequest](_.toJson)
      .build()
