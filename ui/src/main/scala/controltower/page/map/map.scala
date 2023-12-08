package controltower.page.map

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.{ReactiveElement, ReactiveHtmlElement}
import controltower.Page
import controltower.backend.ControlTowerBackend
import controltower.db.ReferenceDataStore
import controltower.ui.*
import io.laminext.websocket.*
import org.updraft0.controltower.protocol.*

import scala.collection.immutable.SeqMap
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success

private class MapViewController(
    id: Int,
    ct: ControlTowerBackend,
    rds: ReferenceDataStore,
    ws: WebSocket[MapMessage, MapRequest],
    mapName: String,
    characterId: Long
) extends ViewController:

  private val cacheSolarSystem = mutable.Map.empty[Long, SolarSystem]
  private var cacheReference   = Option.empty[Reference]

  private val allSystems     = FakeMapVar[Long, MapSystemSnapshot]()
  private val requestBus     = EventBus[MapRequest]()
  private val mapTop         = Var[Option[Element]](None)
  private val selectedSystem = Var[Option[Long]](None)

  def view: Element = div(bind*)

  def bind[El <: HtmlElement]: List[Modifier[El]] =
    List(
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
  private inline def handleIncomingMessage(msg: MapMessage): Unit =
    msg match
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
      case _ => org.scalajs.dom.console.warn(s"UNHANDLED message $msg") // FIXME
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
    val static = SystemStaticData(
      cacheSolarSystem.view,
      cacheReference.map(_.wormholeTypes.map(wt => wt.typeId -> wt).toMap).getOrElse(Map.empty)
    )
    val selectedSystemSnapshot =
      selectedSystem.signal.distinct.combineWithFn(allSystems.signal) {
        case (Some(systemId), allMap) => allMap.get(systemId)
        case _                        => None
      }
    val toolbarView = ToolbarView(selectedSystem.signal, requestBus.writer, rds)
    val systemView  = SystemView(requestBus.writer, static, selectedSystem)

    val systemInfoView =
      SystemInfoView(static, selectedSystemSnapshot.map(_.map(mss => SystemInfo(mss.system.systemId, mss.system.name))))
    val systemSignatureView = SystemSignatureView(static, selectedSystemSnapshot)

    div(
      idAttr := "map-view-inner",
      toolbarView.view,
      div(
        idAttr := "mapParent",
        cls    := "grid",
        cls    := "g-20px",
        inContext(self => onClick --> (ev => if (ev.currentTarget == self.ref) selectedSystem.set(None))),
        div(
          idAttr := "mapInner",
          children <-- allSystems.splitByKey(systemView.view).map(_.toSeq)
        )
      ),
      div(
        idAttr := "mapLeftSidebar",
        systemInfoView.view,
        systemSignatureView.view
      )
    )

object MapViewController:
  import org.updraft0.controltower.protocol.jsoncodec.given
  import sttp.client3.UriContext
  import zio.json.*
  private var counter = 0

  def apply(map: Page.Map)(using ct: ControlTowerBackend): Future[MapViewController] =
    for
      rds <- ReferenceDataStore.usingBackend()
      _ = (counter += 1)
    yield new MapViewController(counter, ct, rds, ws(map), map.name, map.characterId)

  private def ws(map: Page.Map)(using ct: ControlTowerBackend) =
    WebSocket
      .url(
        uri"${ct.wsUrlOpt.get}/api/map/${map.name}/${map.characterId.toString}/ws".toString,
        "ws"
      ) // FIXME - use the other constructor
      .receiveText(_.fromJson[MapMessage].left.map(new RuntimeException(_)))
      .sendText[MapRequest](_.toJson)
      .build()
