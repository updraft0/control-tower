package controltower.page.map.view

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.{ReactiveElement, ReactiveHtmlElement}
import controltower.Page
import controltower.backend.ControlTowerBackend
import controltower.db.ReferenceDataStore
import controltower.page.map.*
import controltower.ui.*
import io.laminext.websocket.*
import org.updraft0.controltower.protocol.*

import java.time.Instant
import scala.annotation.unused
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions

private class MapView(
    id: Int,
    @unused ct: ControlTowerBackend,
    rds: ReferenceDataStore,
    ws: WebSocket[MapMessage, MapRequest],
    @unused characterId: Long,
    time: Signal[Instant]
) extends ViewController:

  private val mapTop                    = Var[Option[Element]](None)
  private var controller: MapController = _

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
      case WebSocketEvent.Received(msg) => controller.responseBus.writer.onNext(msg)
      case WebSocketEvent.Error(ex)     => org.scalajs.dom.console.error(s"Unhandled error in MapControllerView: ${ex}")
      case _                            => // no-op
  private def binderStarted(owner: Owner): Unit =
    controller = new MapController(rds, PositionController, time)(using owner)

    // subscribe output and input
    controller.requestBus.events.addObserver(ws.send)(using owner)
    ws.events.foreach(handleIncoming)(using owner)
    // get the static reference data
    rds.referenceAll().foreach { ref =>
      // render the top element (FUGLY FIXME)
      mapTop.set(Some(renderTop(using owner)))
    }

    // get the initial map snapshot
    controller.requestBus.emit(MapRequest.GetSnapshot)

  private def binderStopped: Unit =
    org.scalajs.dom.console.debug("stopped map view controller")
    controller.clear()
    mapTop.set(None)
    // do not clear the caches of static data?

  private def renderTop(using Owner) =
    given mapCtx: MapViewContext = controller.context

    val static      = mapCtx.staticData
    val toolbarView = ToolbarView(controller.selectedSystem, mapCtx.actions, mapCtx.mapRole, rds, controller.pos)

    val systemInfoView =
      SolarSystemInfoView(
        static,
        controller.selectedSystem.map(_.map(mss => SystemInfo(mss.system.systemId, mss.system.name)))
      )
    val systemSignatureView =
      SystemSignatureView(static, controller.selectedSystem, mapCtx.actions, controller.mapSettings, time)

    val systemNodesTransformer = CollectionCommandTransformer[MapSystemSnapshot, SystemView, Element, Long](
      _.system.systemId,
      mssV =>
        SystemView(
          mssV.now().system.systemId,
          mssV.signal,
          controller.pos,
          controller.selectedSystemId.signal,
          controller.mapSettings
        ),
      (view, _) => view.view
    )
    val systemNodes = systemNodesTransformer.run(controller.allSystemsChangesStream)

    div(
      idAttr := "map-view-inner",
      div(
        idAttr := "map-parent",
        cls    := "grid",
        cls    := "g-20px",
        inContext(self => onClick --> (ev => if (ev.currentTarget == self.ref) controller.selectedSystemId.set(None))),
        toolbarView.view,
        div(
          idAttr := "map-inner",
          children.command <-- systemNodes

//          children <-- allSystems
//            .splitByKey((k, v) =>
//              SystemView(actions, static, positionController, selectedSystem.signal, mapRole)(k, v).view
//            )
//            .map(_.toSeq)
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

  def apply(map: Page.Map, time: Signal[Instant])(using ct: ControlTowerBackend): Future[MapView] =
    for
      rds <- ReferenceDataStore.usingBackend()
      _ = (counter += 1)
    yield new MapView(counter, ct, rds, ws(map), map.characterId, time)

  private def ws(map: Page.Map)(using ct: ControlTowerBackend) =
    WebSocket
      .url(
        uri"${ct.wsUrlOpt.get}/api/map/${map.name}/${map.characterId.toString}/ws".toString,
        "ws"
      ) // FIXME - use the other constructor
      .receiveText(_.fromJson[MapMessage].left.map(new RuntimeException(_)))
      .sendText[MapRequest](_.toJson)
      .build()
