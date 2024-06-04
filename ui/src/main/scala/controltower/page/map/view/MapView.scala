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
import scala.concurrent.duration.*
import scala.language.implicitConversions

// TODO: duplication
given equalEventTarget[El <: org.scalajs.dom.Element]: CanEqual[org.scalajs.dom.EventTarget, El] = CanEqual.derived

private class MapView(
    viewId: Int,
    page: Page.Map,
    @unused ct: ControlTowerBackend,
    rds: ReferenceDataStore,
    ws: WebSocket[MapMessage, MapRequest],
    time: Signal[Instant]
) extends ViewController:

  private val mapTop                    = Var[Option[Element]](None)
  private var controller: MapController = _

  override def view: Element = div(
    idAttr := s"map-controller-view-${viewId}",
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
      case WebSocketEvent.Error(ex) =>
        if (io.laminext.websocket.WebSocketError.eq(ex)) () // no-op
        else org.scalajs.dom.console.error(s"Unhandled error in MapControllerView: $ex")
      case _ => // no-op
  private def binderStarted(owner: Owner): Unit =
    controller = new MapController(rds, time)(using owner)

    // subscribe output and input
    controller.requestBus.events.addObserver(ws.send)(using owner)
    ws.events.foreach(handleIncoming)(using owner)
    // get the static reference data
    rds.referenceAll().foreach { ref =>
      // render the top element (FUGLY FIXME)
      mapTop.set(Some(renderTop(using owner)))
    }

    // get the initial map snapshot
    ws.connected.foreach(_ => controller.requestBus.emit(MapRequest.GetSnapshot))(using owner)

  private def binderStopped: Unit =
    org.scalajs.dom.console.debug("stopped map view controller")
    controller.clear()
    mapTop.set(None) // FIXME - does this trigger the exception?
    // do not clear the caches of static data?

  private def renderTop(using Owner) =
    given mapCtx: MapViewContext = controller.context

    val static = mapCtx.staticData
    val toolbarView = ToolbarView(
      controller.selectedSystem,
      controller.selectedConnection,
      mapCtx.actions,
      mapCtx.mapRole,
      rds,
      controller.pos
    )

    // TODO: move this into the controller?
    val connectingSystem = HVar(MapNewConnectionState.Stopped)

    val navTopView = NavTopView(page.name, mapCtx.characterId, time, mapCtx.mapRole, ws.isConnected)
    val systemInfoView =
      SolarSystemInfoView(
        static,
        controller.selectedSystem.map(_.map(mss => SystemInfo(mss.system.systemId, mss.system.name)))
      )
    val systemSignatureView =
      SystemSignatureView(
        static,
        controller.selectedSystem,
        mapCtx.actions,
        controller.mapSettings,
        mapCtx.mapRole,
        time
      )

    val systemNodesTransformer = CollectionCommandTransformer[MapSystemSnapshot, SystemView, Element, Long](
      _.system.systemId,
      mssV =>
        SystemView(
          mssV.now().system.systemId,
          mssV.signal,
          controller.pos,
          controller.selectedSystemId.signal,
          connectingSystem.current,
          controller.mapSettings
        ),
      (view, _) => view.view
    )

    val connectionNodeTransformer =
      CollectionCommandTransformer[MapWormholeConnectionWithSigs, ConnectionView, Element, Long](
        _.connection.id,
        whcV =>
          ConnectionView(
            whcV.now().connection.id,
            whcV.now().connection.fromSystemId,
            whcV.now().connection.toSystemId,
            whcV.signal,
            controller.selectedConnectionId,
            controller.pos,
            controller.BoxSize
          ),
        (view, _) => view.view
      )

    val systemNodes     = systemNodesTransformer.run(controller.allSystemChangesStream)
    val connectionNodes = connectionNodeTransformer.run(controller.allConnectionChangesStream)
    val connectionInProgress =
      ConnectionInProgressView(connectingSystem, controller.pos, controller.BoxSize, controller.actionsBus)

    div(
      idAttr := "map-view-inner",
      div(
        idAttr := "map-parent",
        cls    := "grid",
        cls    := "g-20px",
        inContext(self =>
          onClick.stopPropagation --> (ev =>
            // note: this click handler cleans up any selection
            if (ev.currentTarget == self.ref)
              Var.set(controller.selectedSystemId -> None, controller.selectedConnectionId -> None)
          )
        ),
        toolbarView.view,
        div(
          idAttr := "map-inner",
          children.command <-- systemNodes,
          svg.svg(
            svg.cls    := "connection-container",
            svg.style  := "position: absolute; left: 0px; top: 0px;",
            svg.height := "100%",
            svg.width  := "100%",
            children.command <-- connectionNodes,
            connectionInProgress.view
          )
        )
      ),
      div(
        idAttr := "map-left-sidebar",
        navTopView.view,
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
    yield new MapView(counter, map, ct, rds, ws(map), time)

  private def ws(map: Page.Map)(using ct: ControlTowerBackend) =
    val path = s"/api/map/${map.name}/${map.character}/ws" // TODO this should use the sttp definition

    ct.wsUrlOpt
      .map(base => WebSocket.url(uri"$base$path".toString, base.scheme.getOrElse("ws")))
      .getOrElse(WebSocket.path(path))
      .receiveText(_.fromJson[MapMessage].left.map(new RuntimeException(_)))
      .sendText[MapRequest](_.toJson)
      .build(
        autoReconnect = true,
        bufferWhenDisconnected = false,
        reconnectDelay = 2.seconds,
        reconnectDelayOffline = 20.seconds,
        reconnectRetries = Int.MaxValue,
        managed = true
      )

enum MapNewConnectionState derives CanEqual:
  case Start(fromSystemId: Long, initial: Coord) extends MapNewConnectionState
  case Move(fromSystemId: Long, at: Coord)       extends MapNewConnectionState
  case Stopped                                   extends MapNewConnectionState
