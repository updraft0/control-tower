package controltower.page.map.view

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.{ReactiveElement, ReactiveHtmlElement}
import controltower.Page
import controltower.backend.ControlTowerBackend
import controltower.component.Modal
import controltower.db.ReferenceDataStore
import controltower.page.map.*
import controltower.ui.*
import io.laminext.websocket.*
import org.updraft0.controltower.constant
import org.updraft0.controltower.protocol.*
import org.scalajs.dom.KeyboardEvent
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.Try
import scala.language.implicitConversions

// TODO: duplication
given equalEventTarget[El <: org.scalajs.dom.Element]: CanEqual[org.scalajs.dom.EventTarget, El] = CanEqual.derived

private final class MapView(
    viewId: Int,
    page: Page.Map,
    ct: ControlTowerBackend,
    rds: ReferenceDataStore,
    ws: WebSocket[MapMessage, MapRequest],
    time: Signal[Instant]
) extends ViewController:

  private val mapTop                    = Var[Option[Element]](None)
  private val inKeyHandler              = Var(false)
  private val notInKeyHandler           = inKeyHandler.signal.map(!_)
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
      ws.isConnected,
      rds,
      controller.pos
    )

    // TODO: move this into the controller?
    val connectingSystem = HVar(MapNewConnectionState.Stopped)

    val navTopView =
      NavTopView(
        page.name,
        controller.mapMetaSignal,
        controller.allLocations.signal,
        time,
        ws.isConnected,
        controller.serverStatus.signal,
        ct
      )
    val systemInfoView =
      SolarSystemInfoView(
        static,
        controller.selectedSystem.map(_.map(mss => SystemInfo(mss.system.systemId, mss.system.name)))
      )
    val systemSignatureView =
      SystemSignatureView(
        controller.selectedSystem,
        mapCtx.actions,
        controller.mapSettings,
        mapCtx.mapRole,
        time,
        ws.isConnected
      )

    val connectionView = ConnectionInfoView(controller.selectedConnection)

    val systemNodesTransformer = CollectionCommandTransformer[MapSystemSnapshot, SystemView, Element, Long](
      _.system.systemId,
      mssV => {
        val systemId = mssV.now().system.systemId
        SystemView(
          systemId,
          mssV.signal,
          controller.pos,
          controller.selectedSystemId.signal,
          controller.bulkSelectedSystemIds.signal,
          controller.allLocations.signal.map(_.getOrElse(constant.SystemId(systemId), Array.empty[CharacterLocation])),
          connectingSystem.current,
          ws.isConnected,
          controller.mapSettings
        )
      },
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

    val selectionView =
      SelectionView(controller.selectedSystemId.signal, controller.bulkSelectedSystemIds.writer, systemNodes)

    div(
      idAttr := "map-view-inner",
      // A -> add system
      modalKeyBinding(
        "KeyA",
        controller.mapRole.map(RoleController.canAddSystem).combineWith(ws.isConnected).map(_ && _),
        _.map(ev => (ev, ())),
        (_, onClose) =>
          Modal.show(
            (closeMe, owner) => systemAddView(controller.actionsBus, closeMe, rds, controller.pos)(using owner),
            onClose,
            true,
            cls := "system-add-dialog"
          )
      ),
      // delete -> remove system
      modalKeyBinding(
        "Delete",
        controller.mapRole.map(RoleController.canRemoveSystem).combineWith(ws.isConnected).map(_ && _),
        _.filterWith(controller.bulkSelectedSystemIds, _.isEmpty)
          .filterWith(controller.selectedSystem, _.isDefined)
          .withCurrentValueOf(controller.selectedSystem)
          .map((ev, opt) => (ev, opt.get)),
        (system, onClose) => removeSystemConfirm(system, controller.actionsBus, onClose)(using rds)
      ),
      // delete -> remove multiple systems
      modalKeyBinding(
        "Delete",
        controller.mapRole.map(RoleController.canRemoveSystem).combineWith(ws.isConnected).map(_ && _),
        _.filterWith(controller.bulkSelectedSystemIds, _.nonEmpty).withCurrentValueOf(controller.bulkSelectedSystemIds),
        (systemIds, onClose) => removeMultipleSystems(systemIds, controller.actionsBus, onClose)(using rds)
      ),
      // P -> paste system signatures
      modalKeyBinding(
        "KeyP",
        controller.mapRole.map(RoleController.canEditSignatures).combineWith(ws.isConnected).map(_ && _),
        _.filterWith(controller.selectedSystem, _.isDefined)
          .withCurrentValueOf(controller.selectedSystem)
          .map((ev, opt) => (ev, opt.get)),
        { (system, onClose) =>
          val solarSystem = static.solarSystemMap(system.system.systemId)
          Modal.show(
            pasteSignaturesView(
              system,
              solarSystem,
              solarSystem.systemClass
                .flatMap(whc => static.signatureByClassAndGroup.get(whc))
                .getOrElse(Map.empty),
              time,
              controller.actionsBus
            ),
            onClose,
            false,
            cls := "system-paste-signatures"
          )
        }
      ),
      // R -> rename system
      modalKeyBinding(
        "KeyR",
        controller.mapRole.map(RoleController.canRenameSystem).combineWith(ws.isConnected).map(_ && _),
        _.filterWith(controller.selectedSystem, _.isDefined)
          .withCurrentValueOf(controller.selectedSystem)
          .map((ev, opt) => (ev, opt.get)),
        (system, onClose) =>
          Modal.show(
            (closeMe, owner) =>
              systemRenameView(
                system.system.systemId,
                system.system.name.getOrElse(""),
                controller.actionsBus,
                closeMe
              ),
            onClose,
            true,
            cls := "system-rename-dialog"
          )
      ),
      div(
        idAttr := "map-parent",
        cls    := "grid",
        cls    := "g-20px",
        toolbarView.view,
        // note: the selection event handler must come before the cancel one
        selectionView.eventHandlers,
        inContext(self =>
          onPointerUp.compose(_.withCurrentValueOf(mapCtx.userPreferences)) --> ((ev, prefs) =>
            // TODO: unsure about the checks against self.ref - should always be true?

            // note: this click handler cleans up any selection
            if (prefs.clickResetsSelection && ev.currentTarget == self.ref)
              Var.set(controller.selectedSystemId -> None, controller.selectedConnectionId -> None)

            if (ev.currentTarget == self.ref)
              // always clean up the multiple system selection
              controller.bulkSelectedSystemIds.set(Array.empty)
          )
        ),
        div(
          idAttr := "map-inner",
          children.command <-- systemNodes,
          svg.svg(
            svg.cls      := "connection-container",
            svg.style    := "position: absolute; left: 0px; top: 0px;",
            svg.height   := "100%",
            svg.width    := "100%",
            svg.overflow := "visible",
            svg.defs(
              svg.filter(
                // TODO could improve with drop shadows etc.
                svg.idAttr := "background-size",
                svg.width  := "1.2",
                svg.height := "1.2",
                svg.x      := "-0.1",
                svg.y      := "-0.1",
                svg.feFlood(svg.floodColor := "#3c3f41" /* FIXME hardcoding? */ ),
                svg.feComposite(
                  svg.in       := "SourceGraphic",
                  svg.operator := "over"
                )
              )
            ),
            children.command <-- connectionNodes,
            connectionInProgress.view
          ),
          selectionView.view
        )
      ),
      div(
        idAttr := "map-left-sidebar",
        navTopView.view,
        systemInfoView.view,
        systemSignatureView.view,
        connectionView.view
      )
    )

  private def modalKeyBinding[B](
      code: String,
      shouldEnable: Signal[Boolean],
      compose: EventStream[KeyboardEvent] => EventStream[(KeyboardEvent, B)],
      action: (B, Observer[Unit]) => Unit
  ) =
    documentEvents(
      _.onKeyDown
        .filter(ev => !ev.repeat && ev.code == code && !ev.ctrlKey && !ev.shiftKey && !ev.metaKey)
    ).compose(es =>
      compose(es).filterWith(Modal.Shown.signal.map(!_)).filterWith(shouldEnable).filterWith(notInKeyHandler.signal)
    ) --> { (ev, b) =>
      ev.preventDefault()
      inKeyHandler.set(true)
      action(b, Observer(_ => inKeyHandler.set(false)))
    }

object MapView:
  import org.updraft0.controltower.protocol.jsoncodec.given
  import sttp.client3.UriContext
  private var counter = 0

  def apply(map: Page.Map, time: Signal[Instant])(using ct: ControlTowerBackend): Future[MapView] =
    for
      rds <- ReferenceDataStore.usingBackend()
      _ = (counter += 1)
    yield new MapView(counter, map, ct, rds, ws(map), time)

  private def ws(map: Page.Map)(using ct: ControlTowerBackend) =
    val path = s"/api/map/ws/${map.name}/${map.character}" // TODO this should use the sttp definition

    ct.wsUrlOpt
      .map(base => WebSocket.url(uri"$base$path".toString, base.scheme.getOrElse("ws")))
      .getOrElse(WebSocket.path(path))
      .receiveText(s => Try(readFromString[MapMessage](s)).toEither)
      .sendText(writeToString[MapRequest](_))
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
