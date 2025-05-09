package controltower.page.map.view

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveElement
import controltower.Page
import controltower.backend.ControlTowerBackend
import controltower.component.Modal
import controltower.db.ReferenceDataStore
import controltower.page.map.*
import controltower.ui.*
import io.laminext.websocket.*
import org.scalajs.dom
import org.scalajs.dom.KeyboardEvent
import org.updraft0.controltower.constant.SystemId
import org.updraft0.controltower.protocol.*

import java.time.Instant
import scala.compiletime.uninitialized
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.language.implicitConversions
import scala.util.Try

// TODO: duplication
given equalEventTarget: [El <: org.scalajs.dom.Element] => CanEqual[org.scalajs.dom.EventTarget, El] =
  CanEqual.derived

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
  private var controller: MapController = uninitialized

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
    controller = new MapController(rds, time, ws.isConnected)(using owner)

    // subscribe output and input
    controller.requestBus.events.addObserver(ws.send)(using owner)
    ws.events.foreach(handleIncoming)(using owner)
    // get the static reference data
    rds.referenceAll().foreach { _ =>
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
        controller.mapSettings
      )

    val connectionView = ConnectionInfoView(controller.selectedConnection)

    val intelNoteView = IntelNoteView(
      controller.selectedSystemId.signal,
      controller.selectedSystem.map(_.map(_.notes).getOrElse(Array.empty))
    )

    val intelStructureView = IntelStructureView(
      controller.selectedSystemId.signal,
      controller.selectedSystem.map(_.map(_.structures).getOrElse(Array.empty))
    )(using mapCtx, ct)

    val intelPingView = IntelPingView(
      controller.selectedSystemId.signal,
      controller.selectedSystem.map(_.map(_.pings).getOrElse(Array.empty))
    )

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
          controller.allLocations.signal.map(_.getOrElse(SystemId(systemId), Array.empty[CharacterLocation])),
          connectingSystem.current,
          ws.isConnected,
          controller.mapSettings,
          controller.mapUiEvents.writer
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
            controller.BoxSize,
            controller.mapUiEvents.writer
          ),
        (view, _) => view.view
      )

    val systemNodes     = systemNodesTransformer.run(controller.allSystemChangesStream)
    val connectionNodes = connectionNodeTransformer.run(controller.allConnectionChangesStream)
    val connectionInProgress =
      ConnectionInProgressView(connectingSystem, controller.pos, controller.BoxSize, controller.actionsBus)

    val selectionView =
      SelectionView(controller.selectedSystemId.signal, controller.bulkSelectedSystemIds.writer, systemNodes)
    val contextMenuView = ContextMenuView(controller)

    div(
      idAttr := "map-view-inner",
      // context menu
      onContextMenu.preventDefault --> Observer.empty, // TODO - right now this is needed in multiple places
      contextMenuView.view,
      // A -> add system
      modalKeyBinding(
        "a",
        controller.roleController.canAddSystem,
        _.map(ev => (ev, ())),
        (_, onClose) =>
          Modal.show(
            (closeMe, _) => systemAddView(controller.actionsBus, closeMe, rds, controller.pos),
            onClose,
            true,
            cls := "system-add-dialog"
          )
      ),
      // C -> cleanup stale signatures in system
      modalKeyBinding[(MapSystemSnapshot, Instant)](
        "c",
        controller.roleController.canEditSignatures,
        _.filterWith(controller.selectedSystem, _.isDefined)
          .withCurrentValueOf(controller.selectedSystem, time)
          .map((ev, opt, now) => (ev, (opt.get, now))),
        { case ((system, now), onClose) =>
          val staleAt = now.minus(StaleSignatureInterval)
          val stale   = system.signatures.filter(_.updatedAt.isBefore(staleAt))

          if (stale.nonEmpty)
            controller.actionsBus.onNext(MapAction.RemoveSignatures(system.system.systemId, stale.map(_.id).toSet))

          onClose.onNext(())
        }
      ),
      // delete -> remove system
      modalKeyBinding(
        "Delete",
        controller.roleController.canRemoveSystem,
        _.filterWith(controller.bulkSelectedSystemIds, _.isEmpty)
          .filterWith(controller.selectedSystem, _.isDefined)
          .withCurrentValueOf(controller.selectedSystem)
          .map((ev, opt) => (ev, opt.get)),
        (system, onClose) => removeSystemConfirm(system, controller.actionsBus, onClose)(using rds)
      ),
      // delete -> remove multiple systems
      modalKeyBinding(
        "Delete",
        controller.roleController.canRemoveSystem,
        _.filterWith(controller.bulkSelectedSystemIds, _.nonEmpty).withCurrentValueOf(controller.bulkSelectedSystemIds),
        (systemIds, onClose) => removeMultipleSystems(systemIds, controller.actionsBus, onClose)
      ),
      // P -> paste system signatures
      modalKeyBinding(
        "p",
        controller.roleController.canEditSignatures,
        _.filterWith(controller.selectedSystem, _.isDefined)
          .withCurrentValueOf(controller.selectedSystem)
          .map((ev, opt) => (ev, opt.get)),
        { (system, onClose) =>
          val solarSystem = static.solarSystemMap(system.system.systemId)
          Modal.show(
            pasteSignaturesView(
              system,
              solarSystem,
              time,
              controller.actionsBus
            ),
            onClose,
            false,
            cls := "system-paste-signatures"
          )
        }
      ),
      // paste system signatures without confirmation // TODO why is event handler only fired on document level
      clipboardEventBinding[(SystemId, Instant)](
        documentEvents(_.onPaste),
        controller.roleController.canEditSignatures,
        _.filterWith(controller.selectedSystemId, _.isDefined)
          .withCurrentValueOf(controller.selectedSystemId, time)
          .map((ev, opt, now) => (ev, (opt.get, now))),
        { case (clipboard, (systemId, now), onClose) =>
          parseLines(clipboard)
            .flatMap(_.map(parseLineToSignature(_, now)).sequence)
            .foreach: sigs =>
              controller.actionsBus.onNext(MapAction.UpdateSignatures(systemId, false, sigs.toArray))
          onClose.onNext(())
        }
      ),
      // R -> rename system
      modalKeyBinding(
        "r",
        controller.roleController.canRenameSystem,
        _.filterWith(controller.selectedSystem, _.isDefined)
          .withCurrentValueOf(controller.selectedSystem)
          .map((ev, opt) => (ev, opt.get)),
        (system, onClose) =>
          Modal.show(
            (closeMe, _) =>
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
      // similarly, ui events to bring up dialogs
      controller.mapUiEvents.events --> Observer[MapUiEvent] {
        case MapUiEvent.AddSystemDialog =>
          Modal.show(
            (closeMe, _) => systemAddView(controller.actionsBus, closeMe, rds, controller.pos),
            Observer.empty,
            true,
            cls := "system-add-dialog"
          )
        case MapUiEvent.RemoveConnectionDialog(conn) =>
          removeConnection(conn, controller.actionsBus, Observer.empty)(using rds)
        case MapUiEvent.RemoveSystemSelectionDialog(SystemSelectionState.Single(system)) =>
          removeSystemConfirm(system, controller.actionsBus, Observer.empty)(using rds)
        case MapUiEvent.RemoveSystemSelectionDialog(SystemSelectionState.Multiple(sIds)) =>
          removeMultipleSystems(sIds, controller.actionsBus, Observer.empty)
        case MapUiEvent.RemoveSystemSelectionDialog(_) => () // no-op
        case _: MapUiEvent.ContextMenu                 => () // no-op
      },
      // main map div
      div(
        idAttr := "map-parent",
        cls    := "grid",
        cls    := "g-20px",
        toolbarView.view,
        // note: the selection event handler must come before the cancel one
        contextMenuView.eventHandlers,
        selectionView.eventHandlers,
        inContext(self =>
          onPointerUp.compose(_.withCurrentValueOf(mapCtx.userPreferences)) --> ((ev, prefs) =>
            // TODO: unsure about the checks against self.ref - should always be true?

            // note: this click handler cleans up any selection
            if (prefs.map.clickResetsSelection && ev.currentTarget == self.ref)
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
      ),
      div(
        // TODO - position this better
        idAttr := "map-bottom-bar",
        intelNoteView.view,
        intelStructureView.view,
        intelPingView.view
      )
    )

  private def modalKeyBinding[B](
      key: String,
      shouldEnable: Signal[Boolean],
      compose: EventStream[KeyboardEvent] => EventStream[(KeyboardEvent, B)],
      action: (B, Observer[Unit]) => Unit,
      ctrlOn: Boolean = false,
      shiftOn: Boolean = false,
      metaOn: Boolean = false
  ) =
    documentEvents(
      _.onKeyDown
        .filter(ev =>
          !ev.repeat && ev.key == key &&
            ((ctrlOn && ev.ctrlKey) || (!ctrlOn && !ev.ctrlKey)) &&
            ((shiftOn && ev.shiftKey) || (!shiftOn && !ev.shiftKey)) &&
            ((metaOn && ev.metaKey) || (!metaOn && !ev.metaKey))
        )
    ).compose(es =>
      compose(es).filterWith(Modal.Shown.signal.map(!_)).filterWith(shouldEnable).filterWith(notInKeyHandler.signal)
    ) --> { (ev, b) =>
      ev.preventDefault()
      inKeyHandler.set(true)
      action(b, Observer(_ => inKeyHandler.set(false)))
    }

  private def clipboardEventBinding[B](
      event: EventStream[dom.ClipboardEvent],
      shouldEnable: Signal[Boolean],
      compose: EventStream[dom.ClipboardEvent] => EventStream[(dom.ClipboardEvent, B)],
      action: (String, B, Observer[Unit]) => Unit
  ) =
    event.compose(es =>
      compose(es).filterWith(Modal.Shown.signal.map(!_)).filterWith(shouldEnable).filterWith(notInKeyHandler.signal)
    ) --> { (ev, b) =>
      ev.preventDefault()
      inKeyHandler.set(true)
      val s = ev.clipboardData.getData("text")
      action(s, b, Observer(_ => inKeyHandler.set(false)))
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
  case Start(fromSystemId: SystemId, initial: Coord) extends MapNewConnectionState
  case Move(fromSystemId: SystemId, at: Coord)       extends MapNewConnectionState
  case Stopped                                       extends MapNewConnectionState
