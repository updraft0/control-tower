package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.ui.*
import controltower.component.{ContextMenu, ContextMenuElement}
import controltower.page.map.MapAction
import org.updraft0.controltower.constant.ConnectionId
import org.updraft0.controltower.protocol.*

enum ContextMenuState derives CanEqual:
  case Closed
  case Open(r: ContextMenuRequest) extends ContextMenuState

/** The map's right-click context menu.
  *
  * The menu visually should appear only on the map canvas but needs to be able to be overlaid on top of it, so it's
  * position is relative to the outer container while the events received only happen on the (inner) canvas.
  */
final class ContextMenuView(
    controller: MapController,
    isConnected: Signal[Boolean]
):

  private val state    = Var[ContextMenuState](ContextMenuState.Closed)
  private val isClosed = state.signal.map(_ == ContextMenuState.Closed)
  private val isOpen   = isClosed.map(!_)

  private var mapParent: org.scalajs.dom.Element = null

  // connections can only be updated if one side is linked to a signature (where the data about the connection is stored)
  private inline def canUpdateConnectionAttributes(connectionId: ConnectionId): Signal[Boolean] =
    controller.roleController.canChangeConnections
      .withCurrentValueOf(controller.allConnections.signal.map(_.get(connectionId)))
      .map((roleAllows, whcOpt) =>
        roleAllows && whcOpt.exists(wss => wss.fromSignature.nonEmpty || wss.toSignature.nonEmpty)
      )

  private inline def menuItem(
      action: String,
      icon: String,
      label: String,
      canPerformAction: Signal[Boolean],
      onItemClick: EventProcessor[org.scalajs.dom.MouseEvent, Unit] => Modifier[Element],
      isDestructive: Boolean = false,
      iconMod: Modifier[HtmlElement] = emptyMod
  ) =
    ContextMenuElement.MenuItem(
      action,
      button(
        tpe := "button",
        i(cls := "ti", cls := icon, iconMod),
        label,
        onItemClick(onClick.mapToUnit),
        onClick.mapTo(ContextMenuState.Closed) --> state
      ),
      disabled = canPerformAction.invert
    )

  def close: Observer[Unit] = Observer(_ => state.set(ContextMenuState.Closed))

  def eventHandlers: Modifier[HtmlElement] =
    modSeq(
      onMountCallback(cx => mapParent = cx.thisNode.ref.asInstanceOf[org.scalajs.dom.Element]),
      onContextMenu.preventDefault --> Observer.empty, // TODO - right now this is needed in multiple places
      onPointerDown
        .compose(
          _.filterWith(isOpen)
            .tapEach(_.stopImmediatePropagation())
            .mapTo(ContextMenuState.Closed)
        ) --> state,
      inContext(self =>
        onPointerUp
          .filter(_.filterWith(PointerFilter.PrimaryPointer | PointerFilter.MouseButtonRight))
          .preventDefault
          .stopImmediatePropagation
          .compose(
            _.filterWith(isClosed)
              .map(_.posRelativeTo(self))
              .map(c => MapUiEvent.ContextMenu(ContextMenuRequest.Generic(c)))
          )
          --> controller.mapUiEvents
      )
    )

  def view: Modifier[HtmlElement] =
    div(
      idAttr := "map-context-menu",
      controller.mapUiEvents.stream
        .collectOpt {
          case MapUiEvent.ContextMenu(req) => Some(req)
          case _                           => None
        }
        .map(ContextMenuState.Open(_)) --> state,
      display <-- state.signal.map:
        case ContextMenuState.Closed => "none"
        case _                       => ""
      ,
      inContext(self =>
        styleAttr <-- state.signal
          .map:
            case ContextMenuState.Open(req) =>
              // TODO: make this open on the left when too close to border
              (mapParent, req) match {
                case (null, _) => // if we don't have a parent element, can't use its scroll position
                  s"left: ${req.mousePosition.x}px; top ${req.mousePosition.y}px;"
                case (_, _: ContextMenuRequest.Generic) =>
                  // TODO: no idea why this case doesn't need the scroll adjustment...
                  val (left, top) = (mapParent.getBoundingClientRect().left, mapParent.getBoundingClientRect().top)
                  s"left: ${req.mousePosition.x + left}px; top: ${req.mousePosition.y + top}px;"
                case _ =>
                  val (left, top) = (mapParent.getBoundingClientRect().left, mapParent.getBoundingClientRect().top)
                  s"left: ${req.mousePosition.x + left - mapParent.scrollLeft}px; top: ${req.mousePosition.y + top - mapParent.scrollTop}px;"
              }
            case _ => "display: none;"
      ),
      child <-- state.signal.map:
        case ContextMenuState.Closed => emptyNode
        case ContextMenuState.Open(c: ContextMenuRequest.Connection) =>
          ContextMenu(
            "connection-action",
            menuItem(
              "toggle-eol",
              "ti-hourglass-low",
              "toggle eol",
              canUpdateConnectionAttributes(c.connectionId),
              _.mapTo(MapAction.ConnectionEolToggle(c.connectionId)) --> controller.actionsBus
            ),
            menuItem(
              "disconnect",
              "ti-link-off",
              "disconnect",
              controller.roleController.canChangeConnections,
              _.compose(
                _.sampleCollectSome(controller.connectionSignal(c.connectionId))
                  .map(MapUiEvent.RemoveConnectionDialog(_))
              ) --> controller.mapUiEvents,
              isDestructive = true
            ),
            ContextMenuElement.Divider,
            menuItem(
              "size-smallest",
              "ti-letter-s",
              "size: smallest",
              canUpdateConnectionAttributes(c.connectionId),
              _.mapTo(MapAction.ConnectionMassSizeChange(c.connectionId, WormholeMassSize.S)) --> controller.actionsBus,
              iconMod = dataAttr("size") := "smallest"
            ),
            menuItem(
              "size-medium",
              "ti-letter-m",
              "size: medium",
              canUpdateConnectionAttributes(c.connectionId),
              _.mapTo(MapAction.ConnectionMassSizeChange(c.connectionId, WormholeMassSize.M)) --> controller.actionsBus,
              iconMod = dataAttr("size") := "medium"
            ),
            menuItem(
              "size-large",
              "ti-letter-l",
              "size: large",
              canUpdateConnectionAttributes(c.connectionId),
              _.mapTo(MapAction.ConnectionMassSizeChange(c.connectionId, WormholeMassSize.L)) --> controller.actionsBus,
              iconMod = dataAttr("size") := "large"
            ),
            menuItem(
              "size-x-large",
              "ti-letter-x",
              "size: x-large",
              canUpdateConnectionAttributes(c.connectionId),
              _.mapTo(
                MapAction.ConnectionMassSizeChange(c.connectionId, WormholeMassSize.XL)
              ) --> controller.actionsBus,
              iconMod = dataAttr("size") := "x-large"
            ),
            ContextMenuElement.Divider,
            menuItem(
              "mass-fresh",
              "ti-circle-filled",
              "mass: fresh",
              canUpdateConnectionAttributes(c.connectionId),
              _.mapTo(
                MapAction.ConnectionMassStatusChange(c.connectionId, WormholeMassStatus.Fresh)
              ) --> controller.actionsBus,
              iconMod = dataAttr("mass") := "fresh"
            ),
            menuItem(
              "mass-reduced",
              "ti-circle-filled",
              "mass: reduced",
              canUpdateConnectionAttributes(c.connectionId),
              _.mapTo(
                MapAction.ConnectionMassStatusChange(c.connectionId, WormholeMassStatus.Reduced)
              ) --> controller.actionsBus,
              iconMod = dataAttr("mass") := "reduced"
            ),
            menuItem(
              "mass-critical",
              "ti-circle-filled",
              "mass: critical",
              canUpdateConnectionAttributes(c.connectionId),
              _.mapTo(
                MapAction.ConnectionMassStatusChange(c.connectionId, WormholeMassStatus.Critical)
              ) --> controller.actionsBus,
              iconMod = dataAttr("mass") := "critical"
            )
          ).view
        case ContextMenuState.Open(s: ContextMenuRequest.System) =>
          ContextMenu(
            "system-action",
            menuItem(
              "toggle-pinned",
              "ti-pin",
              "toggle pinned",
              controller.roleController.canPinUnpinSystem,
              _.mapTo(MapAction.TogglePinned(s.systemId)) --> controller.actionsBus
            ),
            menuItem(
              "remove-current",
              "ti-trash",
              "remove current",
              controller.roleController.canRemoveSystem,
              _.mapToUnit.compose(
                _.withCurrentValueOf(controller.allSystems.signal)
                  .map(_(s.systemId))
                  .map(SystemSelectionState.Single(_))
                  .map(MapUiEvent.RemoveSystemSelectionDialog(_))
              ) --> controller.mapUiEvents,
              isDestructive = true
            ),
            ContextMenuElement.Divider,
            menuItem(
              "stance-unknown",
              "ti-circle-filled",
              "stance: unknown",
              controller.roleController.canUpdateIntelStance,
              _.mapTo(MapAction.IntelChange(s.systemId, IntelStance.Unknown)) --> controller.actionsBus,
              iconMod = dataAttr("stance") := "unknown"
            ),
            menuItem(
              "stance-friendly",
              "ti-heart-filled",
              "stance: friendly",
              controller.roleController.canUpdateIntelStance,
              _.mapTo(MapAction.IntelChange(s.systemId, IntelStance.Friendly)) --> controller.actionsBus,
              iconMod = dataAttr("stance") := "friendly"
            ),
            menuItem(
              "stance-hostile",
              "ti-swords",
              "stance: hostile",
              controller.roleController.canUpdateIntelStance,
              _.mapTo(MapAction.IntelChange(s.systemId, IntelStance.Hostile)) --> controller.actionsBus,
              iconMod = dataAttr("stance") := "hostile"
            )
          ).view
        case ContextMenuState.Open(c: ContextMenuRequest.Generic) =>
          ContextMenu(
            "map-action",
            menuItem(
              "add-system",
              "ti-plus",
              "add system",
              controller.roleController.canAddSystem,
              _.mapTo(MapUiEvent.AddSystemDialog) --> controller.mapUiEvents
            ),
            menuItem(
              "select-unpinned",
              "ti-select-all",
              "select unpinned",
              canPerformAction = Val(true),
              _.mapTo(MapAction.SelectUnpinned) --> controller.actionsBus
            ),
            menuItem(
              "remove-selected",
              "ti-analyze-off",
              "remove selected",
              canPerformAction = controller.systemSelectionState
                .map {
                  case SystemSelectionState.None => false
                  case _                         => true
                }
                .combineWith(controller.roleController.canRemoveSystem)
                .map(_ && _),
              onItemClick = _.compose(
                _.withCurrentValueOf(controller.systemSelectionState).map(ss =>
                  MapUiEvent.RemoveSystemSelectionDialog(ss)
                )
              ) --> controller.mapUiEvents,
              isDestructive = true
            )
          ).view
    )
