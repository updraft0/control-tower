package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.Constant
import controltower.component.Modal
import controltower.db.ReferenceDataStore
import controltower.page.map.{MapAction, PositionController, RoleController}
import controltower.ui.*
import org.updraft0.controltower.protocol.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

/** Render + control the toolbar of icons to perform map actions (e.g. add, delete, etc.)
  */
class ToolbarView(
    selected: Signal[Option[MapSystemSnapshot]],
    selectedConnection: Signal[Option[MapWormholeConnectionWithSigs]],
    actions: WriteBus[MapAction],
    mapRole: Signal[MapRole],
    isConnected: Signal[Boolean],
    rds: ReferenceDataStore,
    positionController: PositionController
) extends ViewController: // TODO use the MapCtx

  override def view: Element =
    div(
      idAttr := "map-toolbar",
      // fix selection view popping up after click
      onPointerDown.stopImmediatePropagation --> Observer.empty,
      // fix custom context menu popping up after right click (not completely for some reason)
      onContextMenu.stopImmediatePropagation --> Observer.empty,
      // buttons
      toolbarButton(
        "add-system",
        "ti-circle-plus",
        disabled <-- mapRole.map(!RoleController.canAddSystem(_)).combineWith(isConnected).map(_ || !_),
        onClick.stopPropagation --> (_ =>
          Modal.show(
            (closeMe, _) => systemAddView(actions, closeMe, rds, positionController),
            Observer.empty[Unit],
            clickCloses = true,
            cls := "system-add-dialog"
          )
        )
      ),
      toolbarButton(
        id = "remove-system",
        "ti-trash",
        disableWhenNotSelectedAndRole(selected, mapRole, RoleController.canRemoveSystem, isConnected),
        onClick.stopPropagation.compose(_.sampleCollectSome(selected)) --> (system =>
          removeSystemConfirm(system, actions, Observer.empty)(using rds)
        )
      ),
      toolbarButtonS(
        id = "pin-system",
        selected.map(_.exists(_.system.isPinned)).map {
          case false => "ti-pin-filled"
          case true  => "ti-pin"
        },
        disableWhenNotSelectedAndRole(selected, mapRole, RoleController.canPinUnpinSystem, isConnected),
        onClick.stopPropagation.compose(_.sampleCollectSome(selected)) -->
          actions.contramap[MapSystemSnapshot](s => MapAction.TogglePinned(s.system.systemId))
      ),
      toolbarButton(
        id = "stance-friendly",
        icon = "ti-heart",
        disableWhenNotSelectedAndRole(selected, mapRole, RoleController.canUpdateIntelStance, isConnected),
        onClick.stopPropagation.compose(_.sampleCollectSome(selected)) -->
          actions.contramap[MapSystemSnapshot](s =>
            MapAction.IntelChange(
              s.system.systemId,
              if (s.system.stance == IntelStance.Friendly) IntelStance.Unknown else IntelStance.Friendly
            )
          )
      ),
      toolbarButton(
        id = "stance-hostile",
        icon = "ti-swords",
        disableWhenNotSelectedAndRole(selected, mapRole, RoleController.canUpdateIntelStance, isConnected),
        onClick.stopPropagation.compose(_.sampleCollectSome(selected)) -->
          actions.contramap[MapSystemSnapshot](s =>
            MapAction.IntelChange(
              s.system.systemId,
              if (s.system.stance == IntelStance.Hostile) IntelStance.Unknown else IntelStance.Hostile
            )
          )
      ),
      toolbarButton(
        id = "remove-connection",
        icon = "ti-link-off",
        disableWhenNotSelectedAndRole(selectedConnection, mapRole, RoleController.canChangeConnections, isConnected),
        onClick.stopPropagation.compose(_.sampleCollectSome(selectedConnection)) --> (conn =>
          removeConnection(conn, actions, Observer.empty)(using rds)
        )
      )
    )

private inline def disableWhenNotSelectedAndRole[A](
    selected: Signal[Option[A]],
    role: Signal[MapRole],
    roleAllows: MapRole => Boolean,
    isConnected: Signal[Boolean]
) =
  disabled <-- selected.combineWith(role, isConnected).map(sr => !sr._3 || sr._1.forall(_ => !roleAllows(sr._2)))

private inline def toolbarButton(id: String, icon: String, mods: Modifier[Button]*) =
  button(idAttr := id, typ := "button", cls := "ti", cls := icon, cls := "toolbar-button", mods)

private inline def toolbarButtonS(id: String, icon: Signal[String], mods: Modifier[Button]*) =
  button(idAttr := id, typ := "button", cls := "ti", cls <-- icon, cls := "toolbar-button", mods)

private object ErrorText:
  val NoSolarSystem        = "No solar system"
  val SystemNotFound       = "System not found"
  val MultipleSystemsFound = "Multiple systems found"
  val Backend              = "Could not lookup systems"

private def systemAddView(
    bus: WriteBus[MapAction],
    closeMe: Observer[Unit],
    rds: ReferenceDataStore,
    pc: PositionController
) =
  val systemVar = Var("")
  val nameVar   = Var("")
  val pinnedVar = Var(false)
  val error     = Var[Option[String]](None)

  def onSubmit(): Unit =
    val solarSystemName = systemVar.now()
    val mapName         = nameVar.now()
    if (solarSystemName.isBlank)
      error.set(Some(ErrorText.NoSolarSystem))
    else
      rds.searchSystemName(solarSystemName).onComplete {
        case Success(List(solarSystem)) =>
          val req = MapAction.Direct(
            MapRequest.AddSystem(
              systemId = solarSystem.id,
              name = Option.when(!mapName.isBlank)(Some(mapName.trim)),
              isPinned = pinnedVar.now(),
              displayData = pc.newSystemDisplay,
              stance = None // FIXME add select for this
            )
          )
          bus.onNext(req)
          closeMe.onNext(())
        case Success(Nil) => error.set(Some(ErrorText.SystemNotFound))
        case Success(_)   => error.set(Some(ErrorText.MultipleSystemsFound))
        case Failure(_)   => error.set(Some(ErrorText.Backend))
      }

  div(
    cls := "system-add-view",
    cls := "dialog-view",
    div(cls := "dialog-header", "Add System"),
    // TODO: implement autocomplete etc.
    div(
      cls := "table",
      div(
        cls := "row",
        label("System:"),
        input(
          controlled(
            value <-- systemVar,
            onInput.mapToValue
              .filter(s => s.length < Constant.MaxSolarSystemNameLength && !s.contains(' ')) --> { s =>
              rds.searchSystemName(s).onComplete {
                case Success(List(_)) => error.set(None)
                case Success(Nil)     => error.set(Some(ErrorText.SystemNotFound))
                case Success(_)       => error.set(Some(ErrorText.MultipleSystemsFound))
                case Failure(_)       => error.set(Some(ErrorText.Backend))
              }
              systemVar.set(s)
            }
          ),
          cls         := "input-solar-system",
          tpe         := "text",
          placeholder := "Solar System",
          autoFocus   := true,
          maxLength   := Constant.MaxSolarSystemNameLength,
          onEnterPress.mapToUnit --> (_ => onSubmit())
        )
      ),
      div(
        cls := "row",
        label("Name:"),
        systemNameInput(nameVar, onEnterPress.mapToUnit --> (_ => onSubmit()))
      ),
      div(
        cls := "row",
        label("Pin:"),
        input(typ := "checkbox", controlled(checked <-- pinnedVar, onInput.mapToChecked --> pinnedVar))
      )
    ),
    div(
      cls := "submit",
      mark(
        cls := "form-error",
        cls("hidden") <-- error.signal.map(_.isDefined),
        child.text <-- error.signal.map(_.getOrElse(""))
      ),
      button(
        typ := "button",
        cls := "add-button",
        i(cls := "ti", cls := "ti-plus"),
        "Add",
        onClick.mapToUnit --> (_ => onSubmit())
      )
    )
  )
