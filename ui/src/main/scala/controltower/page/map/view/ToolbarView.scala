package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.Constant
import controltower.component.Modal
import controltower.db.ReferenceDataStore
import controltower.page.map.{MapAction, RoleController}
import controltower.ui.*
import org.updraft0.controltower.protocol.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

/** Render + control the toolbar of icons to perform map actions (e.g. add, delete, etc.)
  */
class ToolbarView(
    selected: Signal[Option[MapSystemSnapshot]],
    actions: WriteBus[MapAction],
    mapRole: Signal[MapRole],
    rds: ReferenceDataStore
) extends ViewController:

  override def view: Element =
    div(
      idAttr := "map-toolbar",
      toolbarButton(
        "add-system",
        "ti-circle-plus",
        disabled <-- mapRole.map(!RoleController.canAddSystem(_)),
        onClick.stopPropagation --> (_ =>
          Modal.show(
            (closeMe, owner) => systemAddView(actions, closeMe, rds)(using owner),
            clickCloses = true,
            cls := "system-add-dialog"
          )
        )
      ),
      toolbarButton(
        id = "remove-system",
        "ti-trash",
        disableWhenNotSelectedAndRole(selected, mapRole, RoleController.canRemoveSystem),
        onClick.stopPropagation.compose(_.sampleCollectSome(selected)) --> (system =>
          Modal.show(
            (closeMe, owner) => systemConfirmRemoveView(actions, system.system.systemId, closeMe, rds)(using owner),
            clickCloses = true,
            cls := "system-remove-dialog"
          ),
        )
      ),
      toolbarButtonS(
        id = "pin-system",
        selected.map(_.exists(_.system.isPinned)).map {
          case true  => "ti-pin-filled"
          case false => "ti-pin"
        },
        disableWhenNotSelectedAndRole(selected, mapRole, RoleController.canPinUnpinSystem),
        onClick.stopPropagation.compose(_.sampleCollectSome(selected)) -->
          actions.contramap[MapSystemSnapshot](s => MapAction.TogglePinned(s.system.systemId))
      ),
      toolbarButton(
        id = "stance-friendly",
        icon = "ti-friends",
        disableWhenNotSelectedAndRole(selected, mapRole, RoleController.canUpdateIntelStance),
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
        icon = "ti-friends-off",
        disableWhenNotSelectedAndRole(selected, mapRole, RoleController.canUpdateIntelStance),
        onClick.stopPropagation.compose(_.sampleCollectSome(selected)) -->
          actions.contramap[MapSystemSnapshot](s =>
            MapAction.IntelChange(
              s.system.systemId,
              if (s.system.stance == IntelStance.Hostile) IntelStance.Unknown else IntelStance.Hostile
            )
          )
      )
    )

private inline def disableWhenNotSelectedAndRole[A](
    selected: Signal[Option[A]],
    role: Signal[MapRole],
    roleAllows: MapRole => Boolean
) =
  disabled <-- selected.combineWith(role).map(sr => sr._1.forall(_ => !roleAllows(sr._2)))

private inline def toolbarButton(id: String, icon: String, mods: Modifier[Button]*) =
  button(idAttr := id, typ := "button", cls := "ti", cls := icon, cls := "toolbar-button", mods)

private inline def toolbarButtonS(id: String, icon: Signal[String], mods: Modifier[Button]*) =
  button(idAttr := id, typ := "button", cls := "ti", cls <-- icon, cls := "toolbar-button", mods)

private object ErrorText:
  val NoSolarSystem        = "No solar system"
  val SystemNotFound       = "System not found"
  val MultipleSystemsFound = "Multiple systems found"
  val Backend              = "Could not lookup systems"

private def systemAddView(bus: WriteBus[MapAction], closeMe: Observer[Unit], rds: ReferenceDataStore)(using Owner) =
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
              name = Option.when(!mapName.isBlank)(mapName),
              isPinned = pinnedVar.now(),
              displayData = SystemDisplayData.Manual(0, 0), // FIXME manual display of system data
              stance = IntelStance.Unknown
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
                case Success(List(system)) => error.set(None)
                case Success(Nil)          => error.set(Some(ErrorText.SystemNotFound))
                case Success(_)            => error.set(Some(ErrorText.MultipleSystemsFound))
                case Failure(_)            => error.set(Some(ErrorText.Backend))
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

private def systemConfirmRemoveView(
    bus: WriteBus[MapAction],
    systemId: Long,
    closeMe: Observer[Unit],
    rds: ReferenceDataStore
)(using Owner) =
  div(
    cls := "system-remove-view",
    cls := "dialog-view",
    div(cls := "dialog-header", "Confirm Remove"),
    div(
      span("Remove system "),
      span(
        child.text <-- Signal
          .fromFuture(rds.systemForId(systemId))
          .map(_.flatten.map(_.name).getOrElse(s"?? $systemId"))
      ),
      span("?")
    ),
    button(
      tpe := "button",
      cls := "remove-button",
      "Remove",
      onClick.mapToUnit --> { _ =>
        bus.onNext(MapAction.Remove(systemId))
        closeMe.onNext(())
      }
    )
  )
