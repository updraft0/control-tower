package controltower.page.map

import com.raquo.laminar.api.L.*
import org.updraft0.controltower.protocol.{IntelStance, MapRequest, MapSystemSnapshot, SolarSystem, SystemDisplayData}
import controltower.component.Modal
import controltower.Constant
import controltower.ui.*
import controltower.db.ReferenceDataStore

import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

class ToolbarView(currentSystem: StrictSignal[Option[Long]], bus: WriteBus[MapRequest], rds: ReferenceDataStore):

  def view = div(
    idAttr := "map-toolbar",
    toolbarButton(
      "add-system",
      "ti-circle-plus",
      onClick.stopPropagation.mapToUnit --> (_ =>
        Modal.show(
          (closeMe, owner) => systemAddView(bus, closeMe, rds)(using owner),
          clickCloses = true,
          cls := "system-add-dialog"
        )
      )
    ),
    toolbarButton(
      id = "remove-system",
      "ti-trash",
      disabled <-- currentSystem.map(_.isEmpty),
      onClick.stopPropagation.mapToUnit --> (_ =>
        Modal.show(
          (closeMe, owner) => systemConfirmRemoveView(bus, currentSystem.now().get, closeMe, rds)(using owner),
          clickCloses = true,
          cls := "system-remove-dialog"
        ),
      )
    ),
    toolbarButton(
      id = "pin-system",
      "ti-pin-filled",
      disabled <-- currentSystem.map(_.isEmpty),
      onClick.stopPropagation.mapToUnit --> (_ =>
        bus.onNext(MapRequest.UpdateSystem(currentSystem.now().get, isPinned = Some(true) /* FIXME */ ))
      )
    )
  )

private inline def toolbarButton(id: String, icon: String, mods: Modifier[Button]*) =
  button(idAttr := id, typ := "button", cls := "ti", cls := icon, cls := "toolbar-button", mods)

private object ErrorText:
  val NoSolarSystem        = "No solar system"
  val SystemNotFound       = "System not found"
  val MultipleSystemsFound = "Multiple systems found"
  val Backend              = "Could not lookup systems"

private def systemAddView(bus: WriteBus[MapRequest], closeMe: Observer[Unit], rds: ReferenceDataStore)(using Owner) =
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
          val req = MapRequest.AddSystem(
            systemId = solarSystem.id,
            name = Option.when(!mapName.isBlank)(mapName),
            isPinned = pinnedVar.now(),
            displayData = SystemDisplayData.Manual(0, 0), // FIXME manual display of system data
            stance = IntelStance.Unknown
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
    bus: WriteBus[MapRequest],
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
        bus.onNext(MapRequest.RemoveSystem(systemId))
        closeMe.onNext(())
      }
    )
  )
