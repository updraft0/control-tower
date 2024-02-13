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
    rds: ReferenceDataStore,
    positionController: PositionController
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
            (closeMe, owner) => systemAddView(actions, closeMe, rds, positionController)(using owner),
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
          Modal.showConfirmation(
            "Confirm removal",
            span(
              child.text <-- Signal
                .fromFuture(rds.systemForId(system.system.systemId))
                .map(_.flatten.map(_.name).getOrElse(s"?? ${system.system.systemId}"))
                .map(n => s"Remove system $n?")
            ),
            actions.contramap(_ => MapAction.Remove(system.system.systemId)),
            isDestructive = true
          )
        )
      ),
      toolbarButtonS(
        id = "pin-system",
        selected.map(_.exists(_.system.isPinned)).map {
          case false => "ti-pin-filled"
          case true  => "ti-pin"
        },
        disableWhenNotSelectedAndRole(selected, mapRole, RoleController.canPinUnpinSystem),
        onClick.stopPropagation.compose(_.sampleCollectSome(selected)) -->
          actions.contramap[MapSystemSnapshot](s => MapAction.TogglePinned(s.system.systemId))
      ),
      toolbarButton(
        id = "stance-friendly",
        icon = "ti-heart",
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
        icon = "ti-tank",
        disableWhenNotSelectedAndRole(selected, mapRole, RoleController.canUpdateIntelStance),
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
        disableWhenNotSelectedAndRole(selectedConnection, mapRole, RoleController.canChangeConnections),
        onClick.stopPropagation.compose(_.sampleCollectSome(selectedConnection)) --> (conn =>
          Modal.showConfirmation(
            "Confirm removal",
            span(
              child.text <-- Signal
                .fromFuture((for
                  fromSystem <- rds.systemForId(conn.connection.fromSystemId)
                  toSystem   <- rds.systemForId(conn.connection.toSystemId)
                yield fromSystem.map(_.name) -> toSystem.map(_.name)).map {
                  case (Some(fromName), Some(toName)) => Some(s"Remove connection from $fromName to $toName?")
                  case _                              => None
                })
                .map(
                  _.flatten.getOrElse(
                    s"Remove connection between $$${conn.connection.fromSystemId} to $$${conn.connection.toSystemId}"
                  )
                )
            ),
            actions.contramap(_ => MapAction.RemoveConnection(conn.connection.id)),
            isDestructive = true
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

private def systemAddView(
    bus: WriteBus[MapAction],
    closeMe: Observer[Unit],
    rds: ReferenceDataStore,
    pc: PositionController
)(using Owner) =
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
              name = Option.when(!mapName.isBlank)(NewSystemName.Name(mapName.trim)),
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
