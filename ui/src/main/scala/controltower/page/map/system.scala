package controltower.page.map

import com.raquo.laminar.api.L.*
import controltower.component.Modal
import controltower.ui.FakeVarM
import controltower.Constant
import org.updraft0.controltower.constant.WormholeEffects
import org.updraft0.controltower.protocol.*

import scala.collection.MapView
import org.scalajs.dom

private type SystemVar = FakeVarM[Long, MapSystemSnapshot]
// arbitrary limit to system names (on UI side)
private val MaxSystemNameLength = 30

case class SystemStaticData(solarSystemMap: MapView[Long, SolarSystem], wormholeTypes: Map[Long, WormholeType])

/** View (controller) for displaying a single system "box"
  */
class SystemView(
    bus: WriteBus[MapRequest],
    staticData: SystemStaticData,
    selectedSystem: Var[Option[Long]]
):

  def view(systemId: Long, system: SystemVar)(using Owner): HtmlElement =
    if (!staticData.solarSystemMap.contains(systemId))
      div(
        idAttr := s"system-$systemId",
        cls    := "system-error",
        child.text <-- system.signal.map(_.system.name.getOrElse(s"$systemId"))
      )
    else
      val solarSystem = staticData.solarSystemMap(systemId)
      val currentPos  = systemPosition(system)

      inDraggable(
        currentPos,
        { (isDragging, box) =>
          isDragging
            .compose(_.withCurrentValueOf(currentPos.signal))
            .changes
            .foreach((isDragging, pos) =>
              if (!isDragging)
                bus
                  .onNext(
                    MapRequest
                      .UpdateSystem(systemId, displayData = Some(SystemDisplayData.Manual(pos.x.toInt, pos.y.toInt)))
                  )
            )

          val firstLine = div(
            cls := "system-status-line",
            systemClass(solarSystem),
            systemMapName(system, solarSystem),
            systemEffect(solarSystem),
            systemIsPinned(system)
          )

          val secondLine = div(
            cls := "system-status-line",
            systemClass(solarSystem, hide = true),
            systemName(solarSystem),
            systemWhStatics(solarSystem, staticData.wormholeTypes)
          )

          box.amend(
            idAttr := s"system-$systemId",
            cls    := "system",
            cls <-- selectedSystem.signal.map {
              case Some(`systemId`) => "system-selected"
              case _                => ""
            },
            // FIXME: this is also fired when we drag the element so selection is always changing
            onClick.preventDefault.stopPropagation.mapToUnit --> (_ => selectedSystem.set(Some(systemId))),
            onDblClick.preventDefault.stopPropagation.mapToUnit --> (_ =>
              Modal.show(
                (closeMe, owner) =>
                  systemRenameView(systemId, system.now().system.name.getOrElse(""), bus, closeMe)(using owner),
                clickCloses = true,
                cls := "system-rename-dialog"
              )
            ),
            intelStance(system),
            firstLine,
            secondLine
          )
        }
      )

private def systemPosition(v: SystemVar): FakeVarM[Long, Coord] =
  v.zoomIn({
    _.display match
      case Some(SystemDisplayData.Manual(x, y)) => Coord(x.toDouble, y.toDouble)
      case None                                 => Coord.Hidden // TODO support other display types
  })((mss, coord) => mss.copy(display = Some(SystemDisplayData.Manual(coord.x.toInt, coord.y.toInt))))

private inline def intelStance(v: SystemVar) =
  cls <-- v.signal.map(s => (s.structures.nonEmpty, s.system.stance)).map {
    case (_, IntelStance.Hostile)  => "system-stance-hostile"
    case (_, IntelStance.Friendly) => "system-stance-friendly"
    case (true, _)                 => "system-occupied"
    case (_, IntelStance.Unknown)  => "system-stance-unknown"
  }

private inline def systemClass(ss: SolarSystem, hide: Boolean = false) =
  val systemClass = ss.systemClass.get
  mark(
    cls        := "system-class",
    cls        := s"system-class-${systemClass.toString.toLowerCase}",
    visibility := Option.when(hide)("hidden").getOrElse(""),
    systemClass.toString
  )

private inline def systemName(ss: SolarSystem) =
  mark(
    cls := "system-name",
    ss.name
  )

private inline def systemEffect(ss: SolarSystem) =
  ss.effect
    .map { we =>
      val effect = WormholeEffects.ById(we.typeId)
      mark(
        cls := "system-effect",
        cls := s"system-effect-${effect.toString.toLowerCase}",
        cls := "ti",
        cls := "ti-square-filled" // hmnn this does not work otherwise
      )
    }

private inline def systemMapName(v: SystemVar, solarSystem: SolarSystem) =
  val nameSignal = v.signal.map(_.system.name)
  mark(
    cls <-- nameSignal.map(_.map(_ => "system-map-name").getOrElse("system-name")),
    child.text <-- nameSignal.map(_.getOrElse(solarSystem.name))
  )

private inline def systemIsPinned(v: SystemVar) =
  mark(
    cls := "system-pin-status",
    display <-- v.signal.map(_.system.isPinned).map {
      case true  => ""
      case false => "none"
    },
    cls := "ti",
    cls := "ti-pin-filled"
  )

private inline def systemWhStatics(ss: SolarSystem, wormholeTypes: Map[Long, WormholeType]) =
  ss.wormholeStatics.map { static =>
    val tpe = wormholeTypes(static.typeId)
    mark(
      cls := "system-wormhole-static",
      cls := s"system-wormhole-static-${static.name.toLowerCase}",
      cls := s"system-class-${tpe.targetClass.toString.toLowerCase}",
      tpe.targetClass.toString
    )
  }

private def systemRenameView(systemId: Long, name: String, bus: WriteBus[MapRequest], closeMe: Observer[Unit])(using
    Owner
) =
  val nameVar = Var(name)
  div(
    cls := "system-rename-view",
    div(cls := "dialog-header", "Rename System"),
    systemNameInput(
      nameVar,
      autoFocus := true,
      onKeyPress.filter(_.keyCode == dom.KeyCode.Enter) --> { _ =>
        val newName = nameVar.now()
        if (newName != name) {
          bus.onNext(MapRequest.UpdateSystem(systemId, name = Some(Option.when(!newName.isBlank)(newName))))
        }
        closeMe.onNext(())
      }
    )
  )

private[map] inline def systemNameInput(nameVar: Var[String], mods: Modifier[Input]*) =
  input(
    cls := "input-system-name",
    tpe := "text",
    controlled(
      value <-- nameVar,
      onInput.mapToValue.filter(_.length < Constant.MaxSystemNameLength) --> nameVar
    ),
    placeholder := "Name on map",
    maxLength   := Constant.MaxSystemNameLength,
    mods
  )
