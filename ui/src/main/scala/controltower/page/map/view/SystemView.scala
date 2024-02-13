package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.Constant
import controltower.component.Modal
import controltower.page.map.{Coord, MapAction, PositionController, RoleController}
import controltower.ui.{ViewController, onEnterPress}
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.*

import scala.collection.MapView
import scala.collection.mutable

import java.time.Instant

enum SignatureClassified(val name: String):
  case Wormhole(override val name: String, typeId: Long) extends SignatureClassified(name)
  case Other(override val name: String)                  extends SignatureClassified(name)

enum SystemScanStatus:
  case Unscanned
  case PartiallyScanned
  case FullyScanned
  case FullyScannedStale

case class SystemStaticData(
    solarSystemMap: MapView[Long, SolarSystem],
    wormholeTypes: Map[Long, WormholeType],
    signatureByClassAndGroup: Map[WormholeClass, Map[SignatureGroup, List[SignatureClassified]]]
)

object SystemStaticData:

  def apply(solarSystemMap: MapView[Long, SolarSystem], ref: Reference) =
    new SystemStaticData(
      solarSystemMap,
      wormholeTypes = ref.wormholeTypes.map(wt => wt.typeId -> wt).toMap,
      signatureByClassAndGroup = computeSignatures(ref)
    )

  private def computeSignatures(ref: Reference) =
    val whTypeIds   = ref.wormholeTypes.map(wt => wt.name -> wt.typeId).toMap
    val sigsByClass = mutable.Map.empty[WormholeClass, List[SignatureInGroup]]
    ref.signaturesInGroup.foreach(sig =>
      sig.targetClasses.foreach(whc =>
        sigsByClass.updateWith(whc) {
          case Some(prev) => Some(sig :: prev)
          case None       => Some(List(sig))
        }
      )
    )
    sigsByClass.view
      .mapValues(
        _.groupBy(_.signatureGroup).view
          .mapValues(_.map {
            case SignatureInGroup(SignatureGroup.Wormhole, name, _) =>
              SignatureClassified.Wormhole(name, whTypeIds(name))
            case SignatureInGroup(_, name, _) => SignatureClassified.Other(name)
          }.sortBy(_.name))
          .toMap
      )
      .toMap

/** Display a single system on the map and interact with it
  */
class SystemView(
    systemId: Long,
    system: Signal[MapSystemSnapshot],
    pos: PositionController,
    selectedSystem: Signal[Option[Long]],
    connectingState: Var[MapNewConnectionState],
    settings: Signal[MapSettings]
)(using ctx: MapViewContext)
    extends ViewController:

  override def view =
    if (!ctx.staticData.solarSystemMap.contains(systemId))
      div(
        idAttr := s"system-$systemId",
        cls    := "system-error",
        child.text <-- system.map(_.system.name.getOrElse(s"$systemId"))
      )
    else
      val solarSystem = ctx.staticData.solarSystemMap(systemId)

      val canDrag = ctx.mapRole
        .combineWith(system.map(_.system.isPinned))
        .map((role, pinned) => !pinned && RoleController.canRepositionSystem(role))

      inDraggable(
        pos.systemPosition(systemId),
        canDrag,
        c => ctx.actions.onNext(MapAction.Reposition(systemId, c.x, c.y)),
        { (box) =>
          val firstLine = div(
            cls := "system-status-line",
            systemClass(solarSystem),
            systemMapName(system, solarSystem),
            systemShattered(solarSystem),
            systemEffect(solarSystem),
            systemIsPinned(system.map(_.system)),
            systemScanStatus(system.map(_.signatures), settings, ctx.now)
          )

          val secondLine = div(
            cls := "system-status-line",
            systemClass(solarSystem, hide = true),
            systemName(solarSystem),
            systemWhStatics(solarSystem, ctx.staticData.wormholeTypes)
          )

          box.amend(
            idAttr := s"system-$systemId",
            cls    := "system",
            cls("system-selected") <-- selectedSystem.map(_.exists(_ == systemId)),
            // FIXME: this is also fired when we drag the element so selection is always changing
            onClick.preventDefault.stopPropagation.mapToUnit --> ctx.actions.contramap(_ =>
              MapAction.Select(Some(systemId))
            ),
            onDblClick.preventDefault.stopPropagation
              .compose(_.sample(ctx.mapRole).filter(RoleController.canRenameSystem).withCurrentValueOf(system)) -->
              Observer({ case (_, mss: MapSystemSnapshot) =>
                Modal.show(
                  (closeMe, owner) => systemRenameView(systemId, mss.system.name.getOrElse(""), ctx.actions, closeMe),
                  clickCloses = true,
                  cls := "system-rename-dialog"
                )
              }),
            // dragging connections handlers
            inContext(self =>
              Seq(
                onPointerDown.preventDefault.stopPropagation.compose(
                  _.delay(200).filter(pev => pev.isPrimary && pev.button == MouseButtonLeft && pev.shiftKey)
                ) --> { pev =>
                  self.ref.setPointerCapture(pev.pointerId)

                  val parent    = self.ref.parentNode.asInstanceOf[org.scalajs.dom.Element]
                  val parentDim = parent.getBoundingClientRect()
                  val coord     = Coord(pev.clientX - parentDim.left, pev.clientY - parentDim.top)
                  connectingState.set(MapNewConnectionState.Start(systemId, coord))
                },
                onPointerMove.preventDefault.stopPropagation.compose(
                  _.withCurrentValueOf(connectingState.signal).filter(_._2 != MapNewConnectionState.Stopped).map(_._1)
                ) --> { pev =>
                  val parent    = self.ref.parentNode.asInstanceOf[org.scalajs.dom.Element]
                  val parentDim = parent.getBoundingClientRect()
                  val coord     = Coord(pev.clientX - parentDim.left, pev.clientY - parentDim.top)
                  connectingState.set(MapNewConnectionState.Move(systemId, coord))
                }
              )
            ),
            onPointerUp.preventDefault.stopPropagation.mapTo(MapNewConnectionState.Stopped) --> connectingState,
            onPointerCancel.preventDefault.stopPropagation.mapTo(MapNewConnectionState.Stopped) --> connectingState,
            intelStance(system),
            firstLine,
            secondLine
          )
        }
      )

private inline def intelStance(s: Signal[MapSystemSnapshot]) =
  cls <-- s.map(s => (s.structures.nonEmpty, s.system.stance)).map {
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
    systemClassString(systemClass)
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
        cls := "ti-square-filled"
      )
    }

private inline def systemShattered(ss: SolarSystem) =
  Option.when(ss.name.startsWith("J0"))(
    mark(cls := "system-shattered", cls := "ti", cls := "ti-chart-donut-filled")
  )

private inline def systemMapName(s: Signal[MapSystemSnapshot], solarSystem: SolarSystem) =
  val nameSignal = s.map(_.system.name)
  mark(
    cls <-- nameSignal.map(_.map(_ => "system-map-name").getOrElse("system-name")),
    child.text <-- nameSignal.map(_.getOrElse(solarSystem.name))
  )

private inline def systemScanStatus(
    s: Signal[Array[MapSystemSignature]],
    settings: Signal[MapSettings],
    now: Signal[Instant]
) =
  val scanIsStale = now
    .withCurrentValueOf(s, settings)
    .map:
      case (now, signatures, settings) => scanStale(signatures, settings, now)
  val scanStatus = s
    .map(scanPercent(_, fullOnEmpty = false))
    .combineWith(scanIsStale)
    .map:
      case (d, true) if d > 99.99  => SystemScanStatus.FullyScannedStale
      case (d, false) if d > 99.99 => SystemScanStatus.FullyScanned
      case (d, _) if d > 0.01      => SystemScanStatus.PartiallyScanned
      case _                       => SystemScanStatus.Unscanned
  mark(
    cls := "system-scan-status",
    dataAttr("scan-status") <-- scanStatus.map(_.toString),
    cls := "ti",
    cls <-- scanStatus
      .map:
        case SystemScanStatus.Unscanned         => "ti-alert-circle-filled"
        case SystemScanStatus.PartiallyScanned  => "ti-alert-circle-filled"
        case SystemScanStatus.FullyScanned      => "ti-circle-filled"
        case SystemScanStatus.FullyScannedStale => "ti-clock-filled"
  )

private inline def systemIsPinned(s: Signal[MapSystem]) =
  mark(
    cls := "system-pin-status",
    display <-- s.map(_.isPinned).map {
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

private def systemRenameView(systemId: Long, name: String, actions: WriteBus[MapAction], closeMe: Observer[Unit]) =
  val nameVar = Var(name)
  div(
    cls := "system-rename-view",
    div(cls := "dialog-header", "Rename System"),
    systemNameInput(
      nameVar,
      autoFocus := true,
      onEnterPress --> { _ =>
        val newName = nameVar.now()
        if (newName != name) {
          actions.onNext(MapAction.Rename(systemId, name = Option.when(!newName.isBlank)(newName.trim)))
        }
        closeMe.onNext(())
      }
    )
  )

private[view] inline def systemNameInput(nameVar: Var[String], mods: Modifier[Input]*) =
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

private[view] inline def systemClassString(cls: WormholeClass) =
  cls match
    case WormholeClass.Pochven     => "P"
    case WormholeClass.Thera       => "T"
    case other if other.value > 10 => s"C${other.value}"
    case other                     => other.toString
