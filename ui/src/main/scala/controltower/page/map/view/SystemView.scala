package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.Constant
import controltower.backend.ESI
import controltower.component.Modal
import controltower.page.map.{MapAction, PositionController, RoleController}
import controltower.ui.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.*

import scala.annotation.unused
import scala.collection.MapView
import scala.collection.mutable

import java.time.Instant

enum SignatureClassified(val name: String) derives CanEqual:
  case Wormhole(override val name: String, typeId: TypeId) extends SignatureClassified(name)
  case Other(override val name: String)                    extends SignatureClassified(name)

enum SystemScanStatus derives CanEqual:
  case Unscanned
  case PartiallyScanned
  case FullyScanned
  case FullyScannedStale

case class SystemStaticData(
    solarSystemMap: MapView[SystemId, SolarSystem],
    wormholeTypes: Map[TypeId, WormholeType],
    starTypes: Map[TypeId, StarType],
    shipTypes: Map[TypeId, ShipType],
    signatureByClassAndGroup: Map[WormholeClass, Map[SignatureGroup, List[SignatureClassified]]],
    structureTypes: Map[TypeId, StructureType]
)

object SystemStaticData:

  def apply(solarSystemMap: MapView[SystemId, SolarSystem], ref: Reference) =
    new SystemStaticData(
      solarSystemMap,
      wormholeTypes = ref.wormholeTypes.map(wt => wt.typeId -> wt).toMap,
      starTypes = ref.starTypes.map(st => st.typeId -> st).toMap,
      shipTypes = ref.shipTypes.map(st => st.typeId -> st).toMap,
      signatureByClassAndGroup = computeSignatures(ref),
      structureTypes = ref.structureTypes.map(st => st.typeAndName._1 -> st).toMap
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
    systemId: SystemId,
    system: Signal[MapSystemSnapshot],
    pos: PositionController,
    selectedSystem: Signal[Option[SystemId]],
    bulkSelectedSystems: Signal[Array[SystemId]],
    characters: Signal[Array[CharacterLocation]],
    connectingState: Var[MapNewConnectionState],
    isConnected: Signal[Boolean],
    settings: Signal[MapSettings],
    uiEvents: Observer[MapUiEvent]
)(using ctx: MapViewContext)
    extends ViewController:

  override def view =
    if (!ctx.staticData.solarSystemMap.contains(systemId))
      org.scalajs.dom.console.error(s"static solar system data does not contain id=$systemId")
      // TODO improve error view (or back out completely)
      div(
        idAttr := s"system-$systemId",
        cls    := "system-error",
        child.text <-- system.map(_.system.name.getOrElse(s"$systemId"))
      )
    else
      val solarSystem = ctx.staticData.solarSystemMap(systemId)

      val canDrag = ctx.roleController.canRepositionSystem
        .combineWith(system.map(_.system.isPinned))
        .map((ok, pinned) => ok && !pinned)

      inDraggable(
        pos.systemPosition(systemId),
        canDrag,
        c => ctx.actions.onNext(MapAction.Reposition(systemId, c.x, c.y)),
        { (box) =>
          val firstLine = div(
            cls := "system-status-line",
            systemClass(solarSystem),
            systemMapName(system, solarSystem),
            // systemShattered(solarSystem), TODO remove?
            systemOnlineChars(systemId, characters, ctx.staticData.shipTypes),
            systemEffect(solarSystem),
            systemIsPinned(system.map(_.system)),
            systemIntelGroupIndicator(system)
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
            cls("system-selected") <-- selectedSystem.map(_.contains(systemId)),
            cls("system-selected-bulk") <-- bulkSelectedSystems.map(_.contains(systemId)),
            // FIXME: this is also fired when we drag the element so selection is always changing
            onPointerUp
              .filter(_.filterWith(PointerFilter.PrimaryPointer | PointerFilter.MouseButtonLeft))
              .mapToUnit --> ctx.actions.contramap(_ => MapAction.Select(Some(systemId))),
            onPointerUp
              .filter(
                _.filterWith(PointerFilter.PrimaryPointer | PointerFilter.MouseButtonLeft | PointerFilter.CtrlKey)
              )
              .mapToUnit --> ctx.actions.contramap(_ => MapAction.ToggleBulkSelection(systemId)),
            onDblClick
              .filter(ev => !ev.ctrlKey && !ev.shiftKey && !ev.metaKey)
              .compose(
                _.filterWith(isConnected)
                  .sample(ctx.mapRole)
                  .filter(RoleController.canRenameSystem)
                  .withCurrentValueOf(system)
              ) -->
              ((_, mss) =>
                Modal.show(
                  (closeMe, _) => systemRenameView(systemId, mss.system.name.getOrElse(""), ctx.actions, closeMe),
                  Observer.empty[Unit],
                  true,
                  cls := "system-rename-dialog"
                )
              ),
            // context menu
            inContext(self =>
              onPointerDown
                .filter(_.filterWith(PointerFilter.PrimaryPointer | PointerFilter.MouseButtonRight))
                .stopPropagation
                .map(_.posRelativeToParent(self))
                .map(c => MapUiEvent.ContextMenu(ContextMenuRequest.System(c, systemId))) --> uiEvents
            ),
            // dragging connections handlers
            inContext(self =>
              modSeq(
                onPointerDown.preventDefault.stopPropagation.compose(
                  _.delay(200) // FIXME magic constant
                    .filter(
                      _.filterWith(
                        PointerFilter.PrimaryPointer | PointerFilter.MouseButtonLeft | PointerFilter.ShiftKey
                      )
                    )
                    .tapEach(pev => self.ref.setPointerCapture(pev.pointerId))
                    .map(_.posRelativeToParent(self))
                    .map(c => MapNewConnectionState.Start(systemId, c))
                ) --> connectingState,
                onPointerMove.preventDefault.stopPropagation.compose(
                  _.filterWith(connectingState.signal.map(_ != MapNewConnectionState.Stopped))
                    .map(_.posRelativeToParent(self))
                    .map(c => MapNewConnectionState.Move(systemId, c))
                ) --> connectingState
              )
            ),
            onPointerUp.preventDefault.stopPropagation.mapTo(MapNewConnectionState.Stopped) --> connectingState,
            onPointerCancel.preventDefault.stopPropagation.mapTo(MapNewConnectionState.Stopped) --> connectingState,
            intelStance(system),
            div(
              cls := "status-lines",
              firstLine,
              secondLine
            ),
            // do not display scan status for kspace
            if (solarSystem.systemClass.exists(_.spaceType == SpaceType.Known))
              mark(cls := "system-scan-status", nbsp)
            else systemScanStatus(system.map(_.signatures), settings, ctx.now)
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

private inline def systemOnlineChars(
    systemId: SystemId,
    chars: Signal[Array[CharacterLocation]],
    shipTypes: Map[TypeId, ShipType]
) =
  nodeSeq(
    mark(
      cls       := "system-online-chars",
      cls       := "tooltip-target-adjacent",
      styleAttr := s"anchor-name: --online-${systemId}",
      display <-- chars.map(arr => if (arr.isEmpty) "none" else ""),
      text <-- chars.map(_.length.toString)
    ),
    div(
      cls       := "tooltip",
      cls       := "tooltip-on-left",
      cls       := "online-chars-tooltip",
      styleAttr := s"--anchor-var: --online-${systemId}",
      h3(cls := "tooltip-title", "Pilots"),
      table(
        thead(
          th(cls := "character-image"),
          th(cls := "character-name", "Char"),
          th(cls := "ship-image"),
          th(cls := "ship-type"),
          th(cls := "ship-name", "Ship"),
          th(
            cls := "ship-points",
            child.text <-- chars
              .map(_.foldLeft(0L)((_, cl) => shipTypes(cl.shipTypeId).mass))
              .map(pointsFromMass)
              .map(p => s"∑ $p")
          ),
          th("")
        ),
        tbody(
          children <-- chars.map(_.toList).split(_.characterId)(renderLocationRow(shipTypes))
        )
      )
    )
  )

private[map] inline def renderLocationRow(
    shipTypes: Map[TypeId, ShipType]
)(charId: CharacterId, location: CharacterLocation, sig: Signal[CharacterLocation]) =
  tr(
    td(
      cls := "character-image",
      ESI.characterImage(charId, location.characterName, size = MagicConstant.CharacterImageSize)
    ),
    td(cls := "character-name", location.characterName),
    td(cls := "ship-image", child <-- sig.map(cl => ESI.typeIcon(cl.shipTypeId))),
    td(cls := "ship-type", child.text <-- sig.map(cl => shipTypes(cl.shipTypeId).name)),
    td(cls := "ship-name", child.text <-- sig.map(cl => cl.shipName)),
    td(cls := "ship-points", child.text <-- sig.map(cl => pointsFromMass(shipTypes(cl.shipTypeId).mass).toString)),
    td(
      cls := "status",
      i(cls := "ti", cls <-- sig.map(cl => if (cl.structureId.isDefined || cl.stationId.isDefined) "ti-home" else ""))
    )
  )

private inline def systemName(ss: SolarSystem) =
  mark(
    cls := "system-name",
    ss.name
  )

private inline def systemEffect(ss: SolarSystem) =
  ss.effect.toSeq
    .flatMap { we =>
      val effect = WormholeEffects.ById(we.typeId)
      nodeSeq(
        mark(
          cls       := "system-effect",
          cls       := s"system-effect-${effect.toString.toLowerCase}",
          cls       := "tooltip-target-adjacent",
          styleAttr := s"anchor-name: --effect-${ss.id}-${effect.typeId}",
          cls       := "ti",
          cls       := "ti-square-filled"
        ),
        effectTooltip(ss.systemClass.get, effect, s"effect-${ss.id}-${effect.typeId}")
      )

    }

private inline def systemShattered(ss: SolarSystem) =
  Option.when(ss.name.startsWith("J0"))(
    mark(cls := "system-shattered", cls := "ti", cls := "ti-storm")
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
  // TODO: this now does not correspond to the scan status indicator on the signature view
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
    dataAttr("scan-status") <-- scanStatus.map(_.toString)
  )

private inline def systemIntelGroupIndicator(@unused s: Signal[MapSystemSnapshot]) =
  // TODO - add support for intel groups
  mark(cls := "ti", cls := "system-group-indicator", dataAttr("intel-group") := IntelGroup.Unknown.toString)

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

private inline def systemWhStatics(ss: SolarSystem, wormholeTypes: Map[TypeId, WormholeType]) =
  ss.wormholeStatics.flatMap: static =>
    val whType = wormholeTypes(static.typeId)
    nodeSeq(
      mark(
        cls                      := "system-wormhole-static",
        dataAttr("static-name")  := static.name,
        dataAttr("static-class") := whType.targetClass.toString,
        styleAttr                := s"anchor-name: --static-${ss.id}-${whType.typeId}",
        cls                      := "tooltip-target-adjacent",
        // TODO remove these
        cls := s"system-wormhole-static-${static.name.toLowerCase}",
        cls := s"system-class-${whType.targetClass.toString.toLowerCase}",
        whType.targetClass.toString
      ),
      staticTooltip(whType, s"static-${ss.id}-${whType.typeId}")
    )

private def systemRenameView(systemId: SystemId, name: String, actions: WriteBus[MapAction], closeMe: Observer[Unit]) =
  val nameVar = Var(name)
  div(
    cls := "system-rename-view",
    h2(cls := "dialog-header", "Rename System"),
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
  cls.tag

private inline def pointsFromMass(mass: Long) =
  (mass / 10_000_000).toDouble / 10.0d
