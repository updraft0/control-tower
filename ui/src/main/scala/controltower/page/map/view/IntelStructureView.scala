package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.backend.{ControlTowerBackend, ESI}
import controltower.component.*
import controltower.page.map.MapAction
import controltower.ui.*
import org.updraft0.controltower.constant.{SystemId, TypeId}
import org.updraft0.controltower.protocol.*

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContext.Implicits.global

final class IntelStructureView(systemId: Signal[Option[SystemId]], selected: Signal[Array[IntelSystemStructure]])(using
    ctx: MapViewContext,
    ct: ControlTowerBackend
) extends ViewController:

  private val structureGrid = MiniGrid[IntelSystemStructure](
    GridConfig(
      pageSize = None,
      initialSort = Some("Name" -> SortDirection.Asc),
      overrideSort = None,
      toolbarMods = nodeSeq(
        button(
          tpe := "button",
          i(cls := "ti", cls := "ti-plus", title := "Add structure"),
          disabled <-- ctx.roleController.canAddIntelStructure.invert,
          onClick.compose(_.sampleCollectSome(systemId)) --> (sysId =>
            Modal.show(
              (closeMe, owner) => addEditStructureView(sysId, None, ctx.actions, closeMe),
              Observer.empty[Unit],
              true,
              cls := "system-add-structure-dialog"
            )
          )
        )
      ),
      mods = Nil
    ),
    Array(
      Column[IntelSystemStructure, Option[Boolean]](
        name = "Online",
        nameEl = i(cls := "ti", cls := "ti-bolt", title := "Online"),
        tpe = ColumnType.Number,
        select = _.isOnline,
        view = (_, isOpt) =>
          val icon = if (isOpt.contains(true)) "ti-bolt-filled" else "ti-bolt"
          span(i(cls := "ti", cls := icon))
        ,
        sortable = None
      ),
      Column[IntelSystemStructure, StructureType](
        name = "StructureType",
        nameEl = i(cls := "ti", cls := "ti-building-factory-2", title := "Structure Type"),
        tpe = ColumnType.Number,
        select = _.`type`,
        view = (_, t) =>
          val (tId, tName) = t.typeAndName
          span(
            ESI.typeIcon(tId, Some(tName)),
            nbsp,
            tName
          )
        ,
        sortable = None
      ),
      Column[IntelSystemStructure, Option[String]](
        name = "Name",
        nameEl = i(cls := "ti", cls := "ti-abc", title := "Name"),
        tpe = ColumnType.Text,
        select = _.name,
        view = (_, t) => span(t.getOrElse("")),
        sortable = Some(Ordering[Option[String]])
      ),
      Column[IntelSystemStructure, Option[Corporation]](
        name = "Owner",
        nameEl = i(cls := "ti", cls := "ti-crown", title := "Owner"),
        tpe = ColumnType.Text,
        select = _.ownerCorporation,
        view = (_, t) =>
          t.map(c =>
            span(
              cls := "owner-corporation",
              ESI.corporationImage(c.id, c.name),
              nbsp,
              c.name
            )
          ).getOrElse(span()),
        sortable = None
      ),
      Column[IntelSystemStructure, Option[Int]](
        name = "Planet",
        nameEl = i(cls := "ti", cls := "ti-planet", title := "Planet"),
        tpe = ColumnType.Number,
        select = _.nearestPlanetIdx,
        view = (_, t) => span(t.map(romanNumeral).getOrElse("")),
        sortable = Some(Ordering[Option[Int]])
      ),
      Column[IntelSystemStructure, Option[Int]](
        name = "Moon",
        nameEl = i(cls := "ti", cls := "ti-moon", title := "Moon"),
        tpe = ColumnType.Number,
        select = _.nearestMoonIdx,
        view = (_, t) => span(t.map(romanNumeral).getOrElse("")),
        sortable = None
      ),
      Column[IntelSystemStructure, Instant](
        name = "UpdatedAt",
        nameEl = i(cls := "ti", cls := "ti-clock", title := "Updated at"),
        tpe = ColumnType.DateTime,
        select = _.updatedAt,
        view = (_, t) => span(IntelStructureView.DateFormatter.format(t), title := t.toString),
        sortable = Some(Ordering[Instant])
      ),
      Column[IntelSystemStructure, Unit](
        name = "Edit",
        nameEl = i(cls := "ti", cls := "ti-pencil", title := "Edit"),
        tpe = ColumnType.Text,
        select = _ => (),
        view = (ss, _) =>
          button(
            tpe := "button",
            i(cls := "ti", cls := "ti-pencil", title := "Edit Structure"),
            disabled <-- ctx.roleController.canEditIntelStructure.invert,
            onClick.compose(_.sampleCollectSome(systemId)) --> (sysId =>
              Modal.show(
                (closeMe, owner) => addEditStructureView(sysId, Some(ss), ctx.actions, closeMe),
                Observer.empty[Unit],
                true,
                cls := "system-edit-structure-dialog"
              )
            )
          ),
        sortable = None
      ),
      Column[IntelSystemStructure, Unit](
        name = "Delete",
        nameEl = i(cls := "ti", cls := "ti-trash", title := "Delete"),
        tpe = ColumnType.Text,
        select = _ => (),
        view = (ss, _) =>
          button(
            tpe := "button",
            i(cls := "ti", cls := "ti-trash", title := "Delete Structure"),
            disabled <-- ctx.roleController.canRemoveIntelStructure.invert,
            onClick.mapTo(MapAction.RemoveIntelStructure(ss.systemId, ss.id)) --> ctx.actions
          ),
        sortable = None
      )
    ),
    _.id.toString,
    selected
  )

  override def view: Element = sectionTag(
    idAttr := "intel-structure-view",
    cls    := "intel-structure-view",
    cls    := "bottom-column-view",
    h3(cls := "section-title", "Structures"),
    structureGrid.view
  )

object IntelStructureView:
  private val DateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd").withZone(ZoneId.of("UTC"))

enum StructureCategory derives CanEqual:
  case Unknown, Upwell, PlayerOwned

private inline def structureCategory(st: StructureType): StructureCategory = st match
  case _: StructureType.PlayerOwned => StructureCategory.PlayerOwned
  case _: StructureType.Upwell      => StructureCategory.Upwell

private inline def upwellStructureSize(st: StructureType): Option[UpwellStructureSize] = st match
  case u: StructureType.Upwell => Some(u.`type`.size)
  case _                       => None

private inline def posStructureSize(st: StructureType): Option[PlayerStructureSize] = st match
  case p: StructureType.PlayerOwned => Some(p.size)
  case _                            => None

private given DropdownItem[StructureCategory] with
  def key(sc: StructureCategory): String           = sc.toString
  def group(sc: StructureCategory): Option[String] = None
  def view(sc: StructureCategory): Element         = span(dataAttr("structure-category") := sc.toString, sc.toString)

private given DropdownItem[UpwellStructureSize] with
  def key(ss: UpwellStructureSize): String           = ss.toString
  def group(ss: UpwellStructureSize): Option[String] = None
  def view(ss: UpwellStructureSize): Element         = span(dataAttr("structure-size") := ss.toString, ss.toString)

private given DropdownItem[PlayerStructureSize] with
  def key(ss: PlayerStructureSize): String           = ss.toString
  def group(ss: PlayerStructureSize): Option[String] = None
  def view(ss: PlayerStructureSize): Element         = span(dataAttr("structure-size") := ss.toString, ss.toString)

private given DropdownItem[(TypeId, String)] with
  def key(ts: (TypeId, String)): String           = ts._1.toString
  def group(ts: (TypeId, String)): Option[String] = None
  def view(ts: (TypeId, String)): Element =
    if (ts._1 == TypeId.Invalid) span("Invalid")
    else span(dataAttr("structure-type-id") := ts._1.toString, ESI.typeIcon(ts._1, Some(ts._2)), ts._2)

private def searchCorporation(name: String)(using ct: ControlTowerBackend): EventStream[Option[Corporation]] =
  if (name.length < 4) EventStream.fromValue(None)
  else
    EventStream.fromFuture:
      ct.searchEntity(SearchType.Corporation, name)
        .map[Option[Corporation]]:
          case Left(error) =>
            org.scalajs.dom.console.error(s"Search for corporation failed with ${error}")
            None
          case Right(Array(SearchEntityResponse.OfCorporation(corporation))) => Some(corporation)
          case Right(_)                                                      => None

private def romanNumeral(i: Int): String =
  i match
    case i if i < 0  => "?" // give up
    case 0           => ""
    case i if i < 4  => "I".repeat(i)
    case 4           => "IV"
    case 5           => "V"
    case i if i < 9  => s"V${romanNumeral(i - 5)}"
    case 9           => "IX"
    case 10          => "X"
    case i if i < 40 => s"${"X".repeat(i / 10)}${romanNumeral(i % 10)}"
    case 40          => "XL"
    case i if i < 50 => s"${"XL"}${romanNumeral(i % 10)}"
    case 50          => "L"
    case i if i > 50 => "?" // give up, nobody has that many planets

private def addEditStructureView(
    systemId: SystemId,
    existing: Option[IntelSystemStructure],
    actions: WriteBus[MapAction],
    closeMe: Observer[Unit]
)(using ctx: MapViewContext, ct: ControlTowerBackend) =
  val category      = Var(existing.map(ss => structureCategory(ss.`type`)).getOrElse(StructureCategory.Unknown))
  val upwellSize    = Var(existing.flatMap(ss => upwellStructureSize(ss.`type`)).getOrElse(UpwellStructureSize.Medium))
  val playerSize    = Var(existing.flatMap(ss => posStructureSize(ss.`type`)).getOrElse(PlayerStructureSize.Small))
  val structureType = Var(existing.map(ss => ss.`type`.typeAndName).getOrElse((TypeId.Invalid, ""))).distinct
  val name          = Var(existing.flatMap(_.name).getOrElse(""))
  val planetIdx     = Var(existing.flatMap(ss => ss.nearestPlanetIdx).map(_.toString).getOrElse(""))
  val moonIdx       = Var(existing.flatMap(ss => ss.nearestMoonIdx).map(_.toString).getOrElse(""))
  val isOnline      = Var(existing.flatMap(ss => ss.isOnline).getOrElse(true))
  val ownerCorporation = Var(existing.flatMap(ss => ss.ownerCorporation.map(_.name)).getOrElse(""))
  val lastCorporation  = Var(Option.empty[Corporation])

  val validationError = Var(Option.empty[String])

  val maxPlanetIdx = ctx.staticData.solarSystemMap.get(systemId).map(_.planets.length).getOrElse(0)
  // note: no filtering of moon idx because we do not have this data as reference

  val playerTypes = ctx.staticData.structureTypes.view.values.collect[StructureType.PlayerOwned]:
    case p: StructureType.PlayerOwned => p

  val upwellTypes = ctx.staticData.structureTypes.view.values.collect[StructureType.Upwell]:
    case u: StructureType.Upwell => u

  val categorySelect = OptionDropdown[StructureCategory](
    ArraySeq.unsafeWrapArray(StructureCategory.values),
    category,
    mods = modSeq(idAttr := "intel-structure-category")
  )

  val upwellSizeSelect = OptionDropdown[UpwellStructureSize](
    ArraySeq.unsafeWrapArray(UpwellStructureSize.values),
    upwellSize,
    mods = modSeq(idAttr := "structure-size")
  )

  val playerSizeSelect = OptionDropdown[PlayerStructureSize](
    ArraySeq.unsafeWrapArray(PlayerStructureSize.values),
    playerSize,
    mods = modSeq(idAttr := "intel-structure-size")
  )

  val allowedTypes =
    category.signal
      .combineWith(upwellSize.signal, playerSize.signal)
      .distinct
      .map:
        case (StructureCategory.Unknown, _, _) => Nil
        case (StructureCategory.PlayerOwned, _, pSize) =>
          playerTypes.withFilter(_.size == pSize).map(_.typeAndName).toSeq.sortBy(_._2)
        case (StructureCategory.Upwell, uSize, _) =>
          upwellTypes.withFilter(_.`type`.size == uSize).map(_.typeAndName).toSeq.sortBy(_._2)

  div(
    cls := "system-add-intel-structure-view",
    h2(cls := "dialog-header", s"${existing.map(_ => "Edit").getOrElse("Add")} structure"),
    p(
      cls := "form-error",
      display <-- validationError.signal.map(_.isDefined).map(b => if (b) "" else "hidden")
    ),
    form(
      fieldSet(
        legend("Type"),
        p(
          label(forId := "intel-structure-category", "Category"),
          categorySelect.view
        ),
        p(
          label(forId := "intel-structure-size", "Size"),
          child <-- category.signal.map:
            case StructureCategory.Upwell      => upwellSizeSelect.view
            case StructureCategory.PlayerOwned => playerSizeSelect.view
            case StructureCategory.Unknown     => span("Select a category")
        ),
        p(
          label(forId := "intel-structure-type", "Type"),
          child <-- allowedTypes.map(types =>
            OptionDropdown[(TypeId, String)](
              types,
              structureType,
              mods = modSeq(idAttr := "intel-structure-type")
            ).view
          ),
          allowedTypes --> (types =>
            structureType.update((prevId, _) => types.find(_._1 == prevId).getOrElse((TypeId.Invalid, "")))
          )
        )
      ),
      fieldSet(
        legend("Name"),
        p(
          label(forId := "intel-structure-name", "Name"),
          input(
            idAttr := "intel-structure-name",
            controlled(
              value <-- name.signal,
              onInput.mapToValue --> name
            )
          )
        ),
        p(
          label(forId := "intel-structure-is-online", "Online"),
          input(
            idAttr := "intel-structure-is-online",
            tpe    := "checkbox",
            controlled(
              checked <-- isOnline.signal,
              onInput.mapToChecked --> isOnline
            )
          )
        )
      ),
      fieldSet(
        legend("Location"),
        p(
          label(forId := "intel-structure-nearest-planet", "Nearest Planet"),
          input(
            idAttr := "intel-structure-nearest-planet",
            controlled(
              value <-- planetIdx.signal,
              onInput.mapToValue
                .filter(s => s.isEmpty || s.toIntOption.exists(i => i > 0 && i <= maxPlanetIdx)) --> planetIdx
            )
          )
        ),
        p(
          label(forId := "intel-structure-nearest-moon", "Nearest Moon"),
          input(
            idAttr := "intel-structure-nearest-moon",
            controlled(
              value <-- moonIdx.signal,
              onInput.mapToValue
                .filter(s => s.isEmpty || s.toIntOption.exists(_ >= 0)) --> moonIdx
            )
          )
        )
      ),
      fieldSet(
        legend("Owner"),
        p(
          label(forId := "intel-structure-owning-corporation", "Owning Corporation"),
          input(
            idAttr := "intel-structure-owning-corporation",
            controlled(
              value <-- ownerCorporation.signal,
              onInput.mapToValue --> ownerCorporation
            )
          ),
          p(
            cls := "found-corporation",
            children <-- ownerCorporation.signal.distinct.toStream
              .throttle(500, leading = false)
              .flatMapSwitch(searchCorporation(_))
              .map: corpOpt =>
                corpOpt
                  .map: c =>
                    lastCorporation.set(Some(c))
                    nodeSeq(
                      ESI.corporationImage(c.id, c.name),
                      c.name
                    )
                  .getOrElse(nodeSeq())
          )
        )
      ),
      button(
        tpe := "button",
        cls := "add-structure",
        i(cls := "ti", cls := existing.map(_ => "ti-pencil").getOrElse("ti-plus")),
        s"${existing.map(_ => "edit").getOrElse("add")} structure",
        onClick.preventDefault.compose(
          _.mapToUnit
            .withCurrentValueOf(
              structureType,
              name,
              isOnline,
              ownerCorporation,
              lastCorporation,
              planetIdx,
              moonIdx
            )
        )
          --> { (tpe, name, isOnline, ownerCorp, lastCorp, planet, moon) =>
            if (tpe._1 == TypeId.Invalid) validationError.set(Some("Invalid structure type"))
            else if (!ownerCorp.isEmpty && !lastCorp.exists(c => c.name.startsWith(ownerCorp)))
              validationError.set(Some("Invalid corporation"))
            else
              val stype         = ctx.staticData.structureTypes(tpe._1)
              val nameOpt       = Option.when(!name.isEmpty)(name)
              val nearestPlanet = planet.toIntOption
              val nearestMoon   = moon.toIntOption
              val owner         = if (ownerCorp.isEmpty) None else lastCorp

              val update = existing
                .map(prev =>
                  MapAction.UpdateIntelSystemStructure(
                    prev.systemId,
                    prev.copy(
                      `type` = stype,
                      name = nameOpt,
                      ownerCorporation = owner,
                      nearestPlanetIdx = nearestPlanet,
                      nearestMoonIdx = nearestMoon,
                      isOnline = Some(isOnline)
                    )
                  )
                )
                .getOrElse(
                  MapAction.AddIntelSystemStructure(
                    systemId,
                    NewIntelSystemStructure(
                      `type` = stype,
                      name = nameOpt,
                      ownerCorporation = owner.map(_.id),
                      nearestPlanetIdx = nearestPlanet,
                      nearestMoonIdx = nearestMoon,
                      isOnline = Some(isOnline)
                    )
                  )
                )

              actions.onNext(update)
              closeMe.onNext(None)
          }
      )
    )
  )
