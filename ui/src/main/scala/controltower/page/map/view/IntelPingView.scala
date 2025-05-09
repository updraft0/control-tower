package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.backend.ESI
import controltower.component.*
import controltower.page.map.MapAction
import controltower.ui.*
import org.updraft0.controltower.constant.{CharacterId, SystemId}
import org.updraft0.controltower.protocol.{IntelSystemPing, IntelSystemPingTarget}

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import scala.collection.immutable.ArraySeq

final class IntelPingView(systemId: Signal[Option[SystemId]], selected: Signal[Array[IntelSystemPing]])(using
    ctx: MapViewContext
) extends ViewController:

  private val pingGrid = MiniGrid[IntelSystemPing](
    GridConfig(
      pageSize = Some(10),
      initialSort = Some("CreatedAt" -> SortDirection.Desc),
      overrideSort = Some("Target" -> SortDirection.Asc),
      toolbarMods = nodeSeq(
        button(
          tpe := "button",
          i(cls := "ti", cls := "ti-plus", title := "Add ping"),
          disabled <-- ctx.roleController.canAddIntelPing.invert,
          onClick.compose(_.sampleCollectSome(systemId)) --> (sysId =>
            Modal.show(
              (closeMe, _) => addIntelPingView(sysId, ctx.actions, closeMe),
              Observer.empty[Unit],
              clickCloses = true,
              cls := "system-add-ping-dialog"
            )
          )
        )
      ),
      mods = Nil
    ),
    Array(
      Column[IntelSystemPing, IntelSystemPingTarget](
        name = "Target",
        nameEl = i(cls := "ti", cls := "ti-focus", title := "Target"),
        tpe = ColumnType.Text,
        select = _.pingTarget,
        view = (_, pt) =>
          pt match
            case IntelSystemPingTarget.User => i(cls := "ti", cls := "ti-user-filled", title := "User")
            case IntelSystemPingTarget.Map  => i(cls := "ti", cls := "ti-map", title := "Map")
        ,
        sortable = Some(Ordering.by:
          case IntelSystemPingTarget.Map  => 0
          case IntelSystemPingTarget.User => 1)
      ),
      Column[IntelSystemPing, Option[String]](
        name = "Note",
        nameEl = i(cls := "ti", cls := "ti-note", title := "Note"),
        tpe = ColumnType.Text,
        select = _.pingNote,
        view = (_, t) => span(t.getOrElse("")),
        sortable = None
      ),
      Column[IntelSystemPing, CharacterId](
        name = "CreatedBy",
        nameEl = i(cls := "ti", cls := "ti-user", title := "Created By"),
        tpe = ColumnType.Number,
        select = _.createdByCharacterId,
        view = (_, c) => ESI.characterImage(c, ""), // TODO resolve character name
        sortable = Some(Ordering.fromLessThan(_ < _))
      ),
      Column[IntelSystemPing, Instant](
        name = "CreatedAt",
        nameEl = i(cls := "ti", cls := "ti-clock-record", title := "Created"),
        tpe = ColumnType.DateTime,
        select = _.createdAt,
        view = (_, i) => span(IntelPingView.DateFormatter.format(i), title := i.toString),
        sortable = Some(Ordering[Instant])
      ),
      Column[IntelSystemPing, Unit](
        name = "Delete",
        nameEl = i(cls := "ti", cls := "ti-trash", title := "Delete"),
        tpe = ColumnType.Text,
        select = _ => (),
        view = (n, _) =>
          button(
            tpe := "button",
            i(cls := "ti", cls := "ti-trash", title := "Delete Ping"),
            disabled <-- ctx.roleController.canRemoveIntelPing.invert,
            onClick.mapToUnit.compose(
              _.sampleCollectSome(systemId).map(MapAction.RemoveIntelPing(_, n.id))
            ) --> ctx.actions
          ),
        sortable = None
      )
    ),
    _.id.toString,
    selected
  )

  override def view: Element = sectionTag(
    idAttr := "intel-pings-view",
    cls    := "intel-pings-view",
    cls    := "bottom-column-view",
    h3(cls := "section-title", "Connected Pings"),
    pingGrid.view
  )

object IntelPingView:
  private val DateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd").withZone(ZoneId.of("UTC"))

private given DropdownItem[IntelSystemPingTarget]:
  def key(pt: IntelSystemPingTarget): String           = pt.toString
  def group(pt: IntelSystemPingTarget): Option[String] = None
  def view(pt: IntelSystemPingTarget): Element         = span(dataAttr("ping-target") := pt.toString, pt.toString)

private def addIntelPingView(
    systemId: SystemId,
    actions: WriteBus[MapAction],
    closeMe: Observer[Unit]
) =
  val noteVar   = Var("")
  val targetVar = Var(IntelSystemPingTarget.User)
  val targetSelect = OptionDropdown[IntelSystemPingTarget](
    ArraySeq.unsafeWrapArray(IntelSystemPingTarget.values),
    targetVar,
    mods = modSeq(idAttr := "intel-ping-target")
  )

  div(
    cls := "system-add-intel-ping-view",
    h2(cls := "dialog-header", s"Add ping"),
    form(
      fieldSet(
        p(
          label(forId := "intel-ping-target", "Target"),
          targetSelect.view
        ),
        p(
          label(forId := "add-intel-ping-note", "Note"),
          input(
            idAttr := "add-intel-ping-note",
            controlled(
              value <-- noteVar,
              onInput.mapToValue --> noteVar
            )
          )
        )
      ),
      button(
        tpe := "button",
        cls := "add-ping",
        i(cls := "ti", cls := "ti-plus"),
        "add ping",
        onClick.preventDefault.compose(
          _.mapToUnit
            .withCurrentValueOf(targetVar, noteVar)
            .map((target, note) => MapAction.AddIntelPing(systemId, target, Option.when(note.nonEmpty)(note)))
        )
          --> { action =>
            actions.onNext(action)
            closeMe.onNext(None)
          }
      )
    )
  )
