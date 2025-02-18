package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.backend.ESI
import controltower.ui.*
import controltower.component.*
import controltower.page.map.MapAction
import org.updraft0.controltower.protocol.IntelSystemNote
import org.updraft0.controltower.constant.{CharacterId, SystemId}

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter

final class IntelNoteView(systemId: Signal[Option[SystemId]], selected: Signal[Array[IntelSystemNote]])(using
    ctx: MapViewContext
) extends ViewController:

  private val noteGrid = MiniGrid[IntelSystemNote](
    GridConfig(
      pageSize = Some(10),
      initialSort = Some("CreatedAt" -> SortDirection.Desc),
      overrideSort = Some("Pinned" -> SortDirection.Desc),
      toolbarMods = nodeSeq(
        button(
          tpe := "button",
          i(cls := "ti", cls := "ti-plus", title := "Add note"),
          disabled <-- ctx.roleController.canAddIntelNote.invert,
          onClick.compose(_.sampleCollectSome(systemId)) --> (sysId =>
            Modal.show(
              (closeMe, owner) => addEditIntelNoteView(sysId, None, ctx.actions, closeMe),
              Observer.empty[Unit],
              true,
              cls := "system-add-note-dialog"
            )
          )
        )
      ),
      mods = Nil
    ),
    Array(
      Column[IntelSystemNote, Boolean](
        name = "Pinned",
        nameEl = i(cls := "ti", cls := "ti-pin", title := "Pinned"),
        tpe = ColumnType.Number,
        select = _.isPinned,
        view = (sn, is) =>
          val icon = if (is) "ti-pinned-filled" else "ti-pin"
          button(
            i(cls := "ti", cls := icon, title := "Pin/Unpin"),
            onClick.mapToUnit.compose(
              _.sampleCollectSome(systemId).map(sId =>
                MapAction.UpdateIntelSystemNote(sId, sn.id, sn.note, !sn.isPinned)
              )
            ) --> ctx.actions,
            disabled <-- ctx.roleController.canPinIntelNote.invert
          )
        ,
        sortable = Some(Ordering[Boolean])
      ),
      Column[IntelSystemNote, CharacterId](
        name = "CreatedBy",
        nameEl = i(cls := "ti", cls := "ti-user", title := "Created By"),
        tpe = ColumnType.Number,
        select = _.createdByCharacterId,
        view = (_, c) => ESI.characterImage(c, ""), // TODO resolve character name
        sortable = Some(Ordering.fromLessThan(_ < _))
      ),
      Column[IntelSystemNote, Instant](
        name = "CreatedAt",
        nameEl = i(cls := "ti", cls := "ti-clock-record", title := "Created"),
        tpe = ColumnType.DateTime,
        select = _.createdAt,
        view = (_, i) => span(IntelNoteView.DateFormatter.format(i), title := i.toString),
        sortable = Some(Ordering[Instant])
      ),
      Column[IntelSystemNote, String](
        name = "Note",
        nameEl = i(cls := "ti", cls := "ti-note", title := "Note"),
        tpe = ColumnType.Text,
        select = _.note,
        view = (sn, t) => renderNote(t, sn.isPinned),
        sortable = None
      ),
      Column[IntelSystemNote, Unit](
        name = "Edit",
        nameEl = i(cls := "ti", cls := "ti-pencil", title := "Edit"),
        tpe = ColumnType.Text,
        select = _ => (),
        view = (sn, _) =>
          button(
            tpe := "button",
            i(cls := "ti", cls := "ti-pencil", title := "Edit Note"),
            disabled <-- ctx.roleController.canEditIntelNote.invert,
            onClick.compose(_.sampleCollectSome(systemId)) --> (sysId =>
              Modal.show(
                (closeMe, owner) => addEditIntelNoteView(sysId, Some(sn), ctx.actions, closeMe),
                Observer.empty[Unit],
                true,
                cls := "system-edit-note-dialog"
              )
            )
          ),
        sortable = None
      ),
      Column[IntelSystemNote, Unit](
        name = "Delete",
        nameEl = i(cls := "ti", cls := "ti-trash", title := "Delete"),
        tpe = ColumnType.Text,
        select = _ => (),
        view = (n, _) =>
          button(
            tpe := "button",
            i(cls := "ti", cls := "ti-trash", title := "Delete Note"),
            disabled <-- ctx.roleController.canRemoveIntelNote.invert,
            onClick.mapToUnit.compose(
              _.sampleCollectSome(systemId).map(MapAction.RemoveIntelNote(_, n.id))
            ) --> ctx.actions
          ),
        sortable = None
      )
    ),
    _.id.toString,
    selected,
    rowMods = sn => modSeq(cls("pinned") := sn.isPinned)
  )

  override def view: Element = sectionTag(
    idAttr := "intel-notes-view",
    cls    := "intel-notes-view",
    cls    := "bottom-column-view",
    h3(cls := "section-title", "Notes"),
    noteGrid.view
  )

object IntelNoteView:
  private val DateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd").withZone(ZoneId.of("UTC"))

private def renderNote(text: String, isPinned: Boolean) =
  div(
    text.split('\n').map(t => p(cls("pinned") := isPinned, t))*
  )

private def addEditIntelNoteView(
    systemId: SystemId,
    existing: Option[IntelSystemNote],
    actions: WriteBus[MapAction],
    closeMe: Observer[Unit]
) =
  val noteVar  = Var(existing.map(_.note).getOrElse(""))
  val isPinned = Var(existing.exists(_.isPinned))
  div(
    cls := "system-add-intel-note-view",
    h2(cls := "dialog-header", s"${existing.map(_ => "Edit").getOrElse("Add")} note"),
    form(
      fieldSet(
        p(
          label(forId := "add-intel-note-content", "Note"),
          textArea(
            idAttr    := "add-intel-note-content",
            autoFocus := true,
            controlled(
              value <-- noteVar,
              onInput.mapToValue --> noteVar
            )
          )
        ),
        p(
          label(forId := "is-note-pinned", "Pinned"),
          input(
            idAttr := "is-note-pinned",
            tpe    := "checkbox",
            controlled(
              checked <-- isPinned,
              onInput.mapToChecked --> isPinned
            )
          )
        )
      ),
      button(
        tpe := "button",
        cls := "add-note",
        i(cls := "ti", cls := "ti-plus"),
        s"${existing.map(_ => "edit").getOrElse("add")} note",
        onClick.preventDefault.compose(
          _.mapToUnit
            .withCurrentValueOf(noteVar, isPinned)
            .map((note, isPinned) =>
              existing
                .map(esn => MapAction.UpdateIntelSystemNote(systemId, esn.id, note, isPinned))
                .getOrElse(MapAction.AddIntelSystemNote(systemId, note, isPinned))
            )
        )
          --> { action =>
            actions.onNext(action)
            closeMe.onNext(None)
          }
      )
    )
  )
