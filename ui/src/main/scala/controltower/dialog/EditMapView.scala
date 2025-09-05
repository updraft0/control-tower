package controltower.dialog

import com.raquo.laminar.api.L.*
import controltower.*
import controltower.backend.ControlTowerBackend
import controltower.ui.*
import org.scalajs.dom
import org.updraft0.controltower.protocol.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object EditMapView:
  private val DefaultPerm = MapPolicyMember(0L, PolicyMemberType.Character, isDeny = false, MapRole.Viewer)

  // TODO:
  // * dialog name when editing
  // * endpoint differs now when saving

  def apply(
      char: UserCharacter,
      closeMe: Observer[Unit],
      existing: Option[MapInfoWithPermissions] = None
  )(using ct: ControlTowerBackend): HtmlElement =
    val name            = Var(existing.map(_.map.name).getOrElse(""))
    val validationError = Var(Option.empty[String])
    val permissionsAll  = ArrayRenderedVar(
      renderPolicyMember,
      existing
        .map(_.policyMembers)
        .getOrElse(Array(MapPolicyMember(char.characterId, PolicyMemberType.Character, isDeny = false, MapRole.Admin)))
    )

    div(
      cls("modal-content"),
      div(
        cls := "dialog-header",
        h2(existing.map(m => s"Edit '${m.map.name}'").getOrElse("New Map")),
        button(
          tpe := "button",
          cls := "ti",
          cls := "ti-x",
          cls := "close",
          onClick.mapToUnit --> closeMe
        )
      ),
      div(
        cls := "error",
        display <-- validationError.signal.map(ve => Option.when(ve.isDefined)("").getOrElse("none")),
        child.text <-- validationError.signal.map(_.getOrElse(""))
      ),
      div(
        label("Map name", forId := "map-name"),
        input(
          idAttr := "map-name",
          controlled(
            value <-- name.signal,
            onInput.mapToValue.filterNot(_.isBlank) --> name
          ),
          placeholder := "New Map"
        )
      ),
      table(
        thead(
          tr(
            th("id"),
            th("entity"),
            th("role"),
            th("allow"),
            th()
          )
        ),
        tbody(
          children.command <-- permissionsAll.elements
        ),
        tbody(
          tr(
            td(
              button(
                typ("button"),
                cls := "add-row",
                "➕ permission",
                onClick.mapToUnit --> Observer(_ => permissionsAll.append(DefaultPerm))
              )
            )
          ),
          button(
            typ("button"),
            cls := "update",
            existing.map(_ => "Update").getOrElse("Create"),
            onClick.preventDefault.mapToUnit.compose(_.withCurrentValueOf(name)) --> Observer[String]: name =>
              val perms = permissionsAll.itemsNow.filterNot(_.memberId == 0).toArray

              existing match
                case None =>
                  val newMap = NewMap(name, perms, MapDisplayType.Manual)
                  ct.createMap(newMap)
                    .onComplete:
                      case Failure(ex) => dom.console.error("failed to call newMap", ex) // FIXME this isn't called??
                      case Success(Left(error)) => validationError.set(Some(error))
                      case Success(Right(_))    => closeMe.onNext(())
                case Some(prev) =>
                  val updated = MapInfoWithPermissions(prev.map.copy(name = name), perms)
                  ct.updateMap(prev.map.id, updated)
                    .onComplete:
                      case Failure(ex)          => dom.console.error("failed to update map", ex)
                      case Success(Left(error)) => validationError.set(Some(error))
                      case Success(Right(_))    => closeMe.onNext(())
          )
        )
      ),
      permissionsAll.live
    )

  private def renderPolicyMember(
      policyMember: StrictSignal[MapPolicyMember],
      update: Observer[Option[MapPolicyMember]]
  ) =
    // TODO add validation, autocomplete search for characters etc., removing a row
    tr(
      cls := "map-policy-member",
      td(
        input(
          cls := "member-id",
          controlled(
            value <-- policyMember.map(_.memberId.toString),
            onInput.mapToValue --> Observer[String]: s =>
              s.toLongOption.foreach: id =>
                update.onNext(Some(policyMember.now().copy(memberId = id)))
          )
        )
      ),
      td(
        // TODO refactor out the select to be generic
        select(
          cls := "member-type",
          controlled(
            value <-- policyMember.map(_.memberType.toString),
            onChange.mapToValue.map(PolicyMemberType.valueOf) --> (mt =>
              update.onNext(Some(policyMember.now().copy(memberType = mt)))
            )
          ),
          PolicyMemberType.values.map(pmt => option(value := pmt.toString, pmt.toString))
        )
      ),
      td(
        select(
          cls := "map-role",
          controlled(
            value <-- policyMember.map(_.role.toString),
            onChange.mapToValue.map(MapRole.valueOf) --> (r => update.onNext(Some(policyMember.now().copy(role = r))))
          ),
          MapRole.values.map(r => option(value := r.toString, r.toString))
        )
      ),
      td(
        input(
          typ := "checkbox",
          controlled(
            checked <-- policyMember.map(!_.isDeny),
            onInput.mapToChecked --> (v => update.onNext(Some(policyMember.now().copy(isDeny = !v))))
          )
        )
      ),
      td(
        button(
          tpe := "button",
          cls := "remove-row",
          cls := "ti",
          cls := "ti-trash",
          onClick.preventDefault.mapToUnit --> (_ => update.onNext(None))
        )
      )
    )
