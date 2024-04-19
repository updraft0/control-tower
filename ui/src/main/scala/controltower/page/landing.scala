package controltower.page

import com.raquo.laminar.api.L.*
import controltower.*
import controltower.backend.{ControlTowerBackend, ESI}
import controltower.component.*
import controltower.ui.*
import org.scalajs.dom
import org.updraft0.controltower.protocol.*
import sttp.model.Uri

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object LandingPage:
  def renderPage(using ct: ControlTowerBackend) =
    val userInfo = Var[Option[UserInfo]](None)

    ct.getUserInfo().foreach {
      case Left(msg) =>
        dom.console.debug(s"error loading user info: $msg")
        userInfo.set(None)
      case Right(value) => userInfo.set(Some(value))
    }
    div(
      cls := "landing-page",
      "Control Tower Landing page",
      child <-- userInfo.signal.map(
        _.map(renderCharacterMapSelection).getOrElse(emptyNode)
      ),
      renderLogin(ct.loginUrl)
    )

  private def renderCharacterMapSelection(userInfo: UserInfo)(using ControlTowerBackend) =
    val charMaps = userInfo.maps.groupBy(_.characterId)

    table(
      cls := "map-selection",
      thead(
        tr(
          th("Character"),
          th("Map")
        )
      ),
      tbody(
        userInfo.characters
          .sortBy(_.name)
          .map(char => char -> charMaps.getOrElse(char.characterId, Nil).sortBy(_.mapName))
          .map(renderCharacterMaps)
      )
    )

  private def renderCharacterMaps(char: UserCharacter, maps: List[UserCharacterMap])(using ControlTowerBackend) =
    maps match
      case Nil =>
        List(
          tr(
            td(cls := "character", renderCharacter(char)),
            td(cls := "map-link", newMapLink(char))
          )
        )
      case map :: xs =>
        tr(
          td(cls := "character", rowSpan := maps.length + 1, renderCharacter(char)),
          td(cls := "map-link", mapLink(map))
        ) :: xs.map(map => tr(td(cls := "map-link", mapLink(map)))) :::
          List(tr(td(cls := "map-link", newMapLink(char))))

  private def renderLogin(login: Uri) =
    div(
      a(
        href := login.toString,
        img(
          src := "https://web.ccpgamescdn.com/eveonlineassets/developers/eve-sso-login-black-large.png",
          alt := "Login to EVE Online"
        )
      )
    )

  private def renderCharacter(char: UserCharacter) =
    nodeSeq(
      ESI.characterImage(char.characterId, char.name),
      div(
        button(tpe := "button", cls := "logout", cls := "ti", cls := "ti-logout"), // FIXME implement
        mark(char.name)
      )
    )

  private def newMapLink(char: UserCharacter)(using ct: ControlTowerBackend) =
    button(
      cls := "new-map",
      tpe("button"),
      "New map",
      onClick.preventDefault --> { _ =>
        Modal.show((closeMe, owner) => NewMapDialogView(char, closeMe)(using ct, owner), clickCloses = false)
      }
    )

  private def mapLink(map: UserCharacterMap) =
    val page = Page.Map(map.characterId, map.mapName)
    a(
      Routes.navigateTo(page),
      i(cls := "ti", cls := "ti-map"),
      " ",
      map.mapName
    )

object NewMapDialogView:

  def apply(char: UserCharacter, closeMe: Observer[Unit])(using ct: ControlTowerBackend, owner: Owner): HtmlElement =
    val nameVar     = Var("")
    val defaultPerm = MapPolicyMember(0L, PolicyMemberType.Character, isDeny = false, MapRole.Viewer)

    val validationError = Var(Option.empty[String])
    val permissionsVar  = FakeVectorVar[MapPolicyMember]()
    permissionsVar.append(MapPolicyMember(char.characterId, PolicyMemberType.Character, isDeny = false, MapRole.Admin))

    div(
      cls("modal-content"),
      cls("new-map-dialog"),
      button(
        "×",
        autoFocus := true,
        cls       := "close",
        onClick.mapToUnit --> closeMe
      ),
      h2("New Map"),
      div(
        display <-- validationError.signal.map(ve => Option.when(ve.isDefined)("").getOrElse("none")),
        child.text <-- validationError.signal.map(_.getOrElse(""))
      ),
      div(
        label("Map name"),
        input(placeholder := "New Map", onInput.mapToValue --> nameVar)
      ),
      table(
        children <-- permissionsVar.split(renderPolicyMember),
        tr(
          td(
            button(
              typ("button"),
              "➕ permission",
              onClick.mapToUnit --> (() => permissionsVar.append(defaultPerm))
            )
          )
        ),
        button(
          typ("button"),
          "Create",
          onClick.preventDefault --> Observer(_ =>
            val name  = nameVar.now()
            val perms = permissionsVar.now().filterNot(_._2.memberId == 0).map(_._2)

            val newMap = NewMap(name, perms.toArray, MapDisplayType.Manual)
            dom.console.debug(s"creating new map: ${newMap}")
            ct.createMap(newMap).onComplete {
              case Failure(ex)          => dom.console.error("failed to call newMap", ex) // FIXME this isn't called??
              case Success(Left(error)) => validationError.set(Some(error))
              case Success(Right(mapInfo)) => closeMe.onNext(())
            }
          )
        )
      )
    )

  private def renderPolicyMember(policyVar: FakeVar[MapPolicyMember]) =
    // TODO add validation, autocomplete search for characters etc., removing a row
    tr(
      td(
        input(
          controlled(
            value <-- policyVar.signal.map(_.memberId.toString),
            onInput.mapToValue --> policyVar.onUpdateZoom[String]((m, s) => m.copy(memberId = s.toLong))
          )
        )
      ),
      td(
        select(
          controlled(
            value <-- policyVar.signal.map(_.memberType.toString),
            onChange.mapToValue --> policyVar.onUpdateZoom[String]((m, s) =>
              m.copy(memberType = PolicyMemberType.valueOf(s))
            )
          ),
          // FIXME quick hack to hardcode, cannot use varargs as they cannot be spliced in currently
          option(value := PolicyMemberType.Character.toString, PolicyMemberType.Character.toString),
          option(value := PolicyMemberType.Alliance.toString, PolicyMemberType.Alliance.toString),
          option(value := PolicyMemberType.Corporation.toString, PolicyMemberType.Corporation.toString)
        )
      ),
      td(
        select(
          controlled(
            value <-- policyVar.signal.map(_.role.toString),
            onChange.mapToValue --> policyVar.onUpdateZoom[String]((m, s) => m.copy(role = MapRole.valueOf(s)))
          ),
          // FIXME see PolicyMemberType above
          option(value := MapRole.Admin.toString, MapRole.Admin.toString),
          option(value := MapRole.Viewer.toString, MapRole.Viewer.toString),
          option(value := MapRole.Editor.toString, MapRole.Editor.toString)
        )
      ),
      td(
        select(
          controlled(
            value <-- policyVar.signal.map(v => if v.isDeny then "deny" else "allow"),
            onChange.mapToValue --> policyVar.onUpdateZoom[String]((m, s) => m.copy(isDeny = !(s == "allow")))
          ),
          option(value := "allow", "allow"),
          option(value := "deny", "deny")
        )
      )
    )
