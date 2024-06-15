package controltower.page

import com.raquo.laminar.api.L.*
import controltower.*
import controltower.backend.{ControlTowerBackend, ESI}
import controltower.component.*
import controltower.dialog.EditMapView
import org.scalajs.dom
import org.updraft0.controltower.protocol.*
import sttp.model.Uri

import scala.concurrent.ExecutionContext.Implicits.global

object LandingPage:
  val LoginWithEveImageUrlLarge = "https://web.ccpgamescdn.com/eveonlineassets/developers/eve-sso-login-black-large.png"

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
      renderLogin(ct.loginUrl),
      a(Routes.navigateTo(Page.MapEditor), "Map Editor")
    )

  private def renderCharacterMapSelection(userInfo: UserInfo)(using ControlTowerBackend) =
    val charMaps = userInfo.maps.groupBy(_.characterId)

    div(
      idAttr := "map-selection",
      cls    := "map-selection",
      userInfo.characters
        .sortBy(_.name)
        .map(char => char -> charMaps.getOrElse(char.characterId, Nil).sortBy(_.mapName))
        .map(renderCharacterMaps)
    )

  private def renderCharacterMaps(char: UserCharacter, maps: List[UserCharacterMap])(using ControlTowerBackend) =
    maps match
      case Nil =>
        table(
          cls := "character-maps",
          List(
            tr(
              td(cls := "character", renderCharacter(char)),
              td(cls := "map-link", newMapLink(char))
            )
          )
        )
      case map :: xs =>
        table(
          cls := "character-maps",
          tr(
            td(cls := "character", rowSpan := maps.length + 1, renderCharacter(char)),
            td(cls := "map-link", mapLink(char, map))
          ) :: xs.map(map => tr(td(cls := "map-link", mapLink(char, map)))) :::
            List(tr(td(cls := "map-link", newMapLink(char))))
        )

  private def renderLogin(login: Uri) =
    div(
      a(
        href := login.toString,
        img(src := LoginWithEveImageUrlLarge, alt := "Login to EVE Online")
      )
    )

  private def renderCharacter(char: UserCharacter)(using ct: ControlTowerBackend) =
    import com.raquo.laminar.api.features.unitArrows
    nodeSeq(
      ESI.characterImage(char.characterId, char.name),
      div(
        button(
          tpe := "button",
          cls := "logout",
          cls := "ti",
          cls := "ti-logout",
          onClick.preventDefault --> Modal.showConfirmation(
            s"Logout ${char.name}?",
            s"Are you sure you want to logout ${char.name} from the map",
            onOk = Observer(_ =>
              ct.logoutUserCharacter(char.characterId)
                .onComplete(_ => Routes.router.pushState(Page.Landing))
            ),
            isDestructive = true
          )
        ),
        mark(char.name)
      )
    )

  private def newMapLink(char: UserCharacter)(using ct: ControlTowerBackend) =
    button(
      cls := "new-map",
      tpe("button"),
      "New map",
      onClick.preventDefault --> { _ =>
        Modal.show(
          (closeMe, _) => EditMapView(char, closeMe, None)(using ct),
          onCloseObs = Observer.empty[Unit],
          clickCloses = false,
          idAttr := "edit-map-dialog"
        )
      }
    )

  private def mapLink(char: UserCharacter, map: UserCharacterMap) =
    val page = Page.Map(map.mapName, char.name)
    a(
      Routes.navigateTo(page),
      i(cls := "ti", cls := "ti-map"),
      " ",
      map.mapName
    )
