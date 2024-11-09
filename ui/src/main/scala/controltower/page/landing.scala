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
      topNav,
      child <-- userInfo.signal.map(
        _.map(renderCharacterMapSelection).getOrElse(emptyNode)
      ),
      renderLogin(ct.loginUrl)
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
    div(
      cls := "character-and-maps",
      renderCharacter(char),
      div(
        cls := "maps",
        nodeSeq(maps.map(map => div(cls := "map-link", mapLink(char, map)))*),
        div(cls := "map-link", newMapLink(char))
      )
    )

  private def renderLogin(login: Uri) =
    a(
      cls  := "login-to-eve",
      href := login.toString,
      img(src := LoginWithEveImageUrlLarge, alt := "Login to EVE Online")
    )

  private def renderCharacter(char: UserCharacter)(using ct: ControlTowerBackend) =
    div(
      cls                 := "character",
      cls("expired-auth") := !char.authTokenFresh,
      ESI.characterImage(char.characterId, char.name),
      div(
        cls := "character-name",
        if (!char.authTokenFresh) buttonRefreshLogin else emptyNode,
        buttonLogout(char),
        mark(cls := "name", char.name)
      )
    )

  private def buttonRefreshLogin(using ct: ControlTowerBackend) =
    a(
      cls  := "login",
      cls  := "ti",
      cls  := "ti-refresh-alert",
      href := ct.loginUrl.toString
    )

  private def buttonLogout(char: UserCharacter)(using ct: ControlTowerBackend) =
    import com.raquo.laminar.api.features.unitArrows
    button(
      tpe("button"),
      cls := "logout",
      cls := "ti",
      cls := "ti-logout",
      onClick.preventDefault --> Modal.showConfirmation(
        s"Logout ${char.name}?",
        s"Are you sure you want to logout '${char.name}' from the map",
        onOk = Observer(_ =>
          ct.logoutUserCharacter(char.characterId)
            .onComplete(_ => Routes.router.pushState(Page.Landing))
        ),
        isDestructive = true
      )
    )

  private def newMapLink(char: UserCharacter)(using ct: ControlTowerBackend) =
    button(
      cls := "new-map",
      tpe("button"),
      i(cls := "ti", cls := "ti-plus"),
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

  private def topNav =
    div(
      cls := "topnav",
      span("ControlTower"),
      button(cls := "navitem", cls := "ti", cls := "ti-map-cog", Routes.navigateTo(Page.MapEditor))
    )
