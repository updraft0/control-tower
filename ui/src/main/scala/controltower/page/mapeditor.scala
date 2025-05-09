package controltower.page

import com.raquo.laminar.api.L.*
import controltower.backend.ControlTowerBackend
import org.updraft0.controltower.protocol.*
import controltower.component.Modal
import controltower.dialog.EditMapView

import scala.concurrent.ExecutionContext.Implicits.global

object MapEditorPage:

  def renderPage(using ct: ControlTowerBackend) =
    div(
      idAttr := "map-editor",
      h2("Map Editor"),
      child <-- mapEditButtons
    )

  private def mapEditButtons(using ct: ControlTowerBackend) =
    Signal
      .fromFuture(ct.getUserInfo())
      .map:
        case None                  => commentNode()
        case Some(Left(msg))       => div(cls := "error", msg)
        case Some(Right(userInfo)) => mapEditButtonsFromInfo(userInfo)

  private def mapEditButtonsFromInfo(userInfo: UserInfo)(using ControlTowerBackend) =
    val adminMaps      = userInfo.maps.filter(_.mapRole == MapRole.Admin)
    val charactersById = userInfo.characters.map(c => c.characterId -> c).toMap
    div(
      cls := "map-links",
      adminMaps.map(m => editButtonFor(charactersById(m.characterId), m))
    )

  private def editButtonFor(char: UserCharacter, map: UserCharacterMap)(using ct: ControlTowerBackend) =
    button(
      tpe := "button",
      cls := "edit",
      map.mapName,
      onClick.preventDefault --> { _ =>
        ct.getMap(map.mapId)
          .onComplete:
            case scala.util.Success(Right(mapInfoWithPermissions)) =>
              Modal.show(
                (closeMe, _) => EditMapView(char, closeMe, Some(mapInfoWithPermissions)),
                Observer.empty[Unit],
                false,
                idAttr := "edit-map-dialog"
              )
            case other => org.scalajs.dom.console.error(s"Failed to get map permissions: $other")
      }
    )
