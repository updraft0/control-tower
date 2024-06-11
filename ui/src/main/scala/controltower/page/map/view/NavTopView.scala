package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.{Page, Routes}
import controltower.backend.{ControlTowerBackend, ESI}
import controltower.ui.ViewController
import controltower.component.Modal
import controltower.dialog.EditMapView
import org.updraft0.controltower.constant
import org.updraft0.controltower.protocol.*

import java.time.{Instant, LocalDate, LocalTime}
import scala.concurrent.ExecutionContext.Implicits.global

class NavTopView(
    mapName: String,
    mapMeta: Signal[MapMessage.MapMeta],
    locations: Signal[Map[constant.SystemId, Array[CharacterLocation]]],
    time: Signal[Instant],
    isConnected: Signal[Boolean],
    ct: ControlTowerBackend
) extends ViewController:

  override def view = div(
    idAttr := "nav-top-view",
    cls    := "nav-top-view",
    cls    := "left-sidebar-view",
    navButton("go-home", cls := "ti", cls := "ti-home-filled", Routes.navigateTo(Page.Landing)),
    editMapButton(using ct),
    userInfo(mapName, mapMeta.map(_.character), mapMeta.map(_.role)),
    locationStatus(locations),
    timeStatus(time),
    connectionStatus(isConnected)
  )

  private def editMapButton(using ControlTowerBackend) =
    navButton(
      "edit-map",
      cls := "ti",
      cls := "ti-table-options",
      display <-- mapMeta.map(_.role).map {
        case MapRole.Admin => ""
        case _             => "none"
      },
      disabled <-- isConnected.map(!_),
      onClick.preventDefault.mapToUnit.compose(_.withCurrentValueOf(mapMeta)) --> { (meta) =>
        ct.getMap(meta.info.id)
          .onComplete:
            case scala.util.Success(Right(mapInfoWithPermissions)) =>
              Modal.show(
                (closeMe, _) => EditMapView(meta.character, closeMe, Some(mapInfoWithPermissions)),
                clickCloses = false,
                idAttr := "edit-map-dialog"
              )
            case other => org.scalajs.dom.console.error(s"Failed to get map permissions: $other")
      }
    )

private def navButton(id: String, mods: Modifier[Button]*) =
  button(idAttr := id, tpe := "button", cls := "nav-button", mods)

private def userInfo(mapName: String, char: Signal[UserCharacter], mapRole: Signal[MapRole]) =
  nodeSeq(
    ESI.characterImageS(char.map(c => c.characterId -> c.name), size = CharacterImageSize),
    span(cls := "map-role", child.text <-- mapRole.map(r => s"${mapName}/${r.toString}"))
  )

private def locationStatus(locations: Signal[Map[constant.SystemId, Array[CharacterLocation]]]) =
  span(cls := "location-status", cls := "right-block", child <-- locations.map(_.valuesIterator.map(_.length).sum))

private def timeStatus(time: Signal[Instant]) =
  span(
    cls := "time-status",
    cls := "right-block",
    child <-- time.map(i => asLocal(i)._2).map(t => f"${t.getHour}%02d:${t.getMinute}%02d")
  )

private def connectionStatus(isConnected: Signal[Boolean]) =
  span(
    cls := "connection-status",
    cls := "right-block",
    cls := "ti",
    cls <-- isConnected.map(c => if (c) "ti-link" else "ti-unlink")
  )

// TODO move this somewhere else
private val SecondsInDay              = 24 * 60 * 60
private val SecondsFromZeroToEpoch    = ((146097L * 5L) - (30L * 365L + 7L)) * SecondsInDay
private val SecondsInTenThousandYears = 146097L * SecondsInDay * 25L

def asLocal(i: Instant): (LocalDate, LocalTime) =
  def tenThousandPartsAndRemainder: (Long, Long) =
    val seconds = i.getEpochSecond
    if (seconds < -SecondsFromZeroToEpoch)
      val quot = seconds / SecondsInTenThousandYears
      val rem  = seconds % SecondsInTenThousandYears
      (quot, rem)
    else
      val quot = Math.floorDiv(seconds, SecondsInTenThousandYears)
      val rem  = Math.floorMod(seconds, SecondsInTenThousandYears)
      (quot, rem)

  def dateTime(epochSecond: Long): (LocalDate, LocalTime) =
    val epochDay     = Math.floorDiv(epochSecond, SecondsInDay)
    val secondsOfDay = Math.floorMod(epochSecond, SecondsInDay).toInt
    (LocalDate.ofEpochDay(epochDay), LocalTime.ofSecondOfDay(secondsOfDay).withNano(i.getNano))

  val (hi, lo)    = tenThousandPartsAndRemainder
  val epochSecond = lo
  dateTime(epochSecond)
