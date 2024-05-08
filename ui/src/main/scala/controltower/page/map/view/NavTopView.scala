package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.{Page, Routes}
import controltower.backend.ESI
import controltower.ui.ViewController
import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.protocol.*

import java.time.{Instant, LocalDate, LocalTime}

class NavTopView(
    mapName: String,
    characterId: Signal[CharacterId],
    time: Signal[Instant],
    mapRole: Signal[MapRole],
    isConnected: Signal[Boolean]
) extends ViewController:

  override def view = div(
    idAttr := "nav-top-view",
    cls    := "nav-top-view",
    cls    := "left-sidebar-view",
    navButton("go-home", cls := "ti", cls := "ti-home-filled", Routes.navigateTo(Page.Landing)),
    userInfo(mapName, characterId, mapRole),
    timeStatus(time),
    connectionStatus(isConnected)
  )

private def navButton(id: String, mods: Modifier[Button]*) =
  button(idAttr := id, tpe := "button", cls := "nav-button", mods)

private def userInfo(mapName: String, characterId: Signal[CharacterId], mapRole: Signal[MapRole]) =
  nodeSeq(
    ESI.characterImageS(characterId.map(id => id -> "?"), size = CharacterImageSize),
    span(cls := "map-role", child.text <-- mapRole.map(r => s"${mapName}/${r.toString}"))
  )

private def timeStatus(time: Signal[Instant]) =
  span(cls := "time-status", child <-- time.map(i => asLocal(i)._2).map(t => f"${t.getHour}%02d:${t.getMinute}%02d"))

private def connectionStatus(isConnected: Signal[Boolean]) =
  i(cls := "ti", cls := "connection-status", cls <-- isConnected.map(c => if (c) "ti-link" else "ti-unlink"))

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
