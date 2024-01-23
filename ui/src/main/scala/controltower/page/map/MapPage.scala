package controltower.page.map

import com.raquo.laminar.api.L.*
import controltower.Page
import controltower.backend.ControlTowerBackend

import scala.concurrent.ExecutionContext.Implicits.global
import controltower.page.map.view.MapView

import java.time.Clock

object MapPage:

  def renderPage(map: Page.Map)(using ct: ControlTowerBackend, clock: Clock) =
    val viewF = MapView(map, EventStream.periodic(5_000).map(_ => clock.instant).toSignal(clock.instant, false))
    div(
      idAttr := "map-page",
      child.maybe <-- Signal.fromFuture(viewF.map(_.view))
    )
