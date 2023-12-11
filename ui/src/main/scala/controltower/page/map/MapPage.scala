package controltower.page.map

import com.raquo.laminar.api.L.*
import controltower.Page
import controltower.backend.ControlTowerBackend
import scala.concurrent.ExecutionContext.Implicits.global
import controltower.page.map.view.MapView

object MapPage:

  def renderPage(map: Page.Map)(using ct: ControlTowerBackend) =
    val viewF = MapView(map)
    div(
      idAttr := "map-page",
      child.maybe <-- Signal.fromFuture(viewF.map(_.view))
    )
