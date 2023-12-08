package controltower.page

import com.raquo.laminar.api.L.*
import controltower.Page
import controltower.page.map.MapViewController
import controltower.backend.ControlTowerBackend
import scala.concurrent.ExecutionContext.Implicits.global

object MapPage:

  def renderPage(map: Page.Map)(using ct: ControlTowerBackend) =
    val controllerF = MapViewController(map)
    div(
      idAttr := "map-page",
      child.maybe <-- Signal.fromFuture(controllerF.map(_.view))
    )
