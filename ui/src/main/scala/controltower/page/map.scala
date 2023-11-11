package controltower.page

import com.raquo.laminar.api.L.{given, *}
import controltower.Page
import controltower.backend.ControlTowerBackend

object MapPage:

  def renderPage(map: Page.Map)(using ct: ControlTowerBackend) =
    div("hello map")
