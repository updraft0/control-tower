package controltower.page.map

import com.raquo.laminar.api.L.*
import controltower.backend.{ESI, ThirdParty}
import controltower.ui.FakeVarM
import org.updraft0.controltower.constant.{SpaceType, WormholeEffects}
import org.updraft0.controltower.protocol.*

class SystemSignatureView(staticData: SystemStaticData, selected: Signal[Option[MapSystemSnapshot]]):

  def view =
    child <-- selected.map {
      case Some(mss) => sigView(mss)
      case None      => emptyNode
    }

private inline def sigView(mss: MapSystemSnapshot) =
  table(
    cls := "system-signature-view",
    cls := "left-sidebar-view",
    thead(
      tr(
        th("id"),
        th("group"),
        th("type"),
        th("test")
      )
    )
  )
