package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.ui.*
import org.updraft0.controltower.protocol.*

import scala.annotation.unused

class SystemSignatureView(@unused staticData: SystemStaticData, selected: Signal[Option[MapSystemSnapshot]])
    extends ViewController:

  override def view =
    table(
      cls := "system-signature-view",
      cls := "left-sidebar-view",
      hideIfEmptyOpt(selected),
      children <-- selected.map {
        case Some(selected) => sigView(selected)
        case None           => nodeSeq()
      }
    )

private inline def sigView(mss: MapSystemSnapshot) =
  nodeSeq(
    thead(
      tr(
        th("id"),
        th("group"),
        th("type"),
        th("test")
      )
    )
  )
