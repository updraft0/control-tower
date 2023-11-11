package controltower

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import org.scalajs.dom.document

object App:

  def main(args: Array[String]): Unit =
    val _ = documentEvents(_.onDomContentLoaded).foreach { _ =>
      val container = document.getElementById("app")
      val _ =
        render(
          container,
          // TODO: insert page chrome for navigation etc.
          div(child <-- Routes.view)
        )
    }(unsafeWindowOwner)
