package controltower.page.map.view

import com.raquo.laminar.api.L.*

import controltower.ui.FakeVarM

case class Coord(x: Double, y: Double)

object Coord:
  val Hidden = Coord(-1, -1)
  val Origin = Coord(0, 0)

private val DefaultGridSnapPx = 10
private val MouseButtonLeft   = 0

/** Make a draggable content element with grid snapping
  * @param pos
  *   The bidirectional var with the position of the box on the parent element
  * @param canDrag
  *   Whether to allow dragging or not
  * @param f
  *   The function that draws the content of the element (isDragging, parent) => ()
  * @param gridSnap
  *   The grid snapping pixel size
  */
private def inDraggable(
    pos: FakeVarM[Long, Coord],
    canDrag: Signal[Boolean],
    f: (Signal[Boolean], HtmlElement) => Unit,
    gridSnap: Int = DefaultGridSnapPx
): HtmlElement =
  val downMouseCoord = Var(Option.empty[Coord])
  val isDragging     = downMouseCoord.signal.map(_.isDefined)
  val coordSig       = pos.signal
  div(
    cls <-- canDrag.map {
      case true  => "draggable-box"
      case false => "pinned-box"
    },
    inContext(self =>
      List(
        onPointerDown.filter(_.isPrimary).compose(_.withCurrentValueOf(canDrag)) --> { (pev, canDrag) =>
          if (canDrag && pev.button == MouseButtonLeft) {
            self.ref.setPointerCapture(pev.pointerId)
            val bbox  = self.ref.getBoundingClientRect()
            val coord = Coord(x = pev.clientX - bbox.x, y = pev.clientY - bbox.y)
            downMouseCoord.set(Some(coord))
          }
        },
        onPointerUp.mapToUnit --> (_ => downMouseCoord.set(None)),
        onPointerMove
          .compose(
            _.withCurrentValueOf(downMouseCoord.signal, coordSig)
              .filter(_._2.isDefined)
              .map((ev, mopt, cpos) => (ev, mopt.get, cpos))
          ) --> { (pev, mouseOffset, currentPos) =>
          val parent    = self.ref.parentNode.asInstanceOf[org.scalajs.dom.Element]
          val parentDim = parent.getBoundingClientRect()

          val rawLeft = pev.clientX - parentDim.left - mouseOffset.x
          val rawTop  = pev.clientY - parentDim.top - mouseOffset.y

          val nextLeft = Option.when(rawLeft >= 0)(rawLeft - (rawLeft % gridSnap)).getOrElse(currentPos.x)
          val nextTop  = Option.when(rawTop >= 0)(rawTop - (rawTop % gridSnap)).getOrElse(currentPos.y)

          val next = Coord(nextLeft, nextTop)
          pos.onUpdate.onNext(next)
        }
      )
    ),
    Modifier.apply({ self =>
      self.amend(
        display <-- coordSig.map(c => if (c == Coord.Hidden) "none" else ""),
        left <-- coordSig.map(c => s"${c.x}px"),
        top <-- coordSig.map(c => s"${c.y}px")
      )
      f(isDragging, self)
    })
  )
