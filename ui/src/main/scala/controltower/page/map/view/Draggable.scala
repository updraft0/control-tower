package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.page.map.Coord

private val DefaultGridSnapPx = 10
private val MouseButtonLeft   = 0

case class DragState(isDragging: Boolean, initial: Coord)

/** Make a draggable content element with grid snapping
  * @param coordSig
  *   Signal with the current position
  * @param canDrag
  *   Whether to allow dragging or not
  * @param updatePos
  *   Callback to update the position
  * @param f
  *   The function that draws the content of the element (parent) => ()
  * @param gridSnap
  *   The grid snapping pixel size
  */
private def inDraggable(
    coordSig: Signal[Coord],
    canDrag: Signal[Boolean],
    updatePos: Coord => Unit,
    f: HtmlElement => Unit,
    gridSnap: Int = DefaultGridSnapPx
): HtmlElement =
  val initialState  = DragState(isDragging = false, initial = Coord.Hidden)
  val upstreamCoord = Var[Coord](Coord.Hidden)
  val currentCoord  = Var[Coord](Coord.Hidden)
  val downMouse     = Var[Option[Coord]](None)
  val stateVar      = Var(initialState)
  div(
    coordSig --> currentCoord.writer,
    coordSig --> upstreamCoord.writer,
    display <-- currentCoord.signal.map(c => if (c == Coord.Hidden) "none" else ""),
    left <-- currentCoord.signal.map(c => s"${c.x}px"),
    top <-- currentCoord.signal.map(c => s"${c.y}px"),
    cls <-- canDrag.map {
      case true  => "draggable-box"
      case false => "pinned-box"
    },
    inContext(self =>
      modSeq(
        onPointerDown
          .filter(pev => pev.isPrimary && pev.button == MouseButtonLeft)
          .compose(
            _.withCurrentValueOf(canDrag).filter(_._2).map(_._1).withCurrentValueOf(coordSig)
          ) --> { (pev, currentPos) =>
          self.ref.setPointerCapture(pev.pointerId)
          val bbox       = self.ref.getBoundingClientRect()
          val mouseCoord = Coord(x = pev.clientX - bbox.x, y = pev.clientY - bbox.y)
          Var.set(
            downMouse    -> Some(mouseCoord),
            stateVar     -> DragState(isDragging = true, initial = currentPos),
            currentCoord -> currentPos
          )
        },
        onPointerUp.mapToUnit --> (_ =>
          Var.update(
            downMouse -> ((_: Option[Coord]) => None),
            stateVar -> { (prev: DragState) =>
              if (upstreamCoord.now() != currentCoord.now())
                updatePos(currentCoord.now())
              initialState
            }
          )
        ),
        onPointerCancel.mapToUnit --> (_ =>
          Var.set(
            downMouse -> None,
            stateVar  -> initialState
          )
        ),
        onPointerMove
          .compose(
            _.withCurrentValueOf(downMouse.signal, coordSig)
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
          currentCoord.set(next)
        }
      )
    ),
    Modifier.apply(f)
  )
