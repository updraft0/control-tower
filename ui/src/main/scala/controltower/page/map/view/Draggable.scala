package controltower.page.map.view

import org.updraft0.controltower.constant.MagicConstant
import com.raquo.laminar.api.L.*
import controltower.page.map.Hidden
import controltower.ui.*

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
    position: Var[Coord],
    canDrag: Signal[Boolean],
    updatePos: Coord => Unit,
    f: HtmlElement => Unit,
    gridSnap: Int = MagicConstant.GridSnapPx
): HtmlElement =
  // FIXME initial position is always Hidden so we always send a display update first time the element is clicked
  val initialState = DragState(isDragging = false, initial = Hidden)
  val downMouse    = Var[Option[Coord]](None)
  val stateVar     = Var(initialState)
  div(
    display <-- position.signal.map(c => if (c == Hidden) "none" else ""),
    left <-- position.signal.map(c => s"${c.x}px"),
    top <-- position.signal.map(c => s"${c.y}px"),
    cls <-- canDrag.map {
      case true  => "draggable-box"
      case false => "pinned-box"
    },
    inContext(self =>
      modSeq(
        onPointerDown
          .filter(_.filterWith(PointerFilter.PrimaryPointer | PointerFilter.MouseButtonLeft))
          .compose(
            _.withCurrentValueOf(canDrag).filter(_._2).map(_._1).withCurrentValueOf(position.signal)
          ) --> { (pev, currentPos) =>
          self.ref.setPointerCapture(pev.pointerId)
          val bbox       = self.ref.getBoundingClientRect()
          val mouseCoord = Coord(x = pev.clientX - bbox.x, y = pev.clientY - bbox.y)
          Var.set(
            downMouse -> Some(mouseCoord),
            stateVar  -> DragState(isDragging = true, initial = currentPos)
          )
        },
        onPointerUp.mapToUnit.compose(_.withCurrentValueOf(position.signal)) --> (currentPos =>
          Var.update(
            downMouse -> ((_: Option[Coord]) => None),
            stateVar -> { (prev: DragState) =>
              if (prev.initial != currentPos) updatePos(currentPos)
              DragState(isDragging = false, initial = currentPos)
            }
          )
        ),
        onPointerCancel.mapToUnit.compose(_.withCurrentValueOf(stateVar)) --> (currentState =>
          Var.set(
            downMouse -> None,
            stateVar  -> initialState,
            position  -> currentState.initial
          )
        ),
        onPointerMove
          .compose(
            _.withCurrentValueOf(downMouse.signal, position.signal)
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
          if (position.now() != next)
            position.set(next)
        }
      )
    ),
    Modifier.apply(f)
  )
