package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.page.map.Coord
import controltower.page.map.view.SelectionState.Selecting
import controltower.ui.*
import org.updraft0.controltower.constant.SystemId
import org.scalajs.dom

import scala.collection.mutable

enum SelectionState derives CanEqual:
  case Selecting(start: Coord, finish: Coord, bbox: Coord)
  case Stopped

case class ObserverState(rootElement: dom.Element, elements: mutable.ArrayBuffer[Element])

// really magic constants FIXME move
private val Threshold            = 0.0
private val ThrottleMs           = 100
private val ObserverDisconnectMs = 80

final class SelectionView(
    singleSelected: Signal[Option[SystemId]],
    selection: Observer[Array[SystemId]],
    systemNodes: EventStream[CollectionCommand[Element]]
):
  private val state = Var(SelectionState.Stopped)
  private val stateSelecting = state.signal.map:
    case _: SelectionState.Selecting => true
    case _                           => false

  private val observerState = Var[ObserverState | Null](null)

  def eventHandlers: Modifier[HtmlElement] =
    modSeq(
      onPointerDown
        .filter(_.filterWith(PointerFilter.MouseButtonLeft | PointerFilter.PrimaryPointer))
        .compose(_.withCurrentValueOf(observerState.signal))
        --> { (pev, obsState) =>
          val parentBounds = obsState.rootElement.getBoundingClientRect()
          val bbox         = Coord(parentBounds.x, parentBounds.y)
          val mouseCoord   = Coord(x = pev.clientX - bbox.x, y = pev.clientY - bbox.y)

          state.set(SelectionState.Selecting(mouseCoord, mouseCoord, bbox))
        },
      onPointerMove.compose(
        _.filterWith(stateSelecting)
          .withCurrentValueOf(state.signal)
      ) --> { (pev, ss) =>
        ss match
          case SelectionState.Selecting(start, _, bbox) =>
            // set pointer capture to the selection rectangle div (unfortunately easier to find via DOM)
            val el = org.scalajs.dom.document.querySelector("div.selection-rectangle")
            if (el != null && !el.hasPointerCapture(pev.pointerId))
              el.setPointerCapture(pev.pointerId)

            val mouseCoord = Coord(x = pev.clientX - bbox.x, y = pev.clientY - bbox.y)
            state.set(Selecting(start, mouseCoord, bbox))
          case _ => ()
      },
      onPointerMove.compose(
        _.throttle(ThrottleMs)
          .filterWith(stateSelecting)
          .mapToUnit
          .withCurrentValueOf(state.signal, observerState.signal, singleSelected)
      ) --> { (ss, obsState, singleSelectedOpt) =>
        ss match
          case SelectionState.Selecting(start, finish, _) =>
            val opts = new dom.IntersectionObserverInit {}
            opts.root = obsState.rootElement
            opts.threshold = Threshold

            val parentBounds    = obsState.rootElement.getBoundingClientRect()
            val (leftX, rightX) = if (start.x > finish.x) (finish.x, start.x) else (start.x, finish.x)
            val (topY, bottomY) = if (start.y > finish.y) (finish.y, start.y) else (start.y, finish.y)

            val leftMargin   = s"${-leftX}px"
            val topMargin    = s"${-topY}px"
            val rightMargin  = s"${-(parentBounds.width - rightX)}px"
            val bottomMargin = s"${-(parentBounds.height - bottomY)}px"

            // very hacky - because you cannot set a root element outside the ancestor elements, we set the root
            //    to be the map container and then apply negative margins that correspond to the selection rectangle
            //    we are drawing. the margin is static so need to create a new intersection observer every time...
            opts.rootMargin = s"$topMargin $rightMargin $bottomMargin $leftMargin"

            val observer = new dom.IntersectionObserver(
              { (entries, _) =>
                val systemIds = entries.view
                  .filter(e =>
                    e.isIntersecting &&
                      e.target.id.startsWith("system-") &&
                      singleSelectedOpt.forall(sId => !e.target.id.endsWith(sId.toString)) &&
                      !e.target.classList.contains("pinned-box") // manual fix for excluding isPinned systems
                  )
                  .map(e => SystemId(e.target.id.stripPrefix("system-").toLong))
                  .toArray
                selection.onNext(systemIds)
              },
              opts
            )
            val targets = obsState.elements
            targets.foreach((t: Element) => observer.observe(t.ref))

            // hacky - wait for the intersection observer to compute intersections and then kill it
            scala.scalajs.js.timers.setTimeout(ObserverDisconnectMs)(observer.disconnect())

          case _ => ()
      },
      onPointerCancel.mapTo(SelectionState.Stopped) --> state,
      onClick.mapTo(SelectionState.Stopped) --> state,
      onPointerUp.filter { pev =>
        // very hacky again - we do not propagate events that have the capture on the selection rectangle
        val tgt = org.scalajs.dom.document.querySelector("div.selection-rectangle")
        tgt != null && tgt.hasPointerCapture(pev.pointerId)
      }.stopImmediatePropagation --> Observer.empty,
      systemNodes
        .compose(_.withCurrentValueOf(observerState)) --> { (cmd, obsState) =>
        cmd match
          case CollectionCommand.Append(el) => obsState.elements.append(el)
          case CollectionCommand.Remove(el) =>
            obsState.elements.indexOf(el) match
              case -1  => ()
              case idx => obsState.elements.remove(idx)

          case CollectionCommand.Replace(prev, next) =>
            obsState.elements.indexOf(prev) match
              case -1 =>
                obsState.elements.append(next)
              case idx =>
                obsState.elements(idx) = next
          case _ => () // no-op, assume unsupported
      }
    )

  def view: Modifier[HtmlElement] =
    inContext(self =>
      modSeq(
        div(
          cls := "selection-rectangle",
          onMountUnmountCallback(
            { ctx =>
              val parentParent = ctx.thisNode.maybeParent.get.ref.parentNode.asInstanceOf[dom.Element]

              Var.set(
                (state, SelectionState.Stopped),
                observerState -> ObserverState(parentParent, mutable.ArrayBuffer.empty)
              )
              selection.onNext(Array.empty)
            },
            { _ =>
              Var.update(
                state         -> ((_: SelectionState) => SelectionState.Stopped),
                observerState -> { (_: ObserverState) => null }
              )
              selection.onNext(Array.empty)
            }
          ),
          display <-- state.signal.map {
            case SelectionState.Stopped => "none"
            case _                      => ""
          },
          styleAttr <-- state.signal.map {
            case SelectionState.Selecting(start, finish, pbbox) =>
              val bbox = self.ref.getBoundingClientRect()

              val (leftX, rightX) = if (start.x > finish.x) (finish.x, start.x) else (start.x, finish.x)
              val (topY, bottomY) = if (start.y > finish.y) (finish.y, start.y) else (start.y, finish.y)

              s"left: ${leftX - bbox.x + pbbox.x}px; top: ${topY - bbox.y + pbbox.y}px; width: ${rightX - leftX}px; height: ${bottomY - topY}px;"
            case _ => ""
          }
        )
      )
    )
