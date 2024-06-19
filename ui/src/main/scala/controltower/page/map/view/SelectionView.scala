package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.page.map.Coord
import controltower.page.map.view.SelectionState.Selecting
import org.updraft0.controltower.constant.SystemId
import org.scalajs.dom
import scala.collection.mutable

enum SelectionState derives CanEqual:
  case Selecting(start: Coord, finish: Coord)
  case Stopped

case class ObserverState(rootElement: dom.Element, elements: mutable.ArrayBuffer[Element])

private val MouseButtonLeft = 0
private val Threshold       = 0.0

final class SelectionView(
    singleSelected: Signal[Option[Long]],
    selection: Observer[Array[SystemId]],
    systemNodes: EventStream[CollectionCommand[Element]]
):
  def view: Modifier[HtmlElement] =
    val state = Var(SelectionState.Stopped)
    val stateSelecting = state.signal.map:
      case _: SelectionState.Selecting => true
      case _                           => false

    val observerState = Var[ObserverState](null)

    inContext(self =>
      modSeq(
        onPointerDown
          .filter(pev =>
            pev.isPrimary && pev.button == MouseButtonLeft && !pev.shiftKey && !pev.ctrlKey && !pev.metaKey
          )
          --> { pev =>
            val bbox       = self.ref.getBoundingClientRect()
            val mouseCoord = Coord(x = pev.clientX - bbox.x, y = pev.clientY - bbox.y)
            state.set(SelectionState.Selecting(mouseCoord, mouseCoord))
          },
        onPointerMove.compose(
          _.filterWith(stateSelecting)
            .withCurrentValueOf(state.signal)
        ) --> { (pev, ss) =>
          ss match
            case SelectionState.Selecting(start, _) =>
              // set pointer capture to the selection rectangle div (unfortunately easier to find via DOM)
              val el = org.scalajs.dom.document.querySelector("div.selection-rectangle")
              if (el != null && !el.hasPointerCapture(pev.pointerId))
                el.setPointerCapture(pev.pointerId)

              val bbox       = self.ref.getBoundingClientRect()
              val mouseCoord = Coord(x = pev.clientX - bbox.x, y = pev.clientY - bbox.y)
              state.set(Selecting(start, mouseCoord))
            case _ => ()
        },
        onPointerMove.compose(
          _.throttle(100)
            .filterWith(stateSelecting)
            .mapToUnit
            .withCurrentValueOf(state.signal, observerState.signal, singleSelected)
        ) --> { (ss, obsState, singleSelectedOpt) =>
          ss match
            case SelectionState.Selecting(start, finish) =>
              val opts = new dom.IntersectionObserverInit {}
              opts.root = obsState.rootElement
              opts.threshold = Threshold

              val parentBounds    = obsState.rootElement.getBoundingClientRect()
              val (leftX, rightX) = if (start.x > finish.x) (finish.x, start.x) else (start.x, finish.x)
              val (topY, bottomY) = if (start.y < finish.y) (start.y, finish.y) else (finish.y, start.y)

              val leftMargin   = s"${-leftX}px"
              val topMargin    = s"${-topY}px"
              val rightMargin  = s"${-parentBounds.width + rightX}px"
              val bottomMargin = s"${-parentBounds.height + bottomY}px"

              // very hacky - because you cannot set a root element outside the ancestor elements, we set the root
              //    to be the map container and then apply negative margins that correspond to the selection rectangle
              //    we are drawing. the margin is static so need to create a new intersection observer every time...
              opts.rootMargin = s"$topMargin $rightMargin $bottomMargin $leftMargin"

              val observer = new dom.IntersectionObserver(
                { (entries, _) =>
                  val systemIds = entries.view
                    .filter(e =>
                      e.isIntersecting && e.target.id.startsWith("system-") && singleSelectedOpt.forall(sId =>
                        !e.target.id.endsWith(sId.toString)
                      )
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
              scala.scalajs.js.timers.setTimeout(80)(observer.disconnect())

            case _ => ()
        },
        onPointerCancel.mapTo(SelectionState.Stopped) --> state,
        onPointerUp.mapTo(SelectionState.Stopped) --> state,
        onPointerUp.filter { pev =>
          // very hacky again - we do not propagate events that have the capture on the selection rectangle
          val tgt = org.scalajs.dom.document.querySelector("div.selection-rectangle")
          tgt != null && tgt.hasPointerCapture(pev.pointerId)
        }.stopPropagation --> Observer.empty,
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
        },
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
                observerState -> { (obs: ObserverState) => null }
              )
              selection.onNext(Array.empty)
            }
          ),
          display <-- state.signal.map {
            case SelectionState.Stopped => "none"
            case _                      => ""
          },
          styleAttr <-- state.signal.map {
            case SelectionState.Selecting(start, finish) =>
              val (leftX, rightX) = if (start.x > finish.x) (finish.x, start.x) else (start.x, finish.x)
              val (topY, bottomY) = if (start.y < finish.y) (start.y, finish.y) else (finish.y, start.y)

              s"left: ${leftX}px; top: ${topY}px; width: ${rightX - leftX}px; height: ${bottomY - topY}px;"
            case _ => ""
          }
        )
      )
    )
