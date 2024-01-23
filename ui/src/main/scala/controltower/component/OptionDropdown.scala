package controltower.component

import com.raquo.laminar.api.L.*
import scala.concurrent.duration.{Duration, given}
import scala.collection.mutable.ArrayBuffer

trait DropdownItem[E]:
  def key(value: E): String
  def group(value: E): Option[String]
  def view(value: E): Element

/** Implements `<select>` like functionality
  */
class OptionDropdown[E](
    options: Seq[E],
    current: Var[E],
    @scala.annotation.unused mouseLeaveDelay: Duration = 500.millis
)(using D: DropdownItem[E]):

  private val parentId = s"option-dropdown-${hashCode().abs}"
  private val expanded = Var(false)
  private val entered  = Var(false)

  // TODO: the keyboard navigation here is not working properly...

  def view: Element = div(
    cls := "option-dropdown",
    cls("active") <-- expanded,
//    onKeyUp --> { ev =>
//      org.scalajs.dom.console.info(s"button keyUp: ${ev}")
//      if (ev.keyCode == KeyCode.Left || ev.keyCode == KeyCode.Right || ev.keyCode == KeyCode.Down)
//        expanded.set(true)
//    },
    button(
      cls           := "select-button",
      typ           := "button",
      role          := "combobox",
      aria.hasPopup := true,
//      aria.labelledBy := "select button", FIXME
      aria.expanded <-- expanded,
      aria.controls := s"select-dropdown-${parentId}",
      span(cls := "selected-value", child <-- current.signal.map(D.view)),
      mark(cls := "ti", cls := "ti-chevron-down", cls := "expand-dropdown"),
      onClick.stopPropagation.mapToUnit --> (_ => expanded.update(!_))
    ),
    ul(
      idAttr := s"${parentId}-list",
      cls    := "select-dropdown",
      role   := "listbox",
      options
        .foldLeft((Option.empty[String], ArrayBuffer.empty[Element])) { case ((prevGroup, els), e) =>
          val id = s"${parentId}-item-${D.key(e)}"

          if (prevGroup != D.group(e))
            D.group(e).foreach(name => els.append(div(cls := "dropdown-group", name)))

          val item =
            li(
              role := "option",
              input(
                tpe            := "radio",
                idAttr         := id,
                nameAttr       := parentId,
                value          := D.key(e),
                defaultChecked := current.now() == e,
                onChange.mapToUnit --> (() => Var.set(current -> e, expanded -> false))
              ),
              label(forId := id, D.view(e)),
              onClick --> { ev =>
                if (ev.`type` == "click" && ev.clientX != 0 && ev.clientY != 0)
                  expanded.set(false)
              },
              onKeyUp --> { ev =>
                if (ev.key == "Enter")
                  expanded.set(false)
              }
            )
          els.append(item)
          D.group(e) -> els
        }
        ._2
        .toSeq
    ),
    tabIndex := 0,
    onMouseEnter.mapToUnit --> (_ => entered.set(true)),
    onMouseLeave.mapToUnit --> (_ => entered.set(false)),
    onMouseLeave.compose(_.mapToUnit.delay(mouseLeaveDelay.toMillis.toInt).withCurrentValueOf(entered)) --> (entered =>
      if (!entered) expanded.set(false)
    )
  )
