package controltower.ui

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom

extension [T <: scala.reflect.Enum](t: T)
  // TODO: this works but not when you splice it into a varargs constructor currently
  def selectOption: ReactiveHtmlElement[dom.HTMLOptionElement] =
    option(value := t.toString, t.toString)

/** Marker trait for "view controllers" i.e. the coupling of display logic with the control logic needed to drive it
  */
trait ViewController:
  def view: Element

val onEnterPress = onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)
