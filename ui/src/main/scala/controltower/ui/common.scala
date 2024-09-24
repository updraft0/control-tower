package controltower.ui

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import org.scalajs.dom.PointerEvent
import PointerFilter.*

import scala.annotation.targetName

case class Coord(x: Double, y: Double) derives CanEqual

extension [A, B](c: List[Either[A, B]])
  def sequence: Either[A, List[B]] = c.foldRight[Either[A, List[B]]](Right(Nil)):
    case (_, Left(l))          => Left(l)
    case (Left(l), _)          => Left(l)
    case (Right(n), Right(xs)) => Right(n :: xs)

extension [T <: scala.reflect.Enum](t: T)
  // TODO: this works but not when you splice it into a varargs constructor currently
  def selectOption: ReactiveHtmlElement[dom.HTMLOptionElement] =
    option(value := t.toString, t.toString)

extension [A](s: EventStream[A])
  def sampleCollectSome[B](source: SignalSource[Option[B]]): EventStream[B] =
    s.sample(source).filter(_.isDefined).map(_.get)

/** Marker trait for "view controllers" i.e. the coupling of display logic with the control logic needed to drive it
  */
trait ViewController:
  def view: Element

// common binds

val onEnterPress = onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)
inline def hideIfEmptyOpt[A](opt: Observable[Option[A]]) =
  display <-- opt.map:
    case Some(_) => ""
    case None    => "none"

/** Filter for pointer events
  */
opaque type PointerFilter = Long

object PointerFilter:
  inline def MouseButtonLeft: PointerFilter  = 1 << 1
  inline def MouseButtonRight: PointerFilter = 1 << 2
  inline def CtrlKey: PointerFilter          = 1 << 3
  inline def ShiftKey: PointerFilter         = 1 << 4
  inline def MetaKey: PointerFilter          = 1 << 5
  inline def PrimaryPointer: PointerFilter   = 1 << 6

  object MouseButtons:
    val Left  = 0
    val Right = 2

  extension (p: PointerFilter)
    @targetName("bitwise_and")
    inline def &(other: PointerFilter): PointerFilter = (p: Long) & (other: Long)

    @targetName("bitwise_or")
    inline def |(other: PointerFilter): PointerFilter = (p: Long) | (other: Long)

    inline def and(other: PointerFilter): Boolean = (p & other) == other
    inline def andButton(button: Int): Boolean =
      (button == MouseButtons.Left && p.and(MouseButtonLeft)) ||
        (button == MouseButtons.Right && p.and(MouseButtonRight))

extension (pev: PointerEvent)
  inline def filterWith(p: PointerFilter): Boolean =
    // note: primary pointer is always filtered currently, may want to revisit
    pev.isPrimary == p.and(PrimaryPointer) &&
      pev.shiftKey == p.and(ShiftKey) &&
      pev.ctrlKey == p.and(CtrlKey) &&
      pev.metaKey == p.and(MetaKey) &&
      p.andButton(pev.button)

  /** Compute the position of this pointer event relative to an element's parent
    */
  inline def posRelativeToParent(self: Element): Coord =
    val parent    = self.ref.parentNode.asInstanceOf[org.scalajs.dom.Element]
    val parentDim = parent.getBoundingClientRect()
    Coord(pev.clientX - parentDim.left, pev.clientY - parentDim.top)

  /** Compute the position of this pointer event relative to an element
    */
  inline def posRelativeTo(self: Element): Coord =
    val dim = self.ref.getBoundingClientRect()
    Coord(pev.clientX - dim.left, pev.clientY - dim.top)
