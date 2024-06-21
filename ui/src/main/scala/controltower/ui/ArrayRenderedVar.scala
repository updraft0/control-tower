package controltower.ui

import com.raquo.laminar.api.L.*

import scala.reflect.ClassTag
import scala.collection.mutable
import scala.util.Random

type RenderT[A] = (StrictSignal[A], Observer[Option[A]]) => Element

/** A "fake" var that represents a collection which can be appended but the render function can also update elements.
  * This makes up for .split not being available on Vars, see https://github.com/raquo/Airstream/issues/112
  */
final class ArrayRenderedVar[A: ClassTag] private (render: RenderT[A])(using CanEqual[A, A]):
  private val internalVar = Var(mutable.ArraySeq.empty[(Long, Var[A], Element)])
  private val outBus      = EventBus[CollectionCommand[Element]]()

  def this(render: RenderT[A], items: Array[A])(using CanEqual[A, A]) =
    this(render)
    items.foreach(append)

  def this(render: RenderT[A], items: A*)(using CanEqual[A, A]) =
    this(render)
    items.foreach(append)

  private inline def removeOrUpdateFor(k: Long, v: Var[A]) =
    Observer[Option[A]]:
      case None       => removeAt(k)
      case Some(next) => updateAt(k, next)

  // TODO: not very nice
  def itemsNow: Seq[A] = internalVar.now().toSeq.map(_._2).map(_.now())

  def append(item: A): Unit =
    val k  = Random.nextLong
    val v  = Var(item)
    val el = render(v.signal, removeOrUpdateFor(k, v))
    outBus.emit(CollectionCommand.Append(el))
    internalVar.update(arr => arr.appended((k, v, el)))

  def remove(item: A): Unit =
    internalVar.update: arr =>
      arr.indexWhere(_._2.now() == item) match
        case -1  => arr // no-op
        case idx => doRemove(arr, idx)

  private def removeAt(k: Long): Unit =
    internalVar.update: arr =>
      arr.indexWhere(_._1 == k) match
        case -1  => arr // no-op
        case idx => doRemove(arr, idx)

  private def doRemove(arr: mutable.ArraySeq[(Long, Var[A], Element)], idx: Int) =
    val (k, v, el) = arr(idx)
    outBus.emit(CollectionCommand.Remove(el))
    arr.filterNot((ik, _, _) => ik == k)

  def update(item: A, newItem: A): Unit =
    internalVar.update: arr =>
      arr.indexWhere(_._2.now() == item) match
        case -1  => arr // no-op
        case idx => doUpdate(arr, idx, newItem)

  private def updateAt(k: Long, newItem: A): Unit =
    internalVar.update: arr =>
      arr.indexWhere(_._1 == k) match
        case -1  => arr // no-op
        case idx => doUpdate(arr, idx, newItem)

  private def doUpdate(arr: mutable.ArraySeq[(Long, Var[A], Element)], idx: Int, next: A) =
    val (k, v, prevEl) = arr(idx)
    v.set(next)
    // note - do not need to do anything with the element - it is not getting replaced
    arr

  def clear(): Unit =
    internalVar.update: arr =>
      arr.foreach((_, _, el) => outBus.emit(CollectionCommand.Remove(el)))
      mutable.ArraySeq.empty

  def elements: EventStream[CollectionCommand[Element]] = outBus.events

  def live: Modifier[HtmlElement] =
    onMountUnmountCallback(
      mount = _ => internalVar.now().foreach((_, _, el) => outBus.emit(CollectionCommand.Append(el))),
      unmount = _ => internalVar.now().foreach((_, _, el) => outBus.emit(CollectionCommand.Remove(el)))
    )
