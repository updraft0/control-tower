package controltower.ui

import com.raquo.laminar.api.L.*

/** A `Var` that keeps a history of the previous value
  */
final class HVar[A](value: Var[(Option[A], A)]):

  def signal(using Owner): Signal[A]          = current.signal
  def rawSignal: StrictSignal[(Option[A], A)] = value.signal

  def current(using Owner): Var[A] = value.zoom { case ((_, a)) =>
    a
  } { case ((_, prevA), newA) =>
    Some(prevA) -> newA
  }

  protected[ui] def raw: Var[(Option[A], A)] = value

object HVar:
  def apply[A](initial: A): HVar[A] = new HVar[A](Var(None -> initial))

extension [K, V](h: HVar[Map[K, V]])
  def writeChangesTo(
      writeBus: WriteBus[CollectionCommand[V]],
      isSame: (K, V, V) => Boolean = (_: K, a: V, b: V) => a == b
  )(using Owner): Unit =
    h.raw.signal.addObserver(Observer {
      case (Some(prevMap), nextMap) =>
        val prevKeys = prevMap.keySet
        val nextKeys = nextMap.keySet

        prevKeys.diff(nextKeys).foreach(k => writeBus.onNext(CollectionCommand.Remove(prevMap(k))))
        nextKeys.diff(prevKeys).foreach(k => writeBus.onNext(CollectionCommand.Append(nextMap(k))))
        prevKeys
          .intersect(nextKeys)
          .foreach(k =>
            if (!isSame(k, prevMap(k), nextMap(k))) writeBus.onNext(CollectionCommand.Replace(prevMap(k), nextMap(k)))
          )
      case (None, nextMap) =>
        nextMap.foreach((_, v) => CollectionCommand.Append(v))
    })
