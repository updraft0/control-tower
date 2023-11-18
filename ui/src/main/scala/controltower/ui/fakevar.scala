package controltower.ui

import com.raquo.airstream.core.{Observer, Signal}
import com.raquo.airstream.ownership.Owner
import com.raquo.airstream.state.Var
import com.raquo.airstream.state.StrictSignal
import com.raquo.airstream.split.Splittable

import scala.collection.immutable.{SortedMap, TreeMap}

// FIXME current workaround for https://github.com/raquo/Airstream/issues/112
class FakeVectorVar[A]:

  private val underlying = Var(Vector.empty[(Int, A)])
  private val counter    = Var(0)

  def now(): Vector[(Int, A)] = underlying.now()

  def append(el: A) =
    Var.update(
      counter    -> ((c: Int) => c + 1),
      underlying -> ((v: Vector[(Int, A)]) => v :+ counter.now() -> el)
    )

  def setAll(all: Vector[A]) =
    Var.update(
      counter    -> ((_: Int) => all.size),
      underlying -> ((_: Vector[(Int, A)]) => all.zipWithIndex.map(_.swap))
    )

  def removeAt(ctr: Int) = underlying.update(v => v.filterNot(_._1 == ctr))

  def split[B](project: FakeVar[A] => B)(using owner: Owner): Signal[Vector[B]] =
    val obs = Observer[Either[Int, (Int, A)]]:
        case Left(ctrToRemove) => removeAt(ctrToRemove)
        case Right((ctr, value)) =>
          underlying.update(vec =>
            vec.indexWhere(_._1 == ctr) match
              case -1  => vec // no-op
              case idx => vec.updated(idx, (ctr, value))
          )

    underlying.signal.split(_._1)((idx, _, in) => project(FakeVar(in, obs, owner)))

class FakeVar[A](origSignal: Signal[(Int, A)], origObserver: Observer[Either[Int, (Int, A)]], owner: Owner):
  val current = origSignal.observe(owner)

  def counter()             = current.now()._1
  def now()                 = current.now()._2
  def signal: Signal[A]     = origSignal.map(_._2)
  def onUpdate: Observer[A] = origObserver.contramap(v => Right(counter() -> v))
  def onUpdateZoom[B](out: (A, B) => A): Observer[B] = origObserver.contramap(b =>
    val (ctr, a) = current.now()
    Right(ctr -> out(a, b))
  )
  def onDelete: Observer[Unit] = origObserver.contramap(_ => Left(counter()))

given Splittable[Iterable] with
  override def map[A, B](inputs: Iterable[A], project: A => B): Iterable[B] = inputs.map(project)
  override def zipWithIndex[A](inputs: Iterable[A]): Iterable[(A, Int)]     = inputs.zipWithIndex
  override def empty[A]: Iterable[A]                                        = Iterable.empty

class FakeMapVar[K: Ordering, V]:
  private val underlying = Var[SortedMap[K, V]](TreeMap.empty[K, V])

  def now(): SortedMap[K, V]                        = underlying.now()
  def update(f: SortedMap[K, V] => SortedMap[K, V]) = underlying.update(f)
  def removeKey(k: K)                               = update(_.removed(k))

  def splitByKey[U](project: (K, FakeVarM[K, V]) => U)(using owner: Owner): Signal[Iterable[U]] =
    val obs = Observer[Either[K, (K, V)]]:
        case Left(keyToRemove) => removeKey(keyToRemove)
        case Right((k, v))     => update(_.updated(k, v))

    (underlying.signal: StrictSignal[Iterable[(K, V)]]).split[U, K](_._1)((k, _, in) =>
      project(k, FakeVarM[K, V](in, obs, owner))
    )

class FakeVarM[K, V](origSignal: Signal[(K, V)], origObserver: Observer[Either[K, (K, V)]], owner: Owner):
  val current = origSignal.observe(owner)

  def key()                  = current.now()._1
  def now()                  = current.now()._2
  lazy val signal: Signal[V] = origSignal.map(_._2)
  def zoomIn[U](in: V => U)(out: (V, U) => V): FakeVarM[K, U] =
    FakeVarM[K, U](
      origSignal.map((k, v) => k -> in(v)),
      origObserver.contramap[Either[K, (K, U)]](eku => eku.map((k, u) => k -> out(now(), u))),
      owner
    )

  lazy val onUpdate: Observer[V] = origObserver.contramap(v => Right(key() -> v))
  def onUpdateZoom[U](out: (K, V, U) => V): Observer[U] = origObserver.contramap(u =>
    val (k, v) = current.now()
    Right(k -> out(k, v, u))
  )
  lazy val onDelete: Observer[Unit] = origObserver.contramap(_ => Left(key()))
