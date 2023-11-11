package controltower.ui

import com.raquo.airstream.core.{Observer, Signal}
import com.raquo.airstream.ownership.Owner
import com.raquo.airstream.state.Var

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
