package controltower.component

import com.raquo.laminar.api.L.*

enum SelectAction[K](val key: K):
  case Toggle(override val key: K)   extends SelectAction(key)
  case Select(override val key: K)   extends SelectAction(key)
  case Unselect(override val key: K) extends SelectAction(key)

class Selectable[K](current: Var[Set[K]]):
  val writer: Observer[SelectAction[K]] = Observer[SelectAction[K]] {
    case SelectAction.Toggle(key)   => current.update(s => if (s.contains(key)) s - key else s + key)
    case SelectAction.Select(key)   => current.update(s => s + key)
    case SelectAction.Unselect(key) => current.update(s => s - key)
  }

  val signal: Signal[Set[K]] = current.signal.debugWithName("selected")

  def clear(): Unit                           = current.update(_ => Set.empty)
  def clearUpdater: Observer[Any]             = current.updater[Any]((_, _) => Set.empty)
  def filterBy: Observer[Set[K]]              = current.updater[Set[K]]((curr, next) => curr -- next)
  def isSelected(key: K): Observable[Boolean] = current.signal.map(_.contains(key))
  def toggle(key: K): Observer[Unit]          = writer.contramap(_ => SelectAction.Toggle(key))
  def select(key: K): Observer[Unit]          = writer.contramap(_ => SelectAction.Select(key))
  def unselect(key: K): Observer[Unit]        = writer.contramap(_ => SelectAction.Unselect(key))

object Selectable:
  def apply[K](initial: Set[K] = Set.empty[K]): Selectable[K] = new Selectable[K](Var(initial))
