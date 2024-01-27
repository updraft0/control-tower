package controltower.ui

import scala.collection.mutable
import com.raquo.laminar.api.L.*

class CollectionCommandTransformer[In, Cache, Out, Key](
    key: In => Key,
    cache: Var[In] => Cache,
    view: (Cache, Var[In]) => Out
):

  private val state = mutable.Map.empty[Key, (Var[In], Cache, Out)]

  def run(in: EventStream[CollectionCommand[In]]): EventStream[CollectionCommand[Out]] =
    in.collectOpt:
      case CollectionCommand.Append(it) =>
        val v   = Var(it)
        val c   = cache(v)
        val out = view(c, v)
        state.update(key(it), (v, c, out))
        Some(CollectionCommand.Append(out))
      case CollectionCommand.Remove(el) =>
        val oldOut = state(key(el))._3
        state.remove(key(el))
        Some(CollectionCommand.Remove(oldOut))
      case CollectionCommand.Replace(oldIt, newIt) =>
        val v = state(key(oldIt))._1
        v.set(newIt)
        None
      case _ => throw new RuntimeException("Unsupported CollectionCommand")
