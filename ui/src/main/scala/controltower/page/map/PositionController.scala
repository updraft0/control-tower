package controltower.page.map

import com.raquo.airstream.state.Var.VarTuple
import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import org.updraft0.controltower.constant.SystemId
import org.updraft0.controltower.protocol.*
import controltower.ui.Coord

import scala.collection.mutable

export controltower.ui.Coord

val Hidden: Coord = Coord(-1, -1)

/** Determines positions on the map
  */
trait PositionController:

  /** Display data that a newly added system will have when being added to the map
    */
  def newSystemDisplay: SystemDisplayData

  def systemPosition(systemId: SystemId): Var[Coord]

  def systemDisplayData(systemId: SystemId)(using Owner): Var[Option[SystemDisplayData]]

  def clear(): Unit

  def pointInsideBox(coord: Coord): Option[SystemId]

/** Initial implementation of position controller (stateless)
  */
class VarPositionController(map: mutable.Map[SystemId, Var[Coord]], boxSize: Coord) extends PositionController:
  override def newSystemDisplay: SystemDisplayData = SystemDisplayData.Manual(0, 0)

  override def systemPosition(systemId: SystemId): Var[Coord] =
    map.getOrElseUpdate(systemId, Var(Hidden))

  override def systemDisplayData(systemId: SystemId)(using Owner): Var[Option[SystemDisplayData]] =
    systemPosition(systemId).zoom(c => Option.when(c != Hidden)(SystemDisplayData.Manual(c.x.toInt, c.y.toInt))) {
      case (coord, Some(m: SystemDisplayData.Manual)) => Coord(m.x, m.y)
      case (_, _)                                     => Hidden
    }

  override def clear(): Unit =
    Var.set(map.view.values.map(v => (v -> Hidden): VarTuple[?]).toSeq*)
    map.clear()

  override def pointInsideBox(coord: Coord): Option[SystemId] =
    map.view.find((_, coordVar) => isInsideBox(coordVar.now(), boxSize, coord)).map(_._1)

private inline def isInsideBox(boxStart: Coord, boxSize: Coord, point: Coord) =
  point.x >= boxStart.x && point.y >= boxStart.y && point.x < boxStart.x + boxSize.x && point.y < boxStart.y + boxSize.y
