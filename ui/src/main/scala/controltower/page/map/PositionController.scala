package controltower.page.map

import org.updraft0.controltower.protocol.*

import scala.annotation.unused

case class Coord(x: Double, y: Double)

object Coord:
  val Hidden = Coord(-1, -1)
  val Origin = Coord(0, 0)

/** Determines positions on the map
  */
trait PositionController:

  /** Display data that a newly added system will have when being added to the map
    */
  def newSystemDisplay: SystemDisplayData

  /** Determine the position of the system (given the display data)
    */
  def positionOfSystem(systemId: SystemId, display: SystemDisplayData): Coord

  /** Generate new display data based on the previous and new coordinate position
    */
  def updateDisplayFromPosition(systemId: SystemId, prev: Option[SystemDisplayData], position: Coord): SystemDisplayData

/** Initial implementation of position controller (stateless)
  */
object PositionController extends PositionController:
  override def newSystemDisplay: SystemDisplayData = SystemDisplayData.Manual(0, 0)
  override def positionOfSystem(@unused systemId: SystemId, display: SystemDisplayData): Coord = display match
    case SystemDisplayData.Manual(x, y) => Coord(x, y)
  override def updateDisplayFromPosition(
      @unused systemId: SystemId,
      @unused prev: Option[SystemDisplayData],
      position: Coord
  ): SystemDisplayData =
    SystemDisplayData.Manual(position.x.toInt, position.y.toInt)
