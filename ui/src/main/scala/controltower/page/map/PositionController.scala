package controltower.page.map

case class Coord(x: Double, y: Double)

object Coord:
  val Hidden = Coord(-1, -1)
  val Origin = Coord(0, 0)

trait PositionController:

  def newSystemCoordinate: Coord

  println("Test")
