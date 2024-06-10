package controltower.page.map.view

import com.raquo.laminar.api.L.*
import com.raquo.laminar.codecs.StringAsIsCodec
import controltower.page.map.{Coord, MapAction, PositionController}
import controltower.ui.{HVar, ViewController}
import org.updraft0.controltower.constant.ConnectionId
import org.updraft0.controltower.protocol.MapWormholeConnectionWithSigs

object ConnectionView:
  // TODO: figure out an optimal value for this
  val Curviness = (25, 10)

  val EndRadius = 4

  def dataAttr(name: String) = svg.svgAttr(s"data-$name", StringAsIsCodec, namespace = None)

class ConnectionView(
    id: ConnectionId,
    fromSystemId: Long,
    toSystemId: Long,
    conn: Signal[MapWormholeConnectionWithSigs],
    selectedConnection: Var[Option[ConnectionId]],
    pos: PositionController,
    systemViewSize: Coord
) extends ViewController:
  import svg.*

  // TODO: eol status
  // TODO: mass status
  // TODO: direction
  // TODO: size

  private val fromSystemCoord      = pos.systemPosition(fromSystemId)
  private val toSystemCoord        = pos.systemPosition(toSystemId)
  private val connectionWithCoords = conn.combineWith(fromSystemCoord.signal, toSystemCoord.signal)

  override def view =
    g(
      cls                                      := "wormhole-connection",
      ConnectionView.dataAttr("connection-id") := s"$id",
      cls("selected") <-- selectedConnection.signal.map(_.exists(_ == id)),
      onClick.stopImmediatePropagation.compose(_.mapToUnit.withCurrentValueOf(selectedConnection).map {
        case Some(`id`) => None
        case _          => Some(id)
      }) --> selectedConnection.writer,
      children <-- connectionWithCoords.map:
        case (conn, fromBox, toBox) =>
          val (startBox, startRank, endBox, endRank) =
            // TODO: we don't use the horizontal top/bottom lines of the box to attach connections to (unlike PF)

            if (fromBox.x + systemViewSize.x > toBox.x)
              (
                toBox,
                conn.rank.toSystemIdx / (conn.rank.toSystemCount + 1).toDouble,
                fromBox,
                conn.rank.fromSystemIdx / (conn.rank.fromSystemCount + 1).toDouble
              )
            else
              (
                fromBox,
                conn.rank.fromSystemIdx / (conn.rank.fromSystemCount + 1).toDouble,
                toBox,
                conn.rank.toSystemIdx / (conn.rank.toSystemCount + 1).toDouble
              )

          val startPos = Coord(startBox.x + systemViewSize.x, startBox.y + (systemViewSize.y * startRank))
          val endPos   = Coord(endBox.x, endBox.y + (systemViewSize.y * endRank))

          val pathExpr =
            s"M ${startPos.x} ${startPos.y} C ${startPos.x + ConnectionView.Curviness._1} ${startPos.y + ConnectionView.Curviness._2}, ${endPos.x - ConnectionView.Curviness._1} ${endPos.y - ConnectionView.Curviness._2}, ${endPos.x} ${endPos.y}"

          Seq(
            path(
              cls := "connector-path-outline",
              d   := pathExpr
            ),
            path(
              cls := "connector-path",
              d   := pathExpr
            ),
            circle(
              cls := "connector-end",
              cx  := s"${startPos.x}",
              cy  := s"${startPos.y}",
              r   := s"${ConnectionView.EndRadius}"
            ),
            circle(
              cls := "connector-end",
              cx  := s"${endPos.x}",
              cy  := s"${endPos.y}",
              r   := s"${ConnectionView.EndRadius}"
            )
          )
    )

class ConnectionInProgressView(
    state: HVar[MapNewConnectionState],
    positions: PositionController,
    systemViewSize: Coord,
    actions: WriteBus[MapAction]
) extends ViewController:
  import svg.*

  override def view = g(
    cls := "connection-in-progress",
    cls := "wormhole-connection",
    children <-- state.rawSignal
      .map {
        case (Some(MapNewConnectionState.Move(fromSystemId, pos)), MapNewConnectionState.Stopped) =>
          positions.pointInsideBox(pos) match
            case Some(toSystemId) if toSystemId != fromSystemId =>
              actions.onNext(MapAction.AddConnection(fromSystemId, toSystemId))
              nodeSeq()
            case _ =>
              nodeSeq()
        case (Some(MapNewConnectionState.Stopped), MapNewConnectionState.Start(fromSystemId, pos)) =>
          val fromSystemPos = positions.systemPosition(fromSystemId).now()

          Seq(
            circle(
              cls := "connector-end",
              cx  := s"${fromSystemPos.x + systemViewSize.x}",
              cy  := s"${fromSystemPos.y + systemViewSize.y}",
              r   := s"${ConnectionView.EndRadius}"
            )
          )
        case (_, MapNewConnectionState.Move(fromSystemId, endPos)) =>
          val fromSystemPos = positions.systemPosition(fromSystemId).now()

          // TODO: we can compute rank already here and use it to position things properly?
          val startPos = Coord(fromSystemPos.x + systemViewSize.x, fromSystemPos.y + systemViewSize.y)

          // TODO: refactor to remove duplication
          val pathExpr =
            s"M ${startPos.x} ${startPos.y} C ${startPos.x + ConnectionView.Curviness._1} ${startPos.y + ConnectionView.Curviness._2}, ${endPos.x - ConnectionView.Curviness._1} ${endPos.y - ConnectionView.Curviness._2}, ${endPos.x} ${endPos.y}"

          Seq(
            path(
              cls := "connector-path-outline",
              d   := pathExpr
            ),
            path(
              cls := "connector-path",
              d   := pathExpr
            ),
            circle(
              cls := "connector-end",
              cx  := s"${startPos.x}",
              cy  := s"${startPos.y}",
              r   := s"${ConnectionView.EndRadius}"
            )
          )
        case _ => nodeSeq()
      }
  )
