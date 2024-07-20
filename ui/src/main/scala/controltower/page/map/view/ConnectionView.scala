package controltower.page.map.view

import com.raquo.laminar.api.L.*
import com.raquo.laminar.codecs.StringAsIsCodec
import controltower.page.map.{Coord, MapAction, PositionController}
import controltower.ui.{HVar, ViewController}
import org.updraft0.controltower.constant.{ConnectionId, SystemId}
import org.updraft0.controltower.protocol.MapWormholeConnectionWithSigs

object ConnectionView:
  // TODO: figure out an optimal value for this
  val Curviness = (25, 10)

  val EndRadius = 4

  def dataAttr(name: String) = svg.svgAttr(s"data-$name", StringAsIsCodec, namespace = None)

class ConnectionView(
    id: ConnectionId,
    fromSystemId: SystemId,
    toSystemId: SystemId,
    conn: Signal[MapWormholeConnectionWithSigs],
    selectedConnection: Var[Option[ConnectionId]],
    pos: PositionController,
    systemViewSize: Coord
) extends ViewController:
  import svg.*
  import ConnectionView.dataAttr

  // TODO: direction

  private val fromSystemCoord      = pos.systemPosition(fromSystemId)
  private val toSystemCoord        = pos.systemPosition(toSystemId)
  private val connectionWithCoords = conn.combineWith(fromSystemCoord.signal, toSystemCoord.signal)

  override def view =
    g(
      cls                       := "wormhole-connection",
      dataAttr("connection-id") := s"$id",
      dataAttr("mass-size") <-- conn.map(getWormholeMassSize).map(_.toString),
      dataAttr("mass-status") <-- conn.map(getWormholeMassStatus).map(_.toString),
      cls("eol") <-- conn.map(whc =>
        whc.toSignature.exists(_.eolAt.isDefined) || whc.fromSignature.exists(_.eolAt.isDefined)
      ),
      cls("selected") <-- selectedConnection.signal.map(_.exists(_ == id)),
      onClick
        .filter(ev => !ev.ctrlKey && !ev.shiftKey && !ev.metaKey)
        .stopPropagation
        .mapToUnit
        .compose(_.withCurrentValueOf(selectedConnection).map {
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
            ),
            text(
              cls              := "connection-size",
              filterAttr       := "url(#background-size)",
              x                := s"${startPos.x + ((endPos.x - startPos.x) / 2)}",
              y                := s"${startPos.y + ((endPos.y - startPos.y) / 2)}",
              textAnchor       := "middle",
              dominantBaseline := "central",
              getWormholeMassSize(conn).toString
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
    onMountBind(ctx =>
      cls("stopped") <-- state.signal(using ctx.owner).map {
        case MapNewConnectionState.Stopped => true
        case _                             => false
      },
    ),
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
