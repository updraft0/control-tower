package controltower.page

import com.raquo.airstream.ownership.ManualOwner
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import controltower.Page
import controltower.backend.ControlTowerBackend
import org.updraft0.controltower.protocol.{
  MapDisplayType,
  MapMessage,
  MapRequest,
  MapSystem,
  MapSystemSnapshot,
  SystemDisplayData
}
import controltower.ui.{FakeMapVar, FakeVar, FakeVarM, FakeVectorVar}

import scala.collection.immutable.TreeMap
import scala.concurrent.ExecutionContext.Implicits.global

case class Coord(x: Double, y: Double)

object Coord:
  val Origin = Coord(0, 0)

object MapPage:

  def renderPage(map: Page.Map)(using ct: ControlTowerBackend) =
    // TODO: websocket - abstract away? :)
    val owner = ManualOwner()

    val reqBus   = EventBus[MapRequest]()
    val mapState = FakeMapVar[Long, MapSystemSnapshot]()

    ct.mapWebsocket(map.name, map.characterId)(using owner)
      .onComplete(
        _.fold(
          ex => {
            org.scalajs.dom.console.error(s"failed to get at all", ex)
          },
          {
            case Left(msg) => org.scalajs.dom.console.error(s"Failed with: $msg")
            case Right(stream) =>
              stream(reqBus.events)
                .foreach {
                  case MapMessage.MapSnapshot(sys, con) =>
                    mapState.update(_ => TreeMap.from(sys.map(s => s.system.systemId -> s)))
                  case MapMessage.SystemRemoved(systemId) =>
                    mapState.update(_.removed(systemId))
                  case MapMessage.SystemSnapshot(systemId, system, con @ unused) =>
                    mapState.update(_.updated(systemId, system))
                  case MapMessage.SystemDisplayUpdate(systemId, display) =>
                    mapState.update(_.updatedWith(systemId)(_.map(_.copy(display = Some(display)))))
                  case other => // FIXME remove
                    org.scalajs.dom.console.log(s"GOT MESSAGE $other")
                }(using owner)
              reqBus.emit(MapRequest.GetSnapshot)
          }
        )
      )

    // rendering now
    div(
      div(
        height  := "100px",
        width   := "100%",
        display := "block",
        "top placeholder"
      ),
      div(
        idAttr := "mapParent",
        cls    := "grid",
        cls    := "g-25",
        margin := "5px", // FIXME remove...
        div(
          idAttr := "mapInner",
          inContext(container =>
            children <-- mapState.splitByKey(renderSystem(reqBus.writer)(_, _)(using owner))(using owner).map(_.toSeq)
          )
        )
      ),
      div(
        idAttr := "debugOverlay",
        table(
          tr(
            th("systemId"),
            th("x"),
            th("y")
          ),
          tr(
            // FIXME
            children <-- reqBus.events
              .filter {
                case _: MapRequest.UpdateSystemDisplay => true
                case _                                 => false
              }
              .map {
                case MapRequest.UpdateSystemDisplay(systemId, SystemDisplayData.Manual(x, y)) =>
                  List(
                    td(systemId.toString),
                    td(x.toString),
                    td(y.toString)
                  )
                case _ => List(td("?"), td("?"), td("?"))
              }
          )
        )
      )
    )

private def renderSystem(
    mapReq: WriteBus[MapRequest]
)(systemId: Long, value: FakeVarM[Long, MapSystemSnapshot])(using Owner): HtmlElement =
  val pos = value.zoomIn({
    _.display match
      case Some(SystemDisplayData.Manual(x, y)) => Coord(x.toDouble, y.toDouble)
      case None                                 => Coord(-1, -1) // FIXME
  })((mss, coord) => mss.copy(display = Some(SystemDisplayData.Manual(coord.x.toInt, coord.y.toInt))))

  inDraggable(
    pos,
    { (isDragging, box) =>
      isDragging
        .compose(_.withCurrentValueOf(pos.signal))
        .changes
        .foreach((isDragging, pos) =>
          if (!isDragging)
            mapReq.onNext(MapRequest.UpdateSystemDisplay(systemId, SystemDisplayData.Manual(pos.x.toInt, pos.y.toInt)))
        )
      box.amend(
        idAttr := s"system-$systemId",
        child.text <-- value.signal.map(_.system.name.getOrElse("?"))
      )
    }
  )

private def inDraggable(pos: FakeVarM[Long, Coord], f: (Signal[Boolean], HtmlElement) => Unit, gridSnap: Int = 10) =
  val downMouseCoord = Var(Option.empty[Coord])
  val isDragging     = downMouseCoord.signal.map(_.isDefined)
  val coordSig       = pos.signal
  div(
    cls := "box",
    inContext(self =>
      List(
        onPointerDown.filter(_.isPrimary) --> { pev =>
          self.ref.setPointerCapture(pev.pointerId)
          val bbox  = self.ref.getBoundingClientRect()
          val coord = Coord(x = pev.clientX - bbox.x, y = pev.clientY - bbox.y)
          downMouseCoord.set(Some(coord))
        },
        onPointerUp.mapToUnit --> (_ => downMouseCoord.set(None)),
        onPointerMove
          .compose(
            _.withCurrentValueOf(downMouseCoord.signal, coordSig)
              .filter(_._2.isDefined)
              .map((ev, mopt, cpos) => (ev, mopt.get, cpos))
          ) --> { (pev, mouseOffset, currentPos) =>
          val parent    = self.ref.parentNode.asInstanceOf[org.scalajs.dom.Element]
          val parentDim = parent.getBoundingClientRect()

          val rawLeft = pev.clientX - parentDim.left - mouseOffset.x
          val rawTop  = pev.clientY - parentDim.top - mouseOffset.y

          val nextLeft = Option.when(rawLeft >= 0)(rawLeft - (rawLeft % gridSnap)).getOrElse(currentPos.x)
          val nextTop  = Option.when(rawTop >= 0)(rawTop - (rawTop % gridSnap)).getOrElse(currentPos.y)

          val next = Coord(nextLeft, nextTop)
          pos.onUpdate.onNext(next)
        }
      )
    ),
    Modifier.apply({ self =>
      self.amend(
        display <-- coordSig.map(c => if (c.x == -1.0 && c.y == -1.0) "none" else ""),
        left <-- coordSig.map(c => s"${c.x}px"),
        top <-- coordSig.map(c => s"${c.y}px")
      )
      f(isDragging, self)
    })
  )
