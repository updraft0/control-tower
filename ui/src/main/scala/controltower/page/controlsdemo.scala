package controltower.page

import com.raquo.laminar.api.L.*
import controltower.backend.ESI
import controltower.component.*
import org.updraft0.controltower.constant.{CharacterId, TypeId}

import java.time.{Duration, Instant}

case class JumpExample(shipType: TypeId, characterId: CharacterId, massPoints: Double, at: Instant, note: Int)

object JumpExample:
  val Samples = Array(
    JumpExample(TypeId(29984), CharacterId(2120729046), 0.1, Instant.ofEpochSecond(1735024400), 20),
    JumpExample(TypeId(670), CharacterId(2120729046), 0.001, Instant.ofEpochSecond(1735034500), 60),
    JumpExample(TypeId(21097), CharacterId(2120729046), 0.001, Instant.ofEpochSecond(1735084500), 40)
  )

/** Only for demo purposes, render various components
  */
object ControlsDemo:

  val now = EventStream.periodic(5_000).map(_ => Instant.now).startWith(Instant.now)

  val gridJumpsGrid = MiniGrid[JumpExample](
    GridConfig(
      pageSize = Some(2),
      initialSort = Some("Time", SortDirection.Desc),
      mods = List(cls := "grid-jumps-demo")
    ),
    Array(
      Column[JumpExample, TypeId](
        name = "Ship",
        nameEl = i(cls := "ti", cls := "ti-rocket", title := "Ship"),
        tpe = ColumnType.Number,
        select = _.shipType,
        view = (_, t) => ESI.typeIcon(t, size = 32),
        sortable = Some(Ordering.fromLessThan(_ < _))
      ),
      Column[JumpExample, CharacterId](
        name = "Character",
        nameEl = i(cls := "ti", cls := "ti-user", title := "Character"),
        tpe = ColumnType.Number,
        select = _.characterId,
        view = (_, t) => ESI.characterImage(t, ""),
        sortable = Some(Ordering.fromLessThan(_ < _))
      ),
      Column[JumpExample, Double](
        name = "Mass",
        nameEl = i(cls := "ti", cls := "ti-weight", title := "Ship Mass"),
        tpe = ColumnType.Number,
        select = _.massPoints,
        view = (_, t) => span(t),
        sortable = Some(Ordering[Double])
      ),
      Column[JumpExample, Instant](
        name = "Time",
        nameEl = i(cls := "ti", cls := "ti-clock", title := "Timestamp"),
        tpe = ColumnType.DateTime,
        select = _.at,
        view = (_, at) => span(text <-- now.map(ts => s"${Duration.between(at, ts).toHours}h"), title := at.toString),
        sortable = Some(Ordering[Instant])
      ),
      Column[JumpExample, Int](
        name = "Note",
        nameEl = i(cls := "ti", cls := "ti-note", title := "Note"),
        tpe = ColumnType.Number,
        select = _.note,
        view = (_, t) => span(t),
        sortable = Some(Ordering[Int])
      )
    ),
    _.at.toEpochMilli.toString,
    data = Val(JumpExample.Samples)
  )

  def renderPage: HtmlElement = div(
    sectionTag(
      h2("Grid example"),
      gridJumpsGrid.view
    )
  )
