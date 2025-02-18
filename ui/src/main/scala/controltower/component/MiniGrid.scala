package controltower.component

import com.raquo.laminar.api.L.*

import scala.reflect.ClassTag

enum ColumnType:
  case Text, DateTime, Number

case class Column[R, A](
    name: String,
    nameEl: HtmlElement,
    tpe: ColumnType,
    select: R => A,
    view: (R, A) => HtmlElement,
    sortable: Option[Ordering[A]] = None
)

// TODO ergonomics
//extension [R](r: R)(using ValueOf[R])
//  def column[A](select: R => A)(name: String, nameEl: HtmlElement, tpe: ColumnType, view: A => HtmlElement, sortable: Boolean = false) =
//    Column[R, A](name, nameEl, tpe, select, view, sortable)

case class GridConfig(
    pageSize: Option[Int] = None,
    initialSort: Option[(String, SortDirection)] = None,
    overrideSort: Option[(String, SortDirection)] = None,
    toolbarMods: Seq[HtmlMod] = Nil,
    mods: Seq[HtmlMod] = Nil
)

enum SortDirection derives CanEqual:
  case Asc, Desc

  def invert: SortDirection = this match
    case Asc  => Desc
    case Desc => Asc

/** A simplified grid that allows e.g. sorting by column
  *
  * @see
  *   https://www.w3.org/WAI/ARIA/apg/patterns/table/examples/sortable-table/
  */
final class MiniGrid[R: ClassTag](
    config: GridConfig,
    columns: Array[Column[R, ?]],
    key: R => String,
    data: Signal[Array[R]],
    rowMods: R => Seq[HtmlMod] = (_: R) => Nil
):

//  private val pageWindow = Var((-1, -1))

  private val pageSize = config.pageSize.getOrElse(10)
  private val overrideSortedBy: Option[(Column[R, ?], SortDirection)] = config.overrideSort.map:
    case (colName, dir) => columns.find(_.name == colName).get -> dir
  private val sortedBy: Var[Option[(Column[R, ?], SortDirection)]] = Var(config.initialSort.map:
    case (colName, dir) => columns.find(_.name == colName).get -> dir)
  private val dataArr     = Var(Array.empty[R])
  private val currentPage = Var(config.pageSize.map(_ => 0)).distinct
  private val dataArrPaged = dataArr.signal
    .combineWith(currentPage)
    .map:
      case (arr, None) => arr
      case (arr, Some(page)) =>
        arr.slice(Math.min(arr.length, pageSize * page), Math.min(arr.length, pageSize * (page + 1)))

  def view: Element =
    table(
      data
        .combineWith(sortedBy)
        .map {
          case (arr, None) => arr
          case (arr, Some((col, direction))) =>
            overrideSortedBy match
              case Some((overrideCol, overrideDir)) =>
                arr.sortWith((a, b) => orderWithOverrideCol(overrideCol, overrideDir, col, direction, a, b))
              case None => arr.sortWith((a, b) => orderWithCol(col, direction, a, b))
        }
        .combineWith(currentPage) --> ((data, pageOpt) =>
        Var.set(
          dataArr -> data,
          currentPage -> pageOpt.map: page =>
            if (page * pageSize > data.length) data.length / pageSize else page
        )
      ),
      cls := "mini-grid",
      config.mods,
      header,
      rows
    )

  private inline def orderWithCol(col: Column[R, ?], direction: SortDirection, a: R, b: R) =
    val ac  = col.select(a)
    val bc  = col.select(b)
    val res = col.sortable.get.compare(ac, bc)
    if (direction == SortDirection.Asc) res < 0
    else res >= 0

  private inline def orderWithOverrideCol(
      overrideCol: Column[R, ?],
      overrideDir: SortDirection,
      col: Column[R, ?],
      direction: SortDirection,
      a: R,
      b: R
  ) =
    val aoc = overrideCol.select(a)
    val boc = overrideCol.select(b)
    val res = overrideCol.sortable.get.compare(aoc, boc)
    if (res == 0) orderWithCol(col, direction, a, b)
    else if (overrideDir == SortDirection.Asc) res < 0
    else res >= 0

  private inline def header =
    thead(
      tr(
        cls := "toolbar",
        td(colSpan := columns.length, tableToolbar)
      ),
      tr(
        columns.map(headerColumn)
      )
    )

  private inline def tableToolbar =
    div(
      cls := "toolbar",
      span(
        cls := "size",
        text <-- currentPage.signal
          .combineWith(dataArr)
          .map:
            case (None, arr) => s"${Math.min(1, arr.length)} - ${arr.length} of ${arr.length}"
            case (Some(page), arr) =>
              s"${Math.min(1, arr.length) + (page * pageSize)}  - ${Math.min(arr.length, pageSize * (page + 1))} of ${arr.length}"
      ),
      button(
        cls     := "prev-page",
        display := config.pageSize.map(_ => "").getOrElse("none"),
        disabled <-- currentPage.signal.map:
          case None | Some(0) => true
          case _              => false
        ,
        i(cls := "ti", cls := "ti-caret-left-filled"),
        onClick.compose(
          _.mapToUnit
            .withCurrentValueOf(currentPage)
            .map:
              case Some(prev) => Some(prev - 1)
              case None       => None
        ) --> currentPage
      ),
      button(
        cls     := "next-page",
        display := config.pageSize.map(_ => "").getOrElse("none"),
        disabled <-- currentPage.signal
          .combineWith(dataArr)
          .map:
            case (None, _)         => true
            case (Some(page), arr) => (page + 1) * pageSize >= arr.length
        ,
        i(cls := "ti", cls := "ti-caret-right-filled"),
        onClick.compose(
          _.mapToUnit
            .withCurrentValueOf(currentPage)
            .map:
              case Some(prev) => Some(prev + 1)
              case None       => None
        ) --> currentPage
      ),
      config.toolbarMods
    )

  private inline def rows =
    tbody(
      children <-- dataArrPaged.signal.map(_.map(renderRow))
    )

  private inline def renderRow(r: R) =
    tr(
      dataAttr("key") := key(r),
      columns.map(c => renderColumn(c, r)),
      rowMods(r)
    )

  private inline def renderColumn(c: Column[R, ?], r: R) =
    td(
      dataAttr("tpe") := c.tpe.toString,
      dataAttr("col") := c.name,
      c.view(r, c.select(r))
    )

  private inline def headerColumn(c: Column[R, ?]) =
    if (c.sortable.isEmpty || config.overrideSort.exists(_._1 == c.name))
      th(
        cls             := "no-sort",
        dataAttr("col") := c.name,
        dataAttr("tpe") := c.tpe.toString,
        c.nameEl
      )
    else
      th(
        aria.sort <-- sortedBy.signal.map:
          case Some((col, SortDirection.Asc)) if col.name == c.name  => "ascending"
          case Some((col, SortDirection.Desc)) if col.name == c.name => "descending"
          case _                                                     => "none"
        ,
        dataAttr("col") := c.name,
        dataAttr("tpe") := c.tpe.toString,
        button(
          onClick.compose(_.mapToUnit.withCurrentValueOf(sortedBy).map {
            case Some((col, d)) if col.name == c.name => Some((col, d.invert))
            case _                                    => Some((c, SortDirection.Asc))
          }) --> sortedBy,
          c.nameEl,
          span(
            cls         := "ti",
            aria.hidden := true
          )
        )
      )
