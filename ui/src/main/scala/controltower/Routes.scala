package controltower

import com.raquo.airstream.split.SplitMatchOneMacros.*
import com.raquo.laminar.api.L.*
import com.raquo.waypoint.*
import controltower.backend.ControlTowerBackend
import org.scalajs.dom
import org.getshaka.nativeconverter.fromJson

import java.time.Clock
import scala.language.adhocExtensions
import scala.util.{Failure, Success, Try}

object Routes:
  private val landingRoute   = Route.static[Page.Landing.type](Page.Landing, root)
  private val mapEditorRoute = Route.static[Page.MapEditor.type](Page.MapEditor, root / "maps")
  private val mapRoute       = Route[Page.Map, (String, String)](
    encode = m => (m.name, m.character),
    decode = (name, character) => Page.Map(name, character),
    pattern = root / "map" / segment[String] / "char" / segment[String] / endOfSegments
  )
  // test only
  private val controlsDemoRoute = Route.static[Page.ControlsDemo.type](Page.ControlsDemo, root / "controlsdemo")

  given ControlTowerBackend = new ControlTowerBackend()
  given Clock               = Clock.systemUTC()

  val view: Signal[HtmlElement] =
    JsRouter.currentPageSignal.splitMatchOne
      .handleType[Page.Map]((map, _) => page.MapPage.renderPage(map))
      .handleValue[Page.MapEditor.type](Page.MapEditor)(page.MapEditorPage.renderPage)
      .handleValue[Page.ControlsDemo.type](Page.ControlsDemo)(page.ControlsDemo.renderPage)
      .handleValue[Page.Landing.type](Page.Landing)(page.LandingPage.renderPage)
      .toSignal

  object JsRouter
      extends Router[Page](
        routes = List(
          mapRoute,
          mapEditorRoute,
          controlsDemoRoute,
          landingRoute
        ), // landing route is catch-all so has to be last
        serializePage = _.toJson,
        deserializePage = _.fromJson[Page],
        getPageTitle = _.pageTitle
      )

  // Note: this returns a modifier that you need to hang off a Laminar element,
  // e.g. `a(navigateTo(HomePage), "Back to Home")`
  // See https://github.com/raquo/Waypoint docs for why this modifier is useful in general.
  // Note: for fragment ('#') URLs this isn't actually needed.
  def navigateTo(page: Page): Binder[HtmlElement] = Binder: el =>
    val isLinkElement = el.ref.isInstanceOf[dom.html.Anchor]

    if (isLinkElement)
      Try(JsRouter.absoluteUrlForPage(page)) match
        case Success(url) => el.amend(href(url))
        case Failure(err) => dom.console.error(err)

    // If element is a link and user is holding a modifier while clicking:
    //  - Do nothing, browser will open the URL in new tab / window / etc. depending on the modifier key
    // Otherwise:
    //  - Perform regular pushState transition
    //  - Scroll to top of page

    val onRegularClick = onClick
      .filter(ev => !(isLinkElement && (ev.ctrlKey || ev.metaKey || ev.shiftKey || ev.altKey)))
      .preventDefault

    (onRegularClick --> { _ =>
      JsRouter.pushState(page)
      dom.window.scrollTo(0, 0) // Scroll to top of page when navigating
    }).bind(el)
