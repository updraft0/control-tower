package controltower

import com.raquo.laminar.api.L.*
import com.raquo.waypoint.*
import controltower.backend.ControlTowerBackend
import org.scalajs.dom
import org.getshaka.nativeconverter.fromJson

import java.time.Clock
import scala.util.{Failure, Success, Try}

object Routes:
  private val landingRoute   = Route.static(Page.Landing, root)
  private val mapEditorRoute = Route.static(Page.MapEditor, root / "maps")
  private val mapRoute = Route[Page.Map, (String, String)](
    encode = m => (m.name, m.character),
    decode = (name, character) => Page.Map(name, character),
    pattern = root / "map" / segment[String] / "char" / segment[String] / endOfSegments
  )

  given ControlTowerBackend = new ControlTowerBackend()
  given Clock               = Clock.systemUTC()

  val router = new Router[Page](
    routes = List(mapRoute, mapEditorRoute, landingRoute),
    getPageTitle = _.pageTitle,
    serializePage = _.toJson,
    deserializePage = _.fromJson[Page]
  )(
    popStateEvents = windowEvents(_.onPopState),
    owner = unsafeWindowOwner
  )

  private val splitter =
    SplitRender[Page, HtmlElement](router.currentPageSignal)
      .collectStatic(Page.Landing)(page.LandingPage.renderPage)
      .collectStatic(Page.MapEditor)(page.MapEditorPage.renderPage)
      .collect[Page.Map](page.MapPage.renderPage)

  val view: Signal[HtmlElement] = splitter.signal

  // Note: this returns a modifier that you need to hang off a Laminar element,
  // e.g. `a(navigateTo(HomePage), "Back to Home")`
  // See https://github.com/raquo/Waypoint docs for why this modifier is useful in general.
  // Note: for fragment ('#') URLs this isn't actually needed.
  def navigateTo(page: Page): Binder[HtmlElement] = Binder: el =>
    val isLinkElement = el.ref.isInstanceOf[dom.html.Anchor]

    if (isLinkElement)
      Try(router.absoluteUrlForPage(page)) match
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
      router.pushState(page)
      dom.window.scrollTo(0, 0) // Scroll to top of page when navigating
    }).bind(el)
