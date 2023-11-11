package controltower.backend

import org.scalajs.dom.RequestCredentials
import org.updraft0.controltower.protocol.{Endpoints, SessionCookie, UserInfo, MapInfo, NewMap}
import sttp.client3.{FetchBackend, FetchOptions, UriContext}
import sttp.model.Uri
import sttp.tapir.client.sttp.SttpClientInterpreter
import sttp.tapir.Endpoint

import scala.concurrent.Future

class ControlTowerBackend(backendUrlOpt: Option[Uri] = Some(uri"http://localhost:8080") /* FIXME */ ):
  private val fetchBackend = FetchBackend(FetchOptions(Some(RequestCredentials.include), None))

  // note: not sure if it's possible to get rid of this, basically sttp cookies are ignored as they are sent by browser
  //       outside of JS control
  private val dummyCookie = SessionCookie("dummy")

  // region endpoints
  val getUserInfo: () => Future[Either[String, UserInfo]] = () => callSecure(Endpoints.getUserInfo)(())

  val createMap: NewMap => Future[Either[String, MapInfo]] = newMap => callSecure(Endpoints.createMap)(newMap)
  // endregion

  val loginUrl = backendUrlOpt.map(u => uri"$u/api/auth/login").getOrElse(uri"api/auth/login") // FIXME hardcoded url

  def callSecure[I, E, O](ep: Endpoint[SessionCookie, I, E, O, Any]): I => Future[Either[E, O]] =
    val base = SttpClientInterpreter()
      .toSecureClientThrowDecodeFailures(ep, backendUrlOpt, fetchBackend)
      .apply(dummyCookie)
    (in: I) => base.apply(in)
