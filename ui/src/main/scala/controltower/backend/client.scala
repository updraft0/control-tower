package controltower.backend

import com.raquo.airstream.core.{EventStream, Observable}
import com.raquo.airstream.ownership.Owner
import controltower.backend.given
import org.scalajs.dom.RequestCredentials
import org.updraft0.controltower.protocol.*
import sttp.capabilities.WebSockets
import sttp.client3.{FetchBackend, FetchOptions, SttpBackend, UriContext}
import sttp.model.Uri
import sttp.tapir.Endpoint
import sttp.tapir.client.sttp.{SttpClientInterpreter, WebSocketToPipe}

import scala.concurrent.Future

class ControlTowerBackend(
    backendUrlOpt: Option[Uri] = Some(uri"http://localhost:8080"),
    wsUrlOpt: Option[Uri] = Some(uri"ws://localhost:8080") /* FIXME */
):
  private val fetchBackend = FetchBackend(FetchOptions(Some(RequestCredentials.include), None))
    .asInstanceOf[SttpBackend[Future, AirstreamStreams & WebSockets]]

  // note: not sure if it's possible to get rid of this, basically sttp cookies are ignored as they are sent by browser
  //       outside of JS control
  private val dummyCookie = SessionCookie("dummy")

  // region endpoints
  val getUserInfo: () => Future[Either[String, UserInfo]] = () => callSecure(Endpoints.getUserInfo)(())

  val createMap: NewMap => Future[Either[String, MapInfo]] = newMap => callSecure(Endpoints.createMap)(newMap)
  // endregion

  def mapWebsocket(
      mapName: String,
      characterId: Long
  )(using owner: Owner): Future[Either[String, Observable[MapRequest] => EventStream[MapMessage]]] =
    given WebSocketToPipe[AirstreamStreams & WebSockets] = new WebSocketToAirstream[AirstreamStreams & WebSockets]
    SttpClientInterpreter()
      .toSecureClientThrowDecodeFailures(Endpoints.mapWebSocket(using AirstreamStreams), wsUrlOpt, fetchBackend)
      .apply(dummyCookie)
      .apply((mapName, characterId))

  val loginUrl = backendUrlOpt.map(u => uri"$u/api/auth/login").getOrElse(uri"api/auth/login") // FIXME hardcoded url

  def callSecure[I, E, O](
      ep: Endpoint[SessionCookie, I, E, O, Any]
  ): I => Future[Either[E, O]] =
    val base = SttpClientInterpreter()
      .toSecureClientThrowDecodeFailures(ep, backendUrlOpt, fetchBackend)
      .apply(dummyCookie)
    (in: I) => base.apply(in)
