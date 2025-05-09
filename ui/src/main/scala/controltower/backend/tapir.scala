package controltower.backend

import com.raquo.airstream.core.*
import com.raquo.airstream.ownership.Owner
import sttp.capabilities.{Streams, WebSockets}
import sttp.tapir.client.sttp.WebSocketToPipe
import sttp.tapir.model.WebSocketFrameDecodeFailure
import sttp.tapir.{DecodeResult, WebSocketBodyOutput}
import sttp.ws.{WebSocket, WebSocketClosed, WebSocketFrame}

import scala.annotation.unused
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.adhocExtensions
import scala.reflect.TypeTest
import scala.util.{Failure, Success, Try}

trait AirstreamStreams extends Streams[AirstreamStreams]:
  override type BinaryStream = Observable[Byte]
  override type Pipe[A, B]   = Observable[A] => EventStream[B]

object AirstreamStreams extends AirstreamStreams

// TODO: add concept of closed (again, a parameter? sigh)
class WebSocketToAirstream[R <: AirstreamStreams & WebSockets](using owner: Owner) extends WebSocketToPipe[R]:
  override type S    = AirstreamStreams
  override type F[X] = Future[X]

  override def apply[REQ, RESP](s: Any)(
      ws: WebSocket[F],
      o: WebSocketBodyOutput[Any, REQ, RESP, ?, AirstreamStreams]
  ): Any =
    (in: Observable[REQ]) =>
      @unused var isClosed = false
      val sends = in
        .foreach { v => // this starts the subscription, otherwise nothing gets run
          Future
            .fromTry(Try(o.requests.encode(v)))
            .flatMap(ws.send(_, isContinuation = false))
            .onComplete(
              _.fold(
                _ => org.scalajs.dom.console.error("failed to send ws, TODO"), // FIXME
                _ => ()
              )
            )
        }

      def decode(frame: WebSocketFrame): Try[Option[RESP]] =
        o.responses.decode(frame) match
          case failure: DecodeResult.Failure => Failure(new WebSocketFrameDecodeFailure(frame, failure))
          case DecodeResult.Value(v)         => Success(Some(v))

      def raiseBadAccumulator[T](acc: WebSocketFrame, current: WebSocketFrame): Try[Nothing] =
        Failure(
          new WebSocketFrameDecodeFailure(
            current,
            DecodeResult.Error(
              "Bad frame sequence",
              new Exception(
                s"Invalid accumulator frame: $acc, it can't be concatenated with $current"
              )
            )
          )
        )

      def concatOrDecode[A <: WebSocketFrame](acc: Option[WebSocketFrame], frame: A, last: Boolean)(
          f: (A, A) => A
      )(using TypeTest[WebSocketFrame, A]): Try[(Option[WebSocketFrame], Either[Unit, Option[RESP]])] =
        if (last)
          (acc match
            case None       => decode(frame).map(Right(_))
            case Some(x: A) => decode(f(x, frame)).map(Right(_))
            case Some(bad)  => raiseBadAccumulator(bad, frame)
          ).map(None -> _)
        else
          (acc match
            case None       => Success(Some(frame))
            case Some(x: A) => Success(Some(f(x, frame)))
            case Some(bad)  => raiseBadAccumulator(bad, frame)
          ).map(acc => acc -> Left(()))

      val es = FutureEventStreamSourceAccumM(
        ws.receive(),
        { ws.close(); sends.kill() },
        Option.empty[WebSocketFrame],
        {
          case (acc, _: WebSocketFrame.Close) if !o.decodeCloseResponses =>
            Future.successful(acc -> Right(None))
          case (acc, _: WebSocketFrame.Pong) if o.ignorePong =>
            Future.successful(acc -> Left(()))
          case (acc, WebSocketFrame.Ping(p)) if o.autoPongOnPing =>
            ws.send(WebSocketFrame.Pong(p)).map(_ => acc -> Left(()))
          case (prev, frame @ WebSocketFrame.Text(_, last, _)) =>
            Future.fromTry(concatOrDecode(prev, frame, last)((l, r) => r.copy(payload = l.payload + r.payload)))
          case (prev, frame @ WebSocketFrame.Binary(_, last, _)) =>
            Future.fromTry(concatOrDecode(prev, frame, last)((l, r) => r.copy(payload = l.payload ++ r.payload)))
          case (_, frame) =>
            Future.failed(
              new WebSocketFrameDecodeFailure(
                frame,
                DecodeResult.Error(
                  "Unrecognised frame type",
                  new Exception(s"Unrecognised frame type: ${frame.getClass}")
                )
              )
            )
        },
        {
          case _: WebSocketClosed =>
            isClosed = true
            sends.kill()
            true
          case _ => false
        }
      ).collect { case Right(Some(v)) =>
        v
      }: EventStream[RESP]

      es

object FutureEventStreamSourceAccumM:
  import scala.concurrent.ExecutionContext.Implicits.global

  def apply[I, S, O](
      f: => Future[I],
      close: => Unit,
      initial: S,
      accum: (S, I) => Future[(S, O)],
      isClosed: Throwable => Boolean
  ): EventStream[O] =
    EventStream.fromCustomSource(
      shouldStart = _ == 1,
      start = (fireValue, fireError, _, _) => {
        def loop(s: S): Unit =
          f.onComplete(
            _.fold(
              ex => if (!isClosed(ex)) fireError(ex),
              i =>
                accum(s, i).onComplete(
                  _.fold(ex => if (!isClosed(ex)) fireError(ex), (n, o) => { fireValue(o); loop(n) })
                )
            )
          )
        loop(initial)
      },
      stop = _ => close
    )
