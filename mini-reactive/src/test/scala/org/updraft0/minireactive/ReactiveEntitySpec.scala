package org.updraft0.minireactive

import zio.*
import zio.stream.ZStream
import zio.test.*

object ReactiveEntitySpec extends ZIOSpecDefault:

  enum CounterMessage:
    case Get
    case Incr

  import CounterMessage.*

  enum CounterReply:
    case Current(value: Int)

  object CounterEntity extends ReactiveEntity[Any, String, Int, CounterMessage, CounterReply]:
    override def tag                                               = "counter"
    override def hydrate(key: String, in: Enqueue[CounterMessage]) = ZIO.succeed(0)
    override def handle(key: String, state: Int, in: CounterMessage) =
      in match
        case Get  => ZIO.succeed(state -> Chunk.succeed(CounterReply.Current(state)))
        case Incr => ZIO.succeed(state + 1 -> Chunk.empty)

  override def spec =
    suite("counter reactive entity")(
      test("can respond to a sequence of get/increment messages"):
        for
          entity <- MiniReactive(CounterEntity, MiniReactiveConfig(16, Duration.Infinity))
          inQ    <- entity.enqueue("test")
          outQ   <- entity.subscribe("test")
          _      <- ZStream(Get, Incr, Get, Incr, Incr, Get, Incr, Get).mapZIO(inQ.offer).runDrain.forkScoped
          res    <- ZStream.fromQueue(outQ).take(4).runCollect
        yield assertTrue(res == Chunk(0, 1, 3, 4).map(CounterReply.Current.apply))
    )
