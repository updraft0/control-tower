package org.updraft0.controltower.server.tracking

import org.updraft0.controltower.server.Log
import org.updraft0.esi.client.{EsiClient, EsiError, ServerStatusResponse}
import zio.*

trait ServerStatusTracker:
  def status: UIO[Either[EsiError, ServerStatusResponse]]

object ServerStatusTracker:
  type Env = EsiClient

  // Every poll interval, update status
  private val PollInterval = 30.seconds

  def layer: ZLayer[Env, Nothing, ServerStatusTracker] = ZLayer.scoped(apply())

  def apply(): ZIO[Scope & Env, Nothing, ServerStatusTracker] =
    for
      ref <- Ref.make[Either[EsiError, ServerStatusResponse]](
        Left(EsiError.ServiceUnavailable("Internal tracker error"))
      )
      // refresh once
      _ <- doRefresh(ref)
      // fork in background
      _ <- (doRefresh(ref)
        .repeat(Schedule.once.andThen(Schedule.fixed(PollInterval)))
        .ignoreLogged @@ Log.BackgroundOperation("statusTracker")).forkScoped
    yield new ServerStatusTracker:
      override def status: UIO[Either[EsiError, ServerStatusResponse]] = ref.get

  private def doRefresh(ref: Ref[Either[EsiError, ServerStatusResponse]]) =
    ZIO
      .serviceWithZIO[EsiClient](_.getServerStatus(()).either)
      .flatMap(ref.set(_))
