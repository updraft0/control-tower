package org.updraft0.esi.client

import sttp.client3.httpclient.zio.{HttpClientZioBackend, SttpClient}
import sttp.model.{Uri, HeaderNames}
import sttp.tapir.*
import sttp.tapir.client.sttp.SttpClientInterpreter
import zio.*
import zio.stream.ZStream

import sttp.capabilities.zio.ZioStreams

/** Client to download the latest Static Data Export
  */
trait SdeClient:
  def latestChecksum: Task[String]
  def sdeSize: Task[Long]
  def sdeZip: Task[ZStream[Any, Throwable, Byte]]

object SdeClient:

  case class Config(base: Uri)

  private val sdeChecksum = endpoint.get
    .in("tranquility" / "checksum")
    .out(stringBody)

  private val sdeZipEndpoint = endpoint.get
    .in("tranquility" / "sde.zip")
    .out(streamBinaryBody(ZioStreams)(CodecFormat.OctetStream()))

  private val sdeZipHeaders = endpoint.head
    .in("tranquility" / "sde.zip")
    .out(headers)

  def apply(base: Uri, sttp: SttpClient, interp: SttpClientInterpreter): SdeClient =
    new SdeClient:
      override def latestChecksum: Task[String] =
        ZIO.suspend(interp.toClientThrowErrors(sdeChecksum, Some(base), sttp).apply(()))

      override def sdeSize: Task[Long] = ZIO
        .suspend(interp.toClientThrowErrors(sdeZipHeaders, Some(base), sttp).apply(()))
        .flatMap(
          _.find(_.name == HeaderNames.ContentLength)
            .map(h => ZIO.succeed(h.value.toLong))
            .getOrElse(ZIO.dieMessage("No headers - is internet working?"))
        )

      override def sdeZip: Task[ZStream[Any, Throwable, Byte]] =
        ZIO.suspend(interp.toClientThrowErrors(sdeZipEndpoint, Some(base), sttp).apply(()))

  def layer: ZLayer[Config, Throwable, SdeClient] =
    ZLayer.scoped(HttpClientZioBackend.scoped().flatMap(apply))

  def apply(sttp: SttpClient): ZIO[Config, Throwable, SdeClient] =
    for config <- ZIO.service[Config]
    // FIXME
//      sttp   <- ZIO.service[SttpClient]
    yield apply(config.base, zioLoggingBackend(sttp), SttpClientInterpreter())
