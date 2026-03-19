package org.updraft0.esi.client

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import sttp.capabilities.zio.ZioStreams
import sttp.client3.httpclient.zio.{HttpClientZioBackend, SttpClient}
import sttp.model.{HeaderNames, Uri}
import sttp.tapir.*
import sttp.tapir.client.sttp.SttpClientInterpreter
import sttp.tapir.json.jsoniter.*
import zio.*
import zio.stream.ZStream

import java.time.Instant

case class SdeVersion(buildNumber: Long, releaseDate: Instant)

/** Client to download the latest Static Data Export
  */
trait SdeClient:
  def latestVersion: Task[SdeVersion]
  def sdeSize(version: SdeVersion): Task[Long]
  def sdeZip(version: SdeVersion): Task[ZStream[Any, Throwable, Byte]]

object SdeClient:

  // simple codecs
  given JsonValueCodec[SdeVersion] = JsonCodecMaker.make
  given Schema[SdeVersion]         = Schema.derived

  case class Config(base: Uri)

  private val sdeBuild = endpoint.get
    .in("tranquility" / "latest.jsonl")
    .out(jsonBody[SdeVersion])

  private def sdeDownloadEndpoint(version: Long) = endpoint
    .in("tranquility" / s"eve-online-static-data-${version}-jsonl.zip")

  private def sdeDownload(version: Long) =
    sdeDownloadEndpoint(version).get
      .out(streamBinaryBody(ZioStreams)(CodecFormat.OctetStream()))

  private def sdeHeaders(version: Long) =
    sdeDownloadEndpoint(version).head
      .out(headers)

  def apply(base: Uri, sttp: SttpClient, interp: SttpClientInterpreter): SdeClient =
    new SdeClient:
      override def latestVersion: Task[SdeVersion] =
        ZIO.suspend(interp.toClientThrowErrors(sdeBuild, Some(base), sttp).apply(()))

      override def sdeSize(version: SdeVersion): Task[Long] = ZIO
        .suspend(interp.toClientThrowErrors(sdeHeaders(version.buildNumber), Some(base), sttp).apply(()))
        .flatMap(
          _.find(_.name == HeaderNames.ContentLength)
            .map(h => ZIO.succeed(h.value.toLong))
            .getOrElse(ZIO.dieMessage("No headers - is internet working?"))
        )

      override def sdeZip(version: SdeVersion): Task[ZStream[Any, Throwable, Byte]] =
        ZIO.suspend(interp.toClientThrowErrors(sdeDownload(version.buildNumber), Some(base), sttp).apply(()))

  def layer: ZLayer[Config, Throwable, SdeClient] =
    ZLayer.scoped(HttpClientZioBackend.scoped().flatMap(apply))

  def apply(sttp: SttpClient): ZIO[Config, Throwable, SdeClient] =
    for config <- ZIO.service[Config]
    // FIXME
//      sttp   <- ZIO.service[SttpClient]
    yield apply(config.base, zioLoggingBackend(sttp), SttpClientInterpreter())
