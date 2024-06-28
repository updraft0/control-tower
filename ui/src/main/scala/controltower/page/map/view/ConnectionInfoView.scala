package controltower.page.map.view

import com.raquo.laminar.api.L.*
import com.raquo.laminar.codecs.StringAsIsCodec
import controltower.backend.ESI
import controltower.ui.*
import org.updraft0.controltower.protocol.*

val scopeAttr = htmlAttr("scope", StringAsIsCodec)

final class ConnectionInfoView(selected: Signal[Option[MapWormholeConnectionWithSigs]])(using ctx: MapViewContext)
    extends ViewController:
  override def view: Element = div(
    idAttr := "connection-info-view",
    cls    := "connection-info-view",
    cls    := "left-sidebar-view",
    hideIfEmptyOpt(selected),
    children <-- selected.splitOption((_, s) => connectionInfo(s), nodeSeq())
  )

  private def connectionInfo(s: Signal[MapWormholeConnectionWithSigs]) =

    val fromSystem = s.map(whcs => ctx.staticData.solarSystemMap(whcs.connection.fromSystemId))
    val toSystem   = s.map(whcs => ctx.staticData.solarSystemMap(whcs.connection.toSystemId))

    val fromName = s.flatMapSwitch(whcs =>
      ctx
        .systemName(whcs.connection.fromSystemId)
        .map(_.getOrElse(ctx.staticData.solarSystemMap(whcs.connection.fromSystemId).name))
    )
    val toName = s.flatMapSwitch(whcs =>
      ctx
        .systemName(whcs.connection.toSystemId)
        .map(_.getOrElse(ctx.staticData.solarSystemMap(whcs.connection.toSystemId).name))
    )

    // TODO: incorporate these
//    val whMassSize   = s.map(getWormholeMassSize)
//    val whMassStatus = s.map(getWormholeMassStatus)
//    val whType       = s.map(getWormholeClass)

    nodeSeq(
      div(
        cls := "connection-info",
        span(cls := "info-title", "Connection"),
        span(
          cls := "from-system",
          mark(
            cls := "system-class",
            cls <-- fromSystem.map(ss => s"system-class-${ss.systemClass.get.toString.toLowerCase}"),
            child.text <-- fromSystem.map(_.systemClass.get.tag)
          ),
          " ",
          child.text <-- fromName
        ),
        i(cls := "ti", cls := "ti-arrows-exchange-2"),
        span(
          cls := "to-system",
          mark(
            cls := "system-class",
            cls <-- toSystem.map(ss => s"system-class-${ss.systemClass.get.toString.toLowerCase}"),
            child.text <-- toSystem.map(_.systemClass.get.tag)
          ),
          " ",
          child.text <-- toName
        ),
        span(
          cls := "connection-status"
          // TODO - implement this
        ),
        span(
          cls := "connection-mass",
          "Σ ",
          child.text <-- s.map(whcs => pointsFromMass(whcs.jumps.view.map(jumpMass).sum).toString)
        ),
        span(
          cls := "connection-created",
          child.text <-- ctx.now.withCurrentValueOf(s).map((now, c) => timeDiffString(now, c.connection.createdAt))
        ),
        span(
          cls := "connection-created-by",
          child <-- s.map(c =>
            ESI.characterImage(c.connection.createdByCharacterId, "createdBy", size = CharacterImageSize)
          )
        )
      ),
      table(
        cls := "connection-jumps",
        thead(
          th(cls := "connection-jump-ship", i(cls := "ti", cls := "ti-ship")),
          th(cls := "connection-jump-mass", "Mass"),
          th(cls := "connection-jump-time", i(cls := "ti", cls := "ti-clock-filled")),
          th(cls := "connection-jump-character", i(cls := "ti", cls := "ti-user"))
        ),
        tbody(
          children <-- s
            .map(_.jumps.toList)
            .split(identity): (_, _, cjs) =>
              tr(
                td(cls := "connection-jump-ship", child <-- cjs.map(j => ESI.typeIcon(j.shipTypeId))),
                td(
                  cls := "connection-jump-mass",
                  child.text <-- cjs.map(jumpMass).map(m => pointsFromMass(m).toString)
                ),
                td(
                  cls := "connection-jump-time",
                  child.text <-- ctx.now.withCurrentValueOf(cjs).map((now, j) => timeDiffString(now, j.createdAt))
                ),
                td(
                  cls := "connection-jump-character",
                  child <-- cjs.map(j => ESI.characterImage(j.characterId, "jumpedBy", size = CharacterImageSize))
                )
              )
        )
      )
    )

  private inline def jumpMass(j: MapWormholeConnectionJump): Long =
    j.massOverride.getOrElse(ctx.staticData.shipTypes.get(j.shipTypeId).map(_.mass).getOrElse(0L))
