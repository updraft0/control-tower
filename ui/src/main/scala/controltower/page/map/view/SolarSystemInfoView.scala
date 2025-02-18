package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.backend.{ESI, ThirdParty}
import controltower.ui.ViewController
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.*

case class SystemInfo(systemId: SystemId, name: Option[String])

/** Basic solar system information panel
  */
class SolarSystemInfoView(staticData: SystemStaticData, selectedSystem: Observable[Option[SystemInfo]])
    extends ViewController:

  override def view =
    div(
      idAttr := "solar-system-info-view",
      cls    := "solar-system-info-view",
      cls    := "left-sidebar-view",
      table(children <-- selectedSystem.map {
        case Some(info) if staticData.solarSystemMap.contains(info.systemId) =>
          solarSystemInfo(
            staticData.solarSystemMap(info.systemId),
            info.name,
            staticData.wormholeTypes,
            staticData.starTypes
          )
        case _ => nodeSeq(tr("select a system"))
      })
    )

private val IgnoredSpaceTypes = Set(SpaceType.Known, SpaceType.Pochven)

private inline def solarSystemInfo(
    solarSystem: SolarSystem,
    name: Option[String],
    wormholeTypes: Map[TypeId, WormholeType],
    starTypes: Map[TypeId, StarType]
) =
  nodeSeq(
    tr(
      cls := "solar-system-name",
      span(cls("solar-system-name-default")(name.isEmpty), name.getOrElse(solarSystem.name)),
      Option
        .when(solarSystem.systemClass.exists(_.spaceType == SpaceType.Wormhole))(
          ThirdParty.anoikisSystemLink(solarSystem.name)
        ),
      ThirdParty.dotlanSystemLink(solarSystem.name),
      ThirdParty.zkillSystemLink(solarSystem.id)
    ),
    tr(
      cls := "solar-system-location",
      Option.when(!solarSystem.systemClass.exists(wc => IgnoredSpaceTypes.contains(wc.spaceType)))(
        modSeq(
          span(solarSystem.systemClass.map(_.spaceType.toString).getOrElse("Unknown")),
          i(cls := "ti", cls := "ti-arrow-badge-right-filled")
        )
      ),
      span(solarSystem.regionName),
      i(cls := "ti", cls := "ti-arrow-badge-right-filled"),
      span(solarSystem.constellationName),
      i(cls := "ti", cls := "ti-arrow-badge-right-filled"),
      span(solarSystem.name)
    ),
    tr(
      cls := "solar-system-properties",
      mark(
        cls := "system-class",
        cls := s"system-class-${solarSystem.systemClass.get.toString.toLowerCase}",
        systemClassString(solarSystem.systemClass.get)
      ),
      mark(
        cls := "system-security",
        cls := s"system-class-${solarSystem.systemClass.get.toString.toLowerCase}",
        f"${solarSystem.security.getOrElse(-1.0)}%.1f"
      ),
      solarSystem.starTypeId
        .map(starTypeId =>
          mark(
            cls := "system-star",
            ESI
              .typeIcon(starTypeId, description = Some(starTypes(starTypeId).name))
              .amend(
                cls := "star-type"
              )
          )
        ),
      solarSystem.effect.map(effectInfo(solarSystem.systemClass.get, _)),
      mark(
        cls := "system-planets",
        i(cls := "ti", cls := "ti-globe"),
        s" ${solarSystem.planets.length}"
      ),
      // TODO add system moons (requires a query change on backend)
//          mark(
//            cls := "system-moons",
//            i(cls := "ti", cls := "ti-moon-filled"),
//            s" ${solarSystem.planets.map(_.moonCount).sum}"
//          )

      solarSystem.wormholeStatics.take(4).flatMap(staticInfo(wormholeTypes, _)).toSeq
    )
  )

private inline def effectInfo(wormholeClass: WormholeClass, effect: WormholeEffect) =
  nodeSeq(
    mark(
      cls       := "system-effect",
      cls       := "tooltip-target-adjacent",
      styleAttr := s"anchor-name: --system-effect-${effect.typeId}",
      i(cls := "ti", cls := "ti-square-filled", cls := s"system-effect-${effect.toString.toLowerCase}")
    ),
    effectTooltip(wormholeClass, effect, s"system-effect-${effect.typeId}")
  )

private inline def staticInfo(wormholeTypes: Map[TypeId, WormholeType], static: WormholeStatic) =
  val whType = wormholeTypes(static.typeId)
  nodeSeq(
    mark(
      idAttr                   := s"static-${static.typeId}",
      cls                      := "system-wormhole-static",
      dataAttr("static-name")  := static.name,
      dataAttr("static-class") := whType.targetClass.toString,
      cls                      := "tooltip-target-adjacent",
      styleAttr                := s"anchor-name: --static-${static.typeId}",
      // TODO - move away from this CSS class
      cls := s"system-class-${whType.targetClass.toString.toLowerCase}",
      static.name,
      i(cls := "ti", cls := "ti-arrow-narrow-right"),
      whType.targetClass.toString
    ),
    staticTooltip(whType, s"static-${static.typeId}")
  )

private[map] def staticTooltip(whType: WormholeType, anchor: String, anchorCls: String = "tooltip-on-left") =
  div(
    cls := "tooltip",
    cls := anchorCls,
    cls := "wormhole-static-tooltip",
    // FIXME - the anchor position polyfill is not dynamic and does not support vars - aka no Firefox or Safari
    styleAttr := s"--anchor-var: --${anchor}",
    h3(cls := "tooltip-title", whType.name),
    table(
      cls := "wormhole-static-info",
      tbody(
        tr(
          td("Points"),
          td(whType.stablePoints)
        ),
        tr(
          td("Lifetime"),
          td(s"${whType.maxStableTime / 60}h")
        ),
        tr(
          td("Size"),
          td(whType.massSize.toString)
        )
      )
    )
  )

private[map] def effectTooltip(whClass: WormholeClass, effect: WormholeEffect, anchor: String) =
  div(
    cls       := "tooltip",
    cls       := "tooltip-on-left",
    cls       := "system-effect-tooltip",
    styleAttr := s"--anchor-var: --${anchor}",
    h3(cls := "tooltip-title", s"${whClass.toString} ${effect.toString}")
    // TODO render effects!
  )
