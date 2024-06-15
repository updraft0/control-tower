package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.backend.{ESI, ThirdParty}
import controltower.ui.ViewController
import org.updraft0.controltower.constant.{SpaceType, WormholeClass}
import org.updraft0.controltower.protocol.*

case class SystemInfo(systemId: Long, name: Option[String])

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
          solarSystemInfo(staticData.solarSystemMap(info.systemId), info.name, staticData.wormholeTypes)
        case _ => nodeSeq(tr("select a system"))
      })
    )

private val IgnoredSpaceTypes = Set(SpaceType.Known, SpaceType.Pochven)

private inline def solarSystemInfo(
    solarSystem: SolarSystem,
    name: Option[String],
    wormholeTypes: Map[Long, WormholeType]
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
            ESI.typeIcon(starTypeId)
          )
        ),
      solarSystem.effect
        .map(effect =>
          mark(
            cls := "system-effect",
            cls := s"system-effect-${effect.toString.toLowerCase}",
            i(cls := "ti", cls := "ti-square-filled")
          )
        ),
      mark(
        cls := "system-planets",
        i(cls := "ti", cls := "ti-globe"),
        s" ${solarSystem.planets.size}"
      ),
      // TODO add system moons (requires a query change on backend)
//          mark(
//            cls := "system-moons",
//            i(cls := "ti", cls := "ti-moon-filled"),
//            s" ${solarSystem.planets.map(_.moonCount).sum}"
//          )
      Option
        .when(!solarSystem.systemClass.forall(_.isDrifter))(
          solarSystem.wormholeStatics.flatMap(staticInfo(wormholeTypes, _)).toSeq
        )
        .getOrElse(nodeSeq())
    )
  )

private inline def staticInfo(wormholeTypes: Map[Long, WormholeType], static: WormholeStatic) =
  val whType = wormholeTypes(static.typeId)
  nodeSeq(
    mark(
      idAttr                   := s"static-${static.typeId}",
      cls                      := "system-wormhole-static",
      dataAttr("static-name")  := static.name,
      dataAttr("static-class") := whType.targetClass.toString,
      styleAttr                := s"anchor-name: --static-${static.typeId}",
      // TODO - move away from this CSS class
      cls := s"system-class-${whType.targetClass.toString.toLowerCase}",
      static.name,
      i(cls := "ti", cls := "ti-arrow-narrow-right"),
      whType.targetClass.toString
    ),
    div(
      cls := "tooltip",
      cls := "wormhole-static-tooltip",
      // FIXME - the anchor position polyfill is not dynamic and does not support vars - aka no Firefox or Safari
      styleAttr := s"--anchor-var: --static-${static.typeId}",
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
  )
