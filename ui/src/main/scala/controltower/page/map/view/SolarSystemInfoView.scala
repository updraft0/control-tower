package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.backend.{ESI, ThirdParty}
import controltower.ui.ViewController
import org.updraft0.controltower.constant.SpaceType
import org.updraft0.controltower.protocol.*

case class SystemInfo(systemId: Long, name: Option[String])

/** Basic solar system information panel
  */
class SolarSystemInfoView(staticData: SystemStaticData, selectedSystem: Observable[Option[SystemInfo]])
    extends ViewController:

  override def view =
    table(
      cls := "solar-system-info-view",
      cls := "left-sidebar-view",
      children <-- selectedSystem.map {
        case Some(info) if staticData.solarSystemMap.contains(info.systemId) =>
          solarSystemInfo(staticData.solarSystemMap(info.systemId), info.name, staticData.wormholeTypes)
        case _ => nodeSeq(tr("select a system"))
      }
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
      solarSystem.wormholeStatics.map { static =>
        val whType = wormholeTypes(static.typeId)
        mark(
          cls := "system-wormhole-static",
          cls := s"system-wormhole-static-${static.name.toLowerCase}",
          cls := s"system-class-${whType.targetClass.toString.toLowerCase}",
          static.name,
          i(cls := "ti", cls := "ti-arrow-narrow-right"),
          whType.targetClass.toString
        )
      }
    )
  )
