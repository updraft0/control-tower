package org.updraft0.controltower.sde

import zio.stream.ZStream

/** Some of the entries in SDE (notably solar systems) require data from their "parents", so it makes sense to logically
  * group their import
  */
enum GroupedExport:
  case Ungrouped(value: ExportedData)
  case RegionSolarSystems(
      region: ExportedData.Region,
      constellation: ExportedData.Constellation,
      solarSystems: Vector[ExportedData.SolarSystem]
  )

private[sde] enum GrouperState:
  case Initial()
  case InRegion(region: ExportedData.Region)
  case InConstellation(
      region: ExportedData.Region,
      constellation: ExportedData.Constellation,
      solarSystems: Vector[ExportedData.SolarSystem]
  )

private[sde] def groupBySolarSystems[R](
    raw: ZStream[R, parser.Error, ExportedData]
): ZStream[R, parser.Error, GroupedExport] =
  raw
    .map(Some(_))
    .concat(ZStream.succeed(None))
    .mapAccum[GrouperState, Option[GroupedExport]](GrouperState.Initial()) {
      case (state: GrouperState.Initial, Some(region: ExportedData.Region)) =>
        GrouperState.InRegion(region) -> None
      case (GrouperState.InRegion(inRegion), Some(constellation: ExportedData.Constellation))
          if constellation.regionTag == inRegion.tag =>
        GrouperState.InConstellation(inRegion, constellation, Vector.empty) -> None
      case (in: GrouperState.InConstellation, Some(solarSystem: ExportedData.SolarSystem))
          if solarSystem.constellationTag == in.constellation.tag && solarSystem.regionTag == in.region.tag =>
        in.copy(solarSystems = in.solarSystems.appended(solarSystem)) -> None
      case (in: GrouperState.InConstellation, Some(constellation: ExportedData.Constellation))
          if constellation.regionTag == in.region.tag && constellation.tag != in.constellation.tag =>
        GrouperState.InConstellation(in.region, constellation, Vector.empty) -> emit(in)
      case (in: GrouperState.InConstellation, Some(region: ExportedData.Region)) if region.tag != in.region.tag =>
        GrouperState.InRegion(region) -> emit(in)
      case (_, Some(other)) =>
        GrouperState.Initial() -> Some(GroupedExport.Ungrouped(other))
      case (state, None) =>
        GrouperState.Initial() -> emit(state)
    }
    .filterNot(_.isEmpty)
    .map(_.get)

private[sde] def emit(state: GrouperState): Option[GroupedExport] = state match {
  case GrouperState.InConstellation(region, constellation, solarSystems) =>
    Some(GroupedExport.RegionSolarSystems(region, constellation, solarSystems))
  case _ => None
}
