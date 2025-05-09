package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.ui.*
import org.updraft0.controltower.constant.{SigId, TypeId, UnknownOrUnset, WormholeClass}
import org.updraft0.controltower.protocol.*
import java.time.Instant

private case class WormholeSelectInfo(
    key: String,
    name: String,
    group: String,
    targetClass: Option[WormholeClass],
    connectionType: WormholeConnectionType
) derives CanEqual

private object SignatureId:
  // TODO: move to constants
  val MaxLength = 7

  private val SigIdNamePrefix        = "^[A-Za-z]{1,3}".r
  private val SigIdNameDashPrefix    = "^[A-Za-z]{3}-".r
  private val SigIdNameDashNumPrefix = "^[A-Za-z]{3}-[0-9]{1,3}$".r
  private val SigIdRegexFull         = "^[A-Za-z]{3}-[0-9]{3}$".r

  inline def isValidSigId(s: String): Boolean =
    SigIdRegexFull.matches(s)

  inline def isValidSigIdOnInput(s: String): Boolean =
    if (s.isEmpty) true
    else if (s.length <= 3) SigIdNamePrefix.matches(s)
    else if (s.length <= 4) SigIdNameDashPrefix.matches(s)
    else if (s.length < 7) SigIdNameDashNumPrefix.matches(s)
    else SigIdRegexFull.matches(s)

class AddEditSignatureView(
    solarSystem: SolarSystem,
    signatureGroups: Map[SignatureGroup, List[SignatureClassified]],
    existing: Option[MapSystemSignature],
    validationError: Var[Option[String]],
    onAdd: Observer[NewSystemSignature],
    canEdit: Signal[Boolean]
)(using SystemStaticData)
    extends ViewController:

  override def view =
    val signatureId = Var(existing.map(_.id.convert).getOrElse(""))

    val signatureGroup      = Var(existing.map(_.signatureGroup).getOrElse(SignatureGroup.Unknown))
    val oreSignatureType    = Var(existing.filter(_.signatureGroup == SignatureGroup.Ore).flatMap(getSiteName))
    val gasSignatureType    = Var(existing.filter(_.signatureGroup == SignatureGroup.Gas).flatMap(getSiteName))
    val combatSignatureType = Var(existing.filter(_.signatureGroup == SignatureGroup.Combat).flatMap(getSiteName))
    val relicSignatureType  = Var(existing.filter(_.signatureGroup == SignatureGroup.Relic).flatMap(getSiteName))
    val dataSignatureType   = Var(existing.filter(_.signatureGroup == SignatureGroup.Data).flatMap(getSiteName))
    val ghostSignatureType  = Var(existing.filter(_.signatureGroup == SignatureGroup.Ghost).flatMap(getSiteName))
    val wormholeType = Var(
      existing
        .filter(_.signatureGroup == SignatureGroup.Wormhole)
        .flatMap(getWormholeType)
        .getOrElse(WormholeConnectionType.Unknown)
    )

    val wormholeIsEol = Var(existing.exists {
      case w: MapSystemSignature.Wormhole => w.eolAt.isDefined
      case _                              => false
    })

    val wormholeMassStatus = Var(
      existing
        .map {
          case w: MapSystemSignature.Wormhole => w.massStatus
          case _                              => WormholeMassStatus.Unknown
        }
        .getOrElse(WormholeMassStatus.Unknown)
    )

    val wormholeMassSize = Var(
      existing
        .map {
          case w: MapSystemSignature.Wormhole => w.massSize
          case _                              => WormholeMassSize.Unknown
        }
        .getOrElse(WormholeMassSize.Unknown)
    )

    div(
      cls := "add-edit-signature-view",
      div(
        cls := "add-signature-line",
        input(
          typ         := "text",
          cls         := "signature-id",
          maxLength   := SignatureId.MaxLength,
          placeholder := "ABC-123",
          disabled    := existing.isDefined,
          controlled(
            value <-- signatureId,
            onInput.mapToValue.filter(SignatureId.isValidSigIdOnInput(_)) --> signatureId
          )
        ),
        select(
          cls := "signature-group",
          SignatureGroup.values
            .filterNot(sg => sg == SignatureGroup.Unknown && existing.isDefined) // cannot revert to Unknown presently
            .map(sg => if (signatureGroup.now() == sg) sg.selectOption.amend(selected := true) else sg.selectOption)
            .toSeq,
          onInput.mapToValue.map(SignatureGroup.valueOf) --> signatureGroup
        )
      ),
      div(
        cls := "add-signature-line",
        // FIXME - the default selections don't work right now :/
        child <-- signatureGroup.signal.flatMapSwitch:
          case SignatureGroup.Unknown => Val(emptyNode)
          case SignatureGroup.Relic   => Val(selectSite(SignatureGroup.Relic, relicSignatureType, signatureGroups))
          case SignatureGroup.Data    => Val(selectSite(SignatureGroup.Data, dataSignatureType, signatureGroups))
          case SignatureGroup.Combat  => Val(selectSite(SignatureGroup.Combat, combatSignatureType, signatureGroups))
          case SignatureGroup.Ore     => Val(selectSite(SignatureGroup.Ore, oreSignatureType, signatureGroups))
          case SignatureGroup.Gas     => Val(selectSite(SignatureGroup.Gas, gasSignatureType, signatureGroups))
          case SignatureGroup.Ghost   => Val(selectSite(SignatureGroup.Ghost, ghostSignatureType, signatureGroups))
          case SignatureGroup.Wormhole =>
            wormholeType.signal.map: wct =>
              div(
                cls := "add-signature-line",
                wormholeSelect(
                  wct,
                  solarSystem,
                  signatureGroups,
                  canEdit,
                  wormholeType.writer.contramap(_.connectionType),
                  useTd = false
                ),
                label(
                  "EOL?",
                  input(
                    cls := "wormhole-is-eol",
                    tpe := "checkbox",
                    checked <-- wormholeIsEol,
                    onInput.mapToChecked --> wormholeIsEol
                  )
                ),
                select(
                  cls := "wormhole-mass-status",
                  WormholeMassStatus.values
                    .map(ms =>
                      if (wormholeMassStatus.now() == ms) ms.selectOption.amend(selected := true)
                      else ms.selectOption
                    )
                    .toSeq,
                  onInput.mapToValue.map(WormholeMassStatus.valueOf) --> wormholeMassStatus
                ),
                select(
                  cls := "wormhole-mass-size",
                  WormholeMassSize.values
                    .map(ms =>
                      if (wormholeMassSize.now() == ms) ms.selectOption.amend(selected := true)
                      else ms.selectOption
                    )
                    .toSeq,
                  onInput.mapToValue.map(WormholeMassSize.valueOf) --> wormholeMassSize
                )
              )
      ),
      button(
        tpe := "button",
        existing.map(_ => "Edit").getOrElse("Add"),
        onClick.stopPropagation.mapToUnit --> (_ => {
          val sigId          = signatureId.now()
          val connectionType = wormholeType.now()
          val now            = Instant.now()
          val resOpt = (SignatureId.isValidSigId(sigId), sigId, signatureGroup.now()) match
            case (false, _, _) =>
              validationError.set(Some("Invalid sig id"))
              None
            case (true, sigId, SignatureGroup.Unknown) =>
              Some(NewSystemSignature.Unknown(SigId(sigId), now))
            case (true, sigId, SignatureGroup.Wormhole) =>
              Some(
                NewSystemSignature.Wormhole(
                  id = SigId(sigId),
                  createdAt = now,
                  isEol = wormholeIsEol.now(),
                  connectionType = connectionType,
                  massStatus = wormholeMassStatus.now(),
                  massSize = wormholeMassSize.now(),
                  connectionId = UnknownOrUnset.Unknown()
                )
              )
            case (true, sigId, otherGroup) =>
              Some(
                NewSystemSignature.Site(
                  id = SigId(sigId),
                  createdAt = now,
                  group = otherGroup,
                  name = otherGroup match
                    case SignatureGroup.Combat                            => combatSignatureType.now()
                    case SignatureGroup.Ghost                             => ghostSignatureType.now()
                    case SignatureGroup.Relic                             => relicSignatureType.now()
                    case SignatureGroup.Data                              => dataSignatureType.now()
                    case SignatureGroup.Ore                               => oreSignatureType.now()
                    case SignatureGroup.Gas                               => gasSignatureType.now()
                    case SignatureGroup.Unknown | SignatureGroup.Wormhole => None
                )
              )
            case _ => None

          resOpt.foreach(onAdd.onNext)
        })
      )
    )

private val UnknownTypes = List(
  WormholeSelectInfo(
    "unknown",
    "Unknown",
    "Unknown",
    None,
    WormholeConnectionType.Unknown
  )
)

private val K162Types = List(
  WormholeSelectInfo(
    "k162-u",
    "K162 -> C1/2/3",
    "K162",
    None,
    WormholeConnectionType.K162(WormholeK162Type.Unknown)
  ),
  WormholeSelectInfo(
    "k162-d",
    "K162 -> C4/5",
    "K162",
    None,
    WormholeConnectionType.K162(WormholeK162Type.Dangerous)
  ),
  WormholeSelectInfo(
    "k162-e",
    "K162 -> C6",
    "K162",
    Some(WormholeClass.C6),
    WormholeConnectionType.K162(WormholeK162Type.Deadly)
  ),
  WormholeSelectInfo(
    "k162-ns",
    "K162 -> NS",
    "K162",
    Some(WormholeClass.NS),
    WormholeConnectionType.K162(WormholeK162Type.Nullsec)
  ),
  WormholeSelectInfo(
    "k162-l",
    "K162 -> L",
    "K162",
    Some(WormholeClass.L),
    WormholeConnectionType.K162(WormholeK162Type.Losec)
  ),
  WormholeSelectInfo(
    "k162-h",
    "K162 -> H",
    "K162",
    Some(WormholeClass.H),
    WormholeConnectionType.K162(WormholeK162Type.Hisec)
  ),
  WormholeSelectInfo(
    "k162-t",
    "K162 -> T",
    "K162",
    Some(WormholeClass.Thera),
    WormholeConnectionType.K162(WormholeK162Type.Thera)
  )
)

private def getSiteName(mss: MapSystemSignature): Option[String] = mss match
  case s: MapSystemSignature.Site => s.name
  case _                          => None

private def getWormholeType(mss: MapSystemSignature): Option[WormholeConnectionType] = mss match
  case w: MapSystemSignature.Wormhole => Some(w.connectionType)
  case _                              => None

private[map] def wormholeTypesList(
    solarSystem: SolarSystem,
    signatureGroups: Map[SignatureGroup, List[SignatureClassified]],
    wormholeTypes: Map[TypeId, WormholeType]
) =
  val selectableWormholeTypes = signatureGroups
    .get(SignatureGroup.Wormhole)
    .map(_.flatMap {
      case SignatureClassified.Wormhole(_, typeId) => wormholeTypes.get(typeId)
      case _                                       => None
    })
    .getOrElse(Nil)

  val systemStatics = solarSystem.wormholeStatics
    .flatMap(wss => wormholeTypes.get(wss.typeId))
    .map(wt =>
      WormholeSelectInfo(wt.name, wt.name, "Static", Some(wt.targetClass), WormholeConnectionType.Known(wt.typeId))
    )
    .toList

  val frigHoles = selectableWormholeTypes
    .filter(_.massSize == WormholeMassSize.S)
    .sortBy(_.targetClass.value)
    .map(wt =>
      WormholeSelectInfo(wt.name, wt.name, "Frigate", Some(wt.targetClass), WormholeConnectionType.Known(wt.typeId))
    )

  val otherWandering = selectableWormholeTypes
    .filter(_.massSize != WormholeMassSize.S)
    .filterNot(wt => systemStatics.exists(_.name == wt.name))
    .sortBy(_.targetClass.value)
    .map(wt =>
      WormholeSelectInfo(wt.name, wt.name, "Wandering", Some(wt.targetClass), WormholeConnectionType.Known(wt.typeId))
    )

  UnknownTypes ::: systemStatics ::: K162Types ::: otherWandering ::: frigHoles

private inline def unknownToOpt(s: String): Option[String] = s match
  case "unknown" => None
  case other     => Some(other)

private def selectSite(
    group: SignatureGroup,
    current: Var[Option[String]],
    groups: Map[SignatureGroup, List[SignatureClassified]]
) =
  select(
    cls := s"${group.toString.toLowerCase}-type",
    option(value := "unknown", "Unknown"),
    groups(group).map(_.name).map(n => option(value := n, selected := current.now().exists(_.contains(n)), n)),
    onInput.mapToValue.map(unknownToOpt(_)) --> current
  )
