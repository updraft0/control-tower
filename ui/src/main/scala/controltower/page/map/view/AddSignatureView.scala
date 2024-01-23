package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.ui.*
import org.updraft0.controltower.constant.WormholeClass
import org.updraft0.controltower.protocol.*
import java.time.Instant

private case class WormholeSelectInfo(
    key: String,
    name: String,
    group: String,
    targetClass: Option[WormholeClass],
    connectionType: WormholeConnectionType
)

private object SignatureId:
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
    wormholeTypes: Map[Long, WormholeType],
    existing: Option[MapSystemSignature],
    validationError: Var[Option[String]],
    onAdd: Observer[NewSystemSignature]
) extends ViewController:

  override def view =
    val signatureId = Var(existing.map(_.id).getOrElse(""))

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

    val possibleWormholeTypes = wormholeTypesList(solarSystem, signatureGroups, wormholeTypes)

    val wormholeIsEol = Var(existing.exists {
      case w: MapSystemSignature.Wormhole => w.eolAt.isDefined
      case _                              => false
    })

    div(
      cls := "add-edit-signature-view",
      div(
        cls := "add-signature-line",
        input(
          typ         := "text",
          cls         := "signature-id",
          maxLength   := SignatureIdLength,
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
        child <-- signatureGroup.signal.map {
          case SignatureGroup.Unknown  => emptyNode
          case SignatureGroup.Relic    => selectSite(SignatureGroup.Relic, relicSignatureType, signatureGroups)
          case SignatureGroup.Data     => selectSite(SignatureGroup.Data, dataSignatureType, signatureGroups)
          case SignatureGroup.Combat   => selectSite(SignatureGroup.Combat, combatSignatureType, signatureGroups)
          case SignatureGroup.Ore      => selectSite(SignatureGroup.Ore, oreSignatureType, signatureGroups)
          case SignatureGroup.Gas      => selectSite(SignatureGroup.Gas, gasSignatureType, signatureGroups)
          case SignatureGroup.Ghost    => selectSite(SignatureGroup.Ghost, ghostSignatureType, signatureGroups)
          case SignatureGroup.Wormhole =>
            // FIXME - render the wormholes that are possible better
            div(
              select(
                cls := "wormhole-type",
                possibleWormholeTypes.map(wsi =>
                  option(value := wsi.key, selected := wsi.connectionType == wormholeType.now(), wsi.name)
                ),
                onInput.mapToValue.map(i => possibleWormholeTypes.find(_.key == i).get.connectionType) --> wormholeType
              ),
              label(
                "EOL?",
                input(
                  cls := "wormhole-is-eol",
                  tpe := "checkbox",
                  checked <-- wormholeIsEol,
                  onInput.mapToChecked --> wormholeIsEol
                )
              )
            )
        }
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
                  massStatus = WormholeMassStatus.Unknown /* FIXME */,
                  massSize = WormholeMassSize.Unknown /* FIXME */,
                  connectionId = None
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
    Some(WormholeClass.C3),
    WormholeConnectionType.K162(WormholeK162Type.Unknown)
  ),
  WormholeSelectInfo(
    "k162-d",
    "K162 -> C4/5",
    "K162",
    Some(WormholeClass.C5),
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
    wormholeTypes: Map[Long, WormholeType]
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
