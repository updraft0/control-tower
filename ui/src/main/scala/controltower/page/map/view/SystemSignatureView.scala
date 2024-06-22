package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.backend.ESI
import controltower.component.*
import controltower.page.map.{MapAction, RoleController}
import controltower.ui.*
import org.updraft0.controltower.constant.{ConnectionId, UnknownOrUnset}
import org.updraft0.controltower.protocol.*

import java.time.{Duration, Instant}

// TODO: move to magic constants
val CharacterImageSize = 32

enum SignatureFilter derives CanEqual:
  case All, Wormhole, Combat, Indy, Hacking, Unscanned

enum ConnectionTarget derives CanEqual:
  def idOpt: Option[ConnectionId] =
    this match
      case _: Unknown  => None
      case w: Wormhole => Some(w.id)

  case Unknown(isEol: Boolean, massStatus: WormholeMassStatus) extends ConnectionTarget
  case Wormhole(
      id: ConnectionId,
      toSystemId: SystemId,
      toSystemName: Signal[Option[String]],
      toSolarSystem: SolarSystem,
      isEol: Signal[Boolean],
      connection: Signal[Option[MapWormholeConnectionWithSigs]],
      sigId: Option[SigId]
  ) extends ConnectionTarget

class SystemSignatureView(
    staticData: SystemStaticData,
    selected: Signal[Option[MapSystemSnapshot]],
    actions: WriteBus[MapAction],
    settings: Signal[MapSettings],
    mapRole: Signal[MapRole],
    time: Signal[Instant],
    isConnected: Signal[Boolean]
)(using mapCtx: MapViewContext)
    extends ViewController:

  private val filter = Var(SignatureFilter.All)

  override def view =
    div(
      idAttr := "system-signature-view",
      cls    := "system-signature-view",
      cls    := "left-sidebar-view",
      hideIfEmptyOpt(selected),
      table(
        children <-- selected.splitOption(
          (mss, system) => sigView(system, filter, staticData, settings, mapRole, time, isConnected, actions),
          nodeSeq()
        )
      )
    )

private inline def sigView(
    system: Signal[MapSystemSnapshot],
    currentFilter: Var[SignatureFilter],
    static: SystemStaticData,
    settings: Signal[MapSettings],
    mapRole: Signal[MapRole],
    time: Signal[Instant],
    isConnected: Signal[Boolean],
    actions: WriteBus[MapAction]
)(using mapCtx: MapViewContext) =
  val solarSystem = system.map(mss => static.solarSystemMap(mss.system.systemId))
  val selected    = Selectable[SigId]()

  val signatures           = system.map(mss => mss.signatures.toList)
  val signatureScanPercent = signatures.map(sigs => s"${scanPercent(sigs, true).toInt}%")
  val canEdit              = isConnected.combineWith(mapRole.map(RoleController.canEditSignatures(_))).map(_ && _)

  given SystemStaticData = static

  val connectionTargets = system.map: mss =>
    mss.connections.map: whc =>
      val targetId   = if (whc.fromSystemId == mss.system.systemId) whc.toSystemId else whc.fromSystemId
      val connection = mapCtx.connection(whc.id)
      ConnectionTarget.Wormhole(
        id = whc.id,
        toSystemId = targetId,
        toSystemName = mapCtx.systemName(targetId),
        toSolarSystem = static.solarSystemMap(targetId),
        connection = connection,
        isEol = connection.map(
          _.exists(whcs => whcs.toSignature.exists(_.eolAt.nonEmpty) || whcs.fromSignature.exists(_.eolAt.nonEmpty))
        ),
        sigId = mss.signatures
          .find {
            case w: MapSystemSignature.Wormhole => w.connectionId.contains(whc.id)
            case _                              => false
          }
          .map(_.id)
      )

  nodeSeq(
    thead(
      tr(
        cls := "signature-toolbar",
        td(
          colSpan := 6,
          div(
            cls := "signature-toolbar",
            span("Signatures"),
            select(
              idAttr := "system-signature-filter",
              cls    := "signature-filter",
              modSeq(SignatureFilter.values.map(_.selectOption).toSeq),
              controlled(
                value <-- currentFilter.signal.map(_.toString),
                onChange.mapToValue.map(SignatureFilter.valueOf) --> currentFilter
              )
            ),
            span(
              cls := "signature-selection",
              i(cls := "ti", cls := "ti-filter"),
              child.text <-- signatures
                .combineWith(currentFilter.signal)
                .map((sigs, filter) => s"${sigs.count(isVisibleWithFilter(filter, _))}/${sigs.length}")
            ),
            // TODO not sure this is necessary!
//            button(
//              idAttr := "sig-select-all",
//              typ    := "button",
//              cls    := "ti",
//              cls    := "ti-select-all"
//            ),
            button(
              idAttr := "sig-paste-signatures",
              typ    := "button",
              cls    := "ti",
              cls    := "ti-clipboard-plus",
              disabled <-- canEdit.map(!_),
              onClick.stopPropagation.mapToUnit.compose(_.withCurrentValueOf(system, solarSystem)) --> (
                (mss, solarSystem) =>
                  Modal.show(
                    pasteSignaturesView(
                      mss,
                      solarSystem,
                      solarSystem.systemClass
                        .flatMap(whc => static.signatureByClassAndGroup.get(whc))
                        .getOrElse(Map.empty),
                      static,
                      time,
                      actions
                    ),
                    Observer.empty[Unit],
                    false,
                    cls := "system-paste-signatures"
                  )
              )
            ),
            button(
              idAttr := "sig-add-signature",
              typ    := "button",
              cls    := "ti",
              cls    := "ti-plus",
              disabled <-- canEdit.map(!_),
              onClick.stopPropagation.mapToUnit.compose(_.withCurrentValueOf(solarSystem)) --> (solarSystem =>
                Modal.show(
                  addSingleSignatureView(
                    solarSystem,
                    solarSystem.systemClass
                      .flatMap(whc => static.signatureByClassAndGroup.get(whc))
                      .getOrElse(Map.empty),
                    static.wormholeTypes,
                    actions,
                    canEdit
                  ),
                  Observer.empty[Unit],
                  true,
                  cls := "system-add-signature"
                )
              )
            ),
            button(
              idAttr := "sig-edit-signature",
              typ    := "button",
              disabled <-- canEdit.combineWith(selected.signal).map(!_ || _.size != 1),
              cls := "ti",
              cls := "ti-pencil",
              onClick.stopPropagation.mapToUnit.compose(
                _.withCurrentValueOf(selected.signal, signatures, solarSystem)
              ) --> ((selected, signatures, solarSystem) =>
                Modal.show(
                  editSingleSignatureView(
                    solarSystem,
                    signatures.find(_.id == selected.head).get,
                    solarSystem.systemClass
                      .flatMap(whc => static.signatureByClassAndGroup.get(whc))
                      .getOrElse(Map.empty),
                    static.wormholeTypes,
                    actions,
                    canEdit
                  ),
                  Observer.empty[Unit],
                  true,
                  cls := "system-add-signature"
                )
              )
            ),
            button(
              idAttr := "sig-remove-selected",
              typ    := "button",
              disabled <-- canEdit.combineWith(selected.signal).map(!_ || _.isEmpty),
              cls := "sig-destructive",
              cls := "ti",
              cls := "ti-eraser",
              onClick.stopPropagation.mapToUnit.compose(_.withCurrentValueOf(selected.signal, solarSystem)) --> (
                (selected, solarSystem) =>
                  Modal.showConfirmation(
                    s"Remove ${selected.size} signatures?",
                    s"Confirm removal of signatures ${selected.mkString(", ")} in ${solarSystem.name}?",
                    Observer(_ => actions.onNext(MapAction.RemoveSignatures(solarSystem.id, selected)))
                  )
              )
            ),
            button(
              idAttr := "sig-remove-all-signatures",
              typ    := "button",
              cls    := "sig-destructive",
              cls    := "ti",
              cls    := "ti-clear-all",
              disabled <-- canEdit.combineWith(signatures).map(!_ || _.isEmpty),
              onClick.stopPropagation.mapToUnit.compose(_.withCurrentValueOf(solarSystem)) --> (solarSystem =>
                Modal.showConfirmation(
                  "Remove all signatures?",
                  s"Clear all signatures in ${solarSystem.name}?",
                  Observer(_ => actions.onNext(MapAction.RemoveAllSignatures(solarSystem.id)))
                )
              )
            )
          ),
          div(
            cls := "system-scan-header",
            div(
              cls := "system-scan-progress",
              div(
                cls := "system-scan-progress-bar",
                cls <-- signatures.map(scanClass),
                width <-- signatureScanPercent
              )
            ),
            mark(cls := "system-scan-percent", child.text <-- signatureScanPercent)
          )
        )
      ),
      tr(
        cls := "table-header",
        th("id"),
        th("group"),
        th("type"),
        th("target"),
        th(i(cls := "ti", cls := "ti-clock-filled")),
        th(i(cls := "ti", cls := "ti-user"))
      )
    ),
    tbody(
      children <-- signatures.split(mss => (mss.systemId, mss.id))((k, _, sig) =>
        signatureRow(
          k._2,
          sig,
          connectionTargets,
          time,
          currentFilter.signal,
          settings,
          selected,
          actions.contramap(nss => MapAction.UpdateSignatures(k._1, false, Array(nss))),
          solarSystem,
          canEdit
        )(using static)
      )
    )
  )

private def signatureRow(
    sigId: SigId,
    sig: Signal[MapSystemSignature],
    connections: Signal[Array[ConnectionTarget.Wormhole]],
    time: Signal[Instant],
    filter: Signal[SignatureFilter],
    settings: Signal[MapSettings],
    selectedSigs: Selectable[SigId],
    onSigChange: Observer[NewSystemSignature],
    solarSystem: Signal[SolarSystem],
    canEdit: Signal[Boolean]
)(using static: SystemStaticData) =
  val isSelected = selectedSigs.isSelected(sigId)

  val signaturesByGroup =
    solarSystem.map(_.systemClass.map(whc => static.signatureByClassAndGroup(whc)).getOrElse(Map.empty))

  def signatureGroupCell(s: MapSystemSignature) =
    val group = Var(s.signatureGroup)
    val dropdown = OptionDropdown(
      SignatureGroup.values.toSeq.filterNot(s.signatureGroup != SignatureGroup.Unknown && _ == SignatureGroup.Unknown),
      group,
      isDisabled = canEdit.map(!_)
    )
    td(
      cls := "signature-group",
      cls := "editable",
      dropdown.view,
      group.signal.changes --> Observer[SignatureGroup](sg =>
        if (sg != s.signatureGroup) onSigChange.onNext(changeSignatureGroup(sg, s))
      )
    )

  def siteTypeCell(s: MapSystemSignature.Site, signatureGroups: Map[SignatureGroup, List[SignatureClassified]]) =
    // TODO: cannot revert back to Unknown either
    // FIXME: use UknownOrUnset
    val sigType = Var(
      SignatureClassified.Other(
        s.name match
          case Some("")    => "Unknown"
          case Some(other) => other
          case None        => "Unknown"
      )
    )

    val dropdown = OptionDropdown(signatureGroups.getOrElse(s.group, Nil), sigType, isDisabled = canEdit.map(!_))

    td(
      cls := "signature-type",
      cls := "editable",
      dropdown.view,
      sigType.signal.changes --> Observer[SignatureClassified] { sc =>
        val newName = Option.when(sc.name != "Unknown" && !sc.name.isBlank)(sc.name)
        if (newName != s.name) onSigChange.onNext(changeSignatureName(newName, s))
      }
    )

  def wormholeTargetSelect(w: MapSystemSignature.Wormhole, targets: Array[ConnectionTarget.Wormhole]) =
    val default = ConnectionTarget.Unknown(isEol = w.eolAt.isDefined, massStatus = w.massStatus)
    val current = Var(
      targets
        .find(_.idOpt.zip(w.connectionId).exists(_ == _))
        .getOrElse(default)
    )
    val dropdown = OptionDropdown[ConnectionTarget](
      targets.view
        .filter(_.sigId.forall(_ == sigId))
        .toIndexedSeq
        .prepended(default),
      current,
      isDisabled = canEdit.map(!_)
    )
    td(
      cls := "signature-target",
      cls := "editable",
      dropdown.view,
      current.signal.changes --> Observer[ConnectionTarget]: ct =>
        if (ct.idOpt != w.connectionId)
          onSigChange.onNext(changeWormholeConnectionId(ct.idOpt, w))
    )

  val signatureUpdatedTd = td(
    cls := "signature-updated",
    cls("signature-stale") <-- time
      .withCurrentValueOf(settings, sig)
      .map((now, settings, sig) => sigIsStale(sig, settings, now)),
    child.text <-- time.withCurrentValueOf(sig).map((now, sig) => timeDiffString(now, sig.updatedAt))
  )
  val signatureUpdatedByTd = td(
    cls := "updated-by-img",
    child <-- sig.map(s => ESI.characterImage(s.updatedByCharacterId, "updatedBy", size = CharacterImageSize))
  )

  tr(
    onClick.filter(_.ctrlKey).stopPropagation.mapToUnit --> selectedSigs.toggle(sigId),
    cls("selected") <-- isSelected,
    display <-- sig.combineWith(filter).map((s, f) => isVisibleWithFilter(f, s)).map(toDisplayValue),
    td(cls := "signature-id", sigId.convert.take(3)),
    child <-- sig.map(signatureGroupCell),
    child <-- sig
      .combineWith(solarSystem, signaturesByGroup)
      .map: (s, solarSystem, groups) =>
        s match
          case u: MapSystemSignature.Unknown =>
            td(cls := "signature-type")
          case s: MapSystemSignature.Site =>
            siteTypeCell(s, groups)
          case w: MapSystemSignature.Wormhole =>
            wormholeSelect(
              w.connectionType,
              solarSystem,
              groups,
              canEdit,
              Observer[WormholeSelectInfo]: wsi =>
                if (wsi.connectionType != w.connectionType)
                  onSigChange.onNext(changeWormholeConnectionType(wsi.connectionType, w))
            )
    ,
    child <-- sig
      .combineWith(connections)
      .map: (s, connectionTargets) =>
        s match
          case w: MapSystemSignature.Wormhole =>
            wormholeTargetSelect(w, connectionTargets)
          case _ =>
            td(cls := "signature-target")
    ,
    signatureUpdatedTd,
    signatureUpdatedByTd
  )

private def isVisibleWithFilter(filter: SignatureFilter, sig: MapSystemSignature) = (filter, sig) match
  case (SignatureFilter.Unscanned, sig)                     => !sigIsScanned(sig, fakeScan = true)
  case (SignatureFilter.All, _: MapSystemSignature.Unknown) => true
  case (_, _: MapSystemSignature.Unknown)                   => false
  case (_, _: MapSystemSignature.Wormhole)                  => true
  case (SignatureFilter.All, _: MapSystemSignature.Site)    => true
  case (SignatureFilter.Indy, s: MapSystemSignature.Site) =>
    s.signatureGroup == SignatureGroup.Ore || s.signatureGroup == SignatureGroup.Gas
  case (SignatureFilter.Hacking, s: MapSystemSignature.Site) =>
    s.signatureGroup == SignatureGroup.Relic || s.signatureGroup == SignatureGroup.Data || s.signatureGroup == SignatureGroup.Ghost
  case (SignatureFilter.Combat, s: MapSystemSignature.Site) =>
    s.signatureGroup == SignatureGroup.Combat
  case (SignatureFilter.Wormhole, _: MapSystemSignature.Site) => false

private def selectGroup(currentValue: SignatureGroup, newGroup: Observer[SignatureGroup]) =
  // note: need to exclude unknown because currently cannot go back to unknown
  // FIXME allow reverting back to Unknown
  select(
    cls := "signature-group-inline",
    SignatureGroup.values
      .filter(currentValue == SignatureGroup.Unknown || _ != SignatureGroup.Unknown)
      .map(sg => sg.selectOption.amend(selected := currentValue == sg))
      .toSeq,
    onInput.mapToValue.map(SignatureGroup.valueOf) --> newGroup
  )

private def changeSignatureGroup(newGroup: SignatureGroup, prev: MapSystemSignature): NewSystemSignature =
  (newGroup, prev) match
    case (SignatureGroup.Unknown, _) => NewSystemSignature.Unknown(prev.id, prev.createdAt)
    case (SignatureGroup.Wormhole, u: MapSystemSignature.Unknown) =>
      NewSystemSignature.Wormhole(
        id = u.id,
        createdAt = u.createdAt,
        isEol = false,
        connectionType = WormholeConnectionType.Unknown,
        massStatus = WormholeMassStatus.Unknown,
        massSize = WormholeMassSize.Unknown,
        connectionId = UnknownOrUnset.Unset()
      )
    case (SignatureGroup.Wormhole, s: MapSystemSignature.Site) =>
      NewSystemSignature.Wormhole(
        id = s.id,
        createdAt = s.createdAt,
        isEol = false,
        connectionType = WormholeConnectionType.Unknown,
        massStatus = WormholeMassStatus.Unknown,
        massSize = WormholeMassSize.Unknown,
        connectionId = UnknownOrUnset.Unset()
      )
    case (SignatureGroup.Wormhole, w: MapSystemSignature.Wormhole) =>
      NewSystemSignature.Wormhole(
        id = w.id,
        createdAt = w.createdAt,
        isEol = w.eolAt.isDefined,
        connectionType = w.connectionType,
        massStatus = w.massStatus,
        massSize = w.massSize,
        connectionId = UnknownOrUnset(w.connectionId)
      )
    case (_, w: MapSystemSignature.Wormhole) => NewSystemSignature.Site(w.id, w.createdAt, newGroup, name = Some(""))
    case (_, u: MapSystemSignature.Unknown)  => NewSystemSignature.Site(u.id, u.createdAt, newGroup, name = Some(""))
    case (_, s: MapSystemSignature.Site)     => NewSystemSignature.Site(s.id, s.createdAt, newGroup, name = Some(""))

private def changeSignatureName(newName: Option[String], prev: MapSystemSignature.Site): NewSystemSignature =
  NewSystemSignature.Site(
    id = prev.id,
    createdAt = prev.createdAt,
    group = prev.group,
    name = newName
  )

private def changeWormholeConnectionType(
    newType: WormholeConnectionType,
    prev: MapSystemSignature.Wormhole
): NewSystemSignature =
  NewSystemSignature.Wormhole(
    id = prev.id,
    createdAt = prev.createdAt,
    isEol = prev.eolAt.isDefined,
    connectionType = newType,
    massStatus = prev.massStatus,
    massSize = prev.massSize,
    connectionId = UnknownOrUnset(prev.connectionId)
  )

private def changeWormholeConnectionId(
    newId: Option[ConnectionId],
    prev: MapSystemSignature.Wormhole
): NewSystemSignature =
  NewSystemSignature.Wormhole(
    id = prev.id,
    createdAt = prev.createdAt,
    isEol = prev.eolAt.isDefined,
    connectionType = prev.connectionType,
    massStatus = prev.massStatus,
    massSize = prev.massSize,
    connectionId = UnknownOrUnset(newId)
  )

given DropdownItem[SignatureGroup] with
  def key(sg: SignatureGroup): String           = sg.toString
  def group(sg: SignatureGroup): Option[String] = None
  def view(sg: SignatureGroup): Element         = span(dataAttr("signature-group") := sg.toString, sg.toString)

given DropdownItem[SignatureClassified] with
  def key(sc: SignatureClassified): String           = sc.name
  def group(sc: SignatureClassified): Option[String] = None
  def view(sc: SignatureClassified): Element         = span(dataAttr("signature-type") := sc.name, sc.name)

given DropdownItem[ConnectionTarget] with
  def key(ct: ConnectionTarget): String = ct match
    case _: ConnectionTarget.Unknown  => "unknown"
    case w: ConnectionTarget.Wormhole => w.id.toString
  def group(ct: ConnectionTarget): Option[String] = None
  def view(ct: ConnectionTarget): Element = ct match
    case ConnectionTarget.Unknown(isEol, massStatus) =>
      span(
        dataAttr("connection-type") := "Unknown",
        dataAttr("mass-status")     := massStatus.toString,
        cls("wormhole-eol")         := isEol,
        "Unknown"
      )
    case ConnectionTarget.Wormhole(id, _, toName, toSystem, isEol, connection, _) =>
      // TODO: this code duplicates the wormhole rendering code in the paste signature view
      span(
        cls                       := "wormhole-connection-option",
        dataAttr("connection-id") := id.toString,
        dataAttr("mass-status") <-- connection
          .mapSome(getWormholeMassStatus)
          .map(_.getOrElse(WormholeMassStatus.Unknown).toString),
        dataAttr("mass-size") <-- connection
          .mapSome(getWormholeMassSize)
          .map(_.getOrElse(WormholeMassSize.Unknown).toString),
        cls("wormhole-eol") <-- isEol,
        span(
          cls := "connection-system-name",
          child.text <-- toName.map(_.getOrElse(toSystem.name))
        ),
        mark(
          cls := "wh-target-class",
          cls := "system-class",
          cls := s"system-class-${toSystem.systemClass.get.toString.toLowerCase}",
          systemClassString(toSystem.systemClass.get)
        )
      )

given (using static: SystemStaticData): DropdownItem[WormholeSelectInfo] with
  def key(wsi: WormholeSelectInfo): String           = wsi.key
  def group(wsi: WormholeSelectInfo): Option[String] = Some(wsi.group)
  def view(wsi: WormholeSelectInfo): Element =
    div(
      dataAttr("wormhole-type") := wsi.key,
      wormholeTypeCell(wsi.connectionType, false, WormholeMassStatus.Unknown, WormholeMassSize.Unknown, None, static)
    )

private def toDisplayValue(res: Boolean) = if (res) "" else "none"

private[view] def timeDiff(time: Observable[Instant], start: Instant) =
  child.text <-- time.map(now => timeDiffString(now, start))

private[view] def timeDiffString(now: Instant, start: Instant) =
  displayDuration(Duration.between(start, now))

private inline def displayDuration(d: Duration) =
  if (d.getSeconds < 60) s"${d.getSeconds.max(0)}s"
  else if (d.getSeconds < 60 * 60) s"${d.getSeconds / 60}m ${d.getSeconds % 60}s"
  else s"${d.getSeconds / 3_600}h ${d.getSeconds % 3_600 / 60}m"

private def scanClass(sigs: List[MapSystemSignature]) =
  if (sigs.forall(!sigIsScanned(_, fakeScan = true))) "unscanned"
  else if (sigs.exists(!sigIsScanned(_, fakeScan = true))) "partially-scanned"
  else "fully-scanned"

private[map] def scanPercent(sigs: Iterable[MapSystemSignature], fullOnEmpty: Boolean): Double =
  val needsScanning = sigs.count(sigNeedsScanning)
  val scanned       = sigs.count(sigIsScanned(_))

  if (scanned == 0 && fullOnEmpty) 100
  else if (needsScanning == 0) 0
  else 100.0 * (scanned.toDouble / needsScanning)

private inline def sigIsScanned(sig: MapSystemSignature, fakeScan: Boolean = false) =
  sig match
    case _: MapSystemSignature.Unknown   => false
    case wh: MapSystemSignature.Wormhole => hasWormholeTarget(wh)
    case s: MapSystemSignature.Site
        if s.signatureGroup == SignatureGroup.Combat || s.signatureGroup == SignatureGroup.Ore =>
      fakeScan
    case _ => true

private inline def hasWormholeTarget(wh: MapSystemSignature.Wormhole) =
  wh.connectionType != WormholeConnectionType.Unknown && wh.connectionId.isDefined

private inline def sigNeedsScanning(sig: MapSystemSignature) =
  sig.signatureGroup match
    case SignatureGroup.Combat => false
    case SignatureGroup.Ore    => false
    case _                     => true

private[map] def scanStale(sigs: Array[MapSystemSignature], settings: MapSettings, now: Instant): Boolean =
  sigs.exists(sigIsStale(_, settings, now))

private[map] def sigIsStale(sig: MapSystemSignature, settings: MapSettings, now: Instant): Boolean =
  sig.updatedAt.plus(settings.staleScanThreshold).isBefore(now)

private def addSingleSignatureView(
    solarSystem: SolarSystem,
    signatureGroups: Map[SignatureGroup, List[SignatureClassified]],
    wormholeTypes: Map[Long, WormholeType],
    actions: WriteBus[MapAction],
    canEdit: Signal[Boolean]
)(
    closeMe: Observer[Unit],
    owner: Owner
)(using SystemStaticData) =
  val validationError = Var(Option.empty[String])
  val addEdit = AddEditSignatureView(
    solarSystem,
    signatureGroups,
    wormholeTypes,
    None,
    validationError,
    actions.contramap { nss =>
      closeMe.onNext(())
      MapAction.AddSignature(solarSystem.id, nss)
    },
    canEdit
  )

  div(
    cls := "system-add-signature-view",
    cls := "dialog-view",
    h2(cls := "dialog-header", "Add signature"),
    addEdit.view,
    div(
      cls := "add-signature-line",
      hideIfEmptyOpt(validationError.signal),
      child.maybe <-- validationError.signal.map(_.map(span(_)))
    )
  )

private def editSingleSignatureView(
    solarSystem: SolarSystem,
    sig: MapSystemSignature,
    signatureGroups: Map[SignatureGroup, List[SignatureClassified]],
    wormholeTypes: Map[Long, WormholeType],
    actions: WriteBus[MapAction],
    canEdit: Signal[Boolean]
)(closeMe: Observer[Unit], owner: Owner)(using SystemStaticData) =
  val validationError = Var(Option.empty[String])
  val addEdit = AddEditSignatureView(
    solarSystem,
    signatureGroups,
    wormholeTypes,
    Some(sig),
    validationError,
    actions.contramap { nss =>
      closeMe.onNext(())
      MapAction.UpdateSignatures(solarSystem.id, false, Array(nss))
    },
    canEdit
  )

  div(
    cls := "system-edit-signature-view",
    cls := "dialog-view",
    h2(cls := "dialog-header", "Edit signature"),
    addEdit.view,
    div(
      cls := "add-signature-line",
      hideIfEmptyOpt(validationError.signal),
      child.maybe <-- validationError.signal.map(_.map(span(_)))
    )
  )

private[map] def pasteSignaturesView(
    mss: MapSystemSnapshot,
    solarSystem: SolarSystem,
    signatureGroups: Map[SignatureGroup, List[SignatureClassified]],
    static: SystemStaticData,
    time: Signal[Instant],
    actions: WriteBus[MapAction]
)(closeMe: Observer[Unit], owner: Owner) =
  val validationError = Var(Option.empty[String])
  val updates         = Var(Option.empty[Array[SignatureUpdate]])
  val shouldReplace   = Var(false)
  val addAll          = PasteSignaturesView(mss.signatures, static, time, updates.writer, shouldReplace)
  div(
    cls := "system-paste-signatures-view",
    cls := "dialog-view",
    h2(cls := "dialog-header", s"Paste system signatures [${mss.system.name.getOrElse(solarSystem.name)}]"),
    addAll.view,
    div(
      cls := "add-signature-line",
      hideIfEmptyOpt(validationError.signal),
      child.maybe <-- validationError.signal.map(_.map(s => span(cls := "validation-error", s)))
    ),
    div(
      cls := "dialog-submit",
      button(
        tpe := "button",
        cls := "cancel",
        "cancel",
        onClick.preventDefault.stopPropagation.mapToUnit --> closeMe
      ),
      button(
        tpe := "button",
        cls := "update-signatures",
        i(cls := "ti", cls := "ti-clipboard-plus"),
        " update signatures",
        onClick.preventDefault.stopPropagation.mapToUnit.compose(
          _.withCurrentValueOf(updates).filter(_.isDefined).map(_.get).withCurrentValueOf(shouldReplace.signal)
        ) --> { case (updates, replaceAll) =>
          val scanned = updates.flatMap(_.toScanned)
          actions.onNext(MapAction.UpdateSignatures(mss.system.systemId, replaceAll, scanned))
          closeMe.onNext(())
        }
      )
    )
  )

private[view] def wormholeSelect(
    connectionType: WormholeConnectionType,
    solarSystem: SolarSystem,
    signatureGroups: Map[SignatureGroup, List[SignatureClassified]],
    canEdit: Signal[Boolean],
    observer: Observer[WormholeSelectInfo],
    useTd: Boolean = true
)(using static: SystemStaticData) =
  val possibleWormholeTypes = wormholeTypesList(
    solarSystem,
    signatureGroups,
    static.wormholeTypes
  )
  val wormholeType = Var(
    possibleWormholeTypes
      .find(_.connectionType == connectionType)
      .getOrElse:
//        org.scalajs.dom.console
//          .debug(s"Potential bug - have a connection type ${connectionType} but no possible wormhole types found")
        possibleWormholeTypes.head
  )
  val dropdown = OptionDropdown(possibleWormholeTypes, wormholeType, isDisabled = canEdit.map(!_))

  if (useTd)
    td(
      cls := "signature-type",
      cls := "editable",
      dropdown.view,
      wormholeType.signal.changes --> observer
    )
  else
    div(
      cls := "signature-type",
      cls := "editable",
      dropdown.view,
      wormholeType.signal.changes --> observer
    )

private[view] inline def getWormholeMassStatus(connection: MapWormholeConnectionWithSigs) =
  getFromBoth(connection, _.massStatus, WormholeMassStatus.Unknown)

private[view] inline def getWormholeMassSize(connection: MapWormholeConnectionWithSigs) =
  getFromBoth(connection, _.massSize, WormholeMassSize.Unknown)

private inline def getFromBoth[A](
    connection: MapWormholeConnectionWithSigs,
    f: MapSystemSignature.Wormhole => A,
    default: A
)(using CanEqual[A, A]): A =
  (connection.fromSignature, connection.toSignature) match
    case (Some(from), Some(to)) =>
      val (fromA, toA) = (f(from), f(to))
      if (fromA != toA)
        throw new IllegalStateException(
          s"BUG: connection with sigs attribute did not equal on both signatures [$toA != $fromA]"
        )
      fromA
    case (None, Some(to))   => f(to)
    case (Some(from), None) => f(from)
    case (None, None)       => default
