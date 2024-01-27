package controltower.page.map.view

import com.raquo.laminar.api.L.*
import controltower.ui.{ViewController, sequence}
import org.updraft0.controltower.constant.WormholeClass
import org.updraft0.controltower.protocol.*

import java.time.Instant

case class ParsedLine(signatureId: String, tpe: String, group: String, name: String, percent: String, distance: String)

enum SignatureUpdate:
  case Added(sig: NewSystemSignature)
  case Removed(prev: MapSystemSignature)
  case Unchanged(prev: MapSystemSignature, scanned: Option[NewSystemSignature])
  case Updated(prev: MapSystemSignature, scanned: NewSystemSignature)

  def signatureId: SigId = this match
    case Added(a)        => a.id
    case Removed(r)      => SigId(r.id) /* FIXME */
    case Unchanged(p, _) => SigId(p.id)
    case Updated(p, _)   => SigId(p.id)

  def toScanned: Option[NewSystemSignature] = this match
    case Added(a)        => Some(a)
    case Removed(_)      => None
    case Unchanged(_, u) => u
    case Updated(_, u)   => Some(u)

class PasteSignaturesView(
    existingSigs: Array[MapSystemSignature],
    static: SystemStaticData,
    time: Signal[Instant],
    updates: Observer[Option[Array[SignatureUpdate]]],
    shouldReplace: Var[Boolean]
) extends ViewController:

  override def view =
    val signatures = Var("")
    val signaturesParsed = signatures.signal
      .map(parseLines)
      .withCurrentValueOf(time)
      .map { (res, now) =>
        res.filterOrElse(_.nonEmpty, "No signatures pasted").flatMap(_.map(parseLineToSignature(_, now)).sequence)
      }
      .combineWith(shouldReplace.signal)
      .map {
        case (Right(newSigs), shouldReplace) =>
          val res = diffExistingWithScanned(shouldReplace, existingSigs.toList, newSigs)
          updates.onNext(Some(res.toArray))
          Right(res)
        case (Left(msg), _) =>
          updates.onNext(None)
          Left(msg)
      }

    div(
      cls := "paste-signatures-view",
      label(
        "Replace signatures?"
      ),
      input(
        cls := "signature-replace",
        tpe := "checkbox",
        onInput.mapToChecked --> shouldReplace
      ),
      textArea(
        cls         := "signature-paste",
        placeholder := "Paste signatures here...",
        onInput.mapToValue --> signatures
      ),
      div(
        cls := "display-signatures",
        child <-- signaturesParsed.signal
          .foldEither(
            errText => span(errText),
            tableOfUpdates
          )
      )
    )

  private def tableOfUpdates(changes: List[SignatureUpdate]) =
    table(
      cls := "signature-list",
      thead(
        tr(
          th(cls := "signature-id", "id"),
          th(cls := "signature-group", "group"),
          th(cls := "signature-name", "name"),
          th(cls := "wh-target", "target"),
          th(cls := "signature-created", "created"),
          th(cls := "signature-updated", "updated")
        )
      ),
      tbody(
        changes.map { su =>
          tr(
            cls := rowClass(su),
            td(cls := "signature-id", signatureId(su): String),
            td(
              cls                         := "signature-group",
              dataAttr("signature-group") := signatureGroup(su).toString,
              signatureGroup(su).toString
            ),
            td(cls := "signature-name", signatureName(su)),
            td(cls := "wh-target" /* FIXME */ ),
            td(cls := "signature-created", timeDiff(time, signatureCreated(su))),
            td(cls := "signature-updated", timeDiff(time, signatureUpdated(su)))
          )
        }
      ),
      tfoot(
        tr(
          cls := "table-stat",
          td(
            colSpan := 6,
            div(
              cls := "table-stat",
              mark(
                cls := "table-stat",
                "Unchanged: ",
                span(
                  cls := "counter",
                  s"${changes.count {
                      case _: SignatureUpdate.Unchanged => true
                      case _                            => false
                    }}"
                )
              ),
              mark(
                cls := "table-stat",
                "Added: ",
                span(
                  cls := "counter",
                  s"${changes.count {
                      case _: SignatureUpdate.Added => true
                      case _                        => false
                    }}"
                )
              ),
              mark(
                cls := "table-stat",
                "Removed: ",
                span(
                  cls := "counter",
                  s"${changes.count {
                      case _: SignatureUpdate.Removed => true
                      case _                          => false
                    }}"
                )
              ),
              mark(
                cls := "table-stat",
                "Updated: ",
                span(
                  cls := "counter",
                  s"${changes.count {
                      case _: SignatureUpdate.Updated => true
                      case _                          => false
                    }}"
                )
              )
            )
          )
        )
      )
    )

  private def rowClass(su: SignatureUpdate) = su match
    case _: SignatureUpdate.Added     => "signature-added"
    case _: SignatureUpdate.Unchanged => "signature-unchanged"
    case _: SignatureUpdate.Updated   => "signature-updated"
    case _: SignatureUpdate.Removed   => "signature-removed"

  private inline def signatureId(su: SignatureUpdate) = su.signatureId
  private inline def signatureGroup(su: SignatureUpdate) = su match
    case SignatureUpdate.Added(a)        => a.signatureGroup
    case SignatureUpdate.Unchanged(p, _) => p.signatureGroup
    case SignatureUpdate.Updated(_, s)   => s.signatureGroup
    case SignatureUpdate.Removed(p)      => p.signatureGroup
  private inline def signatureName(su: SignatureUpdate): HtmlMod = su match
    case SignatureUpdate.Added(a)        => nameOf(a)
    case SignatureUpdate.Removed(p)      => nameOf(p)
    case SignatureUpdate.Unchanged(p, _) => nameOf(p)
    case SignatureUpdate.Updated(_, u)   => nameOf(u)

  private inline def signatureCreated(su: SignatureUpdate) = su match
    case SignatureUpdate.Added(a)        => createdAt(a)
    case SignatureUpdate.Removed(p)      => createdAt(p)
    case SignatureUpdate.Unchanged(p, _) => createdAt(p)
    case SignatureUpdate.Updated(_, u)   => createdAt(u)

  private inline def signatureUpdated(su: SignatureUpdate) = su match
    case SignatureUpdate.Added(a)        => updatedAt(a)
    case SignatureUpdate.Removed(p)      => updatedAt(p)
    case SignatureUpdate.Unchanged(p, _) => updatedAt(p)
    case SignatureUpdate.Updated(_, u)   => updatedAt(u)

  private inline def nameOf(s: NewSystemSignature): HtmlMod = s match
    case _: NewSystemSignature.Unknown => ""
    case s: NewSystemSignature.Site    => span(cls := "site-name", s.name.getOrElse(""))
    case w: NewSystemSignature.Wormhole =>
      wormholeTypeCell(w.connectionType, w.isEol, w.massStatus, w.massSize, w.connectionId, static)

  private inline def nameOf(s: MapSystemSignature): HtmlMod = s match
    case _: MapSystemSignature.Unknown => ""
    case s: MapSystemSignature.Site    => span(cls := "site-name", s.name.getOrElse(""))
    case w: MapSystemSignature.Wormhole =>
      wormholeTypeCell(w.connectionType, w.eolAt.isDefined, w.massStatus, w.massSize, w.connectionId, static)

  private inline def createdAt(s: NewSystemSignature) = s.createdAt
  private inline def createdAt(m: MapSystemSignature) = m.createdAt
  private inline def updatedAt(s: NewSystemSignature) = s.createdAt
  private inline def updatedAt(m: MapSystemSignature) = m.updatedAt

def parseLines(text: String): Either[String, List[ParsedLine]] =
  text
    .split('\n')
    .map(_.trim)
    .filterNot(_.isBlank)
    .map(_.split('\t'))
    .map {
      case Array(sigId, sigType, sigGroup, sigName, scanPercent, distance) =>
        Right(ParsedLine(sigId, sigType, sigGroup, sigName, scanPercent, distance))
      case other => Left(s"Line has incorrect format: ${other.mkString("\t")}")
    }
    .toList
    .sequence

def parseLineToSignature(line: ParsedLine, now: Instant): Either[String, NewSystemSignature] =
  for
    sigId <- Either.cond(SignatureId.isValidSigId(line.signatureId), SigId(line.signatureId), "Invalid signature id")
    group <- signatureGroupFor(line)
  yield signatureFrom(sigId, group, line, now)

// TODO: ghost sites?
private def signatureGroupFor(line: ParsedLine) = (line.tpe, line.group) match
  case (_, "Combat Site")                 => Right(SignatureGroup.Combat)
  case ("Cosmic Signature", "")           => Right(SignatureGroup.Unknown)
  case ("Cosmic Signature", "Wormhole")   => Right(SignatureGroup.Wormhole)
  case ("Cosmic Signature", "Gas Site")   => Right(SignatureGroup.Gas)
  case ("Cosmic Signature", "Data Site")  => Right(SignatureGroup.Data)
  case ("Cosmic Signature", "Relic Site") => Right(SignatureGroup.Relic)
  case ("Cosmic Anomaly", "Ore Site")     => Right(SignatureGroup.Ore)
  case (_, _)                             => Left("Could not determine signature group")

private def signatureFrom(sigId: SigId, group: SignatureGroup, line: ParsedLine, now: Instant) =
  group match
    case SignatureGroup.Unknown => NewSystemSignature.Unknown(sigId, now)
    case SignatureGroup.Wormhole =>
      NewSystemSignature.Wormhole(
        id = sigId,
        createdAt = now,
        isEol = false,
        connectionType = WormholeConnectionType.Unknown,
        massStatus = WormholeMassStatus.Unknown /* FIXME */,
        massSize = WormholeMassSize.Unknown,
        connectionId = None
      )
    case _ => NewSystemSignature.Site(sigId, now, group, Option.when(!line.name.isBlank)(line.name))

private def diffExistingWithScanned(
    isReplace: Boolean,
    existing: List[MapSystemSignature],
    scanned: List[NewSystemSignature]
): List[SignatureUpdate] =
  val existingMap = existing.map(mss => SigId(mss.id) /* FIXME */ -> mss).toMap
  val scannedMap  = scanned.map(s => s.id -> s).toMap

  val (potentialDiffs, removed, added) =
    if (isReplace)
      (
        existingMap.keySet.intersect(scannedMap.keySet),
        existingMap.keySet.diff(scannedMap.keySet),
        scannedMap.keySet.diff(existingMap.keySet)
      )
    else
      (existingMap.keySet.diff(scannedMap.keySet), Set.empty[SigId], scannedMap.keySet.diff(existingMap.keySet))

  (potentialDiffs.toList.map(sigId => compareSigs(existingMap(sigId), scannedMap.get(sigId))) :::
    removed.toList.map(sigId => SignatureUpdate.Removed(existingMap(sigId))) :::
    added.toList.map(sigId => SignatureUpdate.Added(scannedMap(sigId)))).sortBy(_.signatureId)

private def compareSigs(prev: MapSystemSignature, scannedOpt: Option[NewSystemSignature]): SignatureUpdate =
  (prev, scannedOpt) match
    case (_: MapSystemSignature.Unknown, Some(s: NewSystemSignature.Site))     => SignatureUpdate.Updated(prev, s)
    case (_: MapSystemSignature.Unknown, Some(w: NewSystemSignature.Wormhole)) => SignatureUpdate.Updated(prev, w)
    case (ps: MapSystemSignature.Site, Some(s @ NewSystemSignature.Site(_, _, newGroup, _))) if ps.group != newGroup =>
      SignatureUpdate.Updated(prev, s)
    case (ps: MapSystemSignature.Site, Some(s @ NewSystemSignature.Site(_, _, _, Some(newName))))
        if ps.name.isEmpty || !ps.name.contains(newName) =>
      SignatureUpdate.Updated(prev, s)
    case (_, _) => SignatureUpdate.Unchanged(prev, scannedOpt)

private[view] def wormholeTypeCell(
    ct: WormholeConnectionType,
    isEol: Boolean,
    massStatus: WormholeMassStatus,
    massSize: WormholeMassSize,
    connectionId: Option[Long],
    static: SystemStaticData
): HtmlMod = ct match
  case WormholeConnectionType.Unknown => "Unknown"
  case WormholeConnectionType.Known(typeId) =>
    val whType = static.wormholeTypes(typeId)
    nodeSeq(
      span(
        cls("wormhole-eol") := isEol,
        cls                 := s"wormhole-mass-status-${massStatus.toString.toLowerCase}",
        whType.name
      ),
      " ",
      span(
        cls := "wh-mass-size",
        dataAttr("wh-mass-size")(whType.massSize.toString),
        massSizeNotUnknown(whType.massSize)
      ),
      i(cls := "ti", cls := "ti-arrow-narrow-right"),
      " ",
      mark(
        cls := "wh-target-class",
        cls := "system-class",
        cls := s"system-class-${whType.targetClass.toString.toLowerCase}",
        systemClassString(whType.targetClass)
      )
    )
  case WormholeConnectionType.K162(tpe) =>
    nodeSeq(
      span(
        cls("wormhole-eol") := isEol,
        cls                 := s"wormhole-mass-status-${massStatus.toString.toLowerCase}",
        "K162"
      ),
      " ",
      span(
        cls := "wh-mass-size",
        dataAttr("wh-mass-size")(massSize.toString),
        massSizeNotUnknown(massSize)
      ),
      i(cls := "ti", cls := "ti-arrow-narrow-right"),
      " ",
      mark(
        cls := "wh-target-class",
        cls := "system-class",
        cls := s"system-class-${tpe.possibleTarget.head.toString.toLowerCase}",
        tpe.possibleTarget match
          case Nil          => "???"
          case List(single) => systemClassString(single)
          case xs           => xs.map(_.value).mkString("C", "/", "")
      )

      // TODO add other attributes
    )

private inline def massSizeNotUnknown(ms: WormholeMassSize) =
  if (ms == WormholeMassSize.Unknown) "?" else ms.toString
