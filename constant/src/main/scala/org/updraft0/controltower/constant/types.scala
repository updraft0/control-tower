package org.updraft0.controltower.constant

import scala.language.implicitConversions

// TODO: move to typeclass encoding

opaque type TypeId = Int

object TypeId:
  val Invalid = TypeId(-1)

  def apply(i: Int): TypeId = i

  given Conversion[TypeId, Int]:
    def apply(s: TypeId): Int = s

  given Ordering[TypeId]:
    def compare(x: TypeId, y: TypeId): Int = x.compare(y)

  given CanEqual[TypeId, TypeId] = CanEqual.derived

  extension (inline v: TypeId) inline def value: Int = v

// CharacterId
opaque type CharacterId = Long

object CharacterId:
  val Invalid: CharacterId = CharacterId(-1)
  val System: CharacterId  = CharacterId(1)

  def apply(i: Long): CharacterId = i

  given Conversion[CharacterId, Long]:
    def apply(s: CharacterId): Long = s

  given Ordering[CharacterId]:
    def compare(x: CharacterId, y: CharacterId): Int = x.compare(y)

  given CanEqual[CharacterId, CharacterId] = CanEqual.derived

  extension (inline v: CharacterId) inline def value: Long = v

// CorporationId
opaque type CorporationId = Long

object CorporationId:
  def apply(i: Long): CorporationId = i

  given Conversion[CorporationId, Long]:
    def apply(s: CorporationId): Long = s

  given Ordering[CorporationId]:
    def compare(x: CorporationId, y: CorporationId): Int = x.compare(y)

  given CanEqual[CorporationId, CorporationId] = CanEqual.derived

  extension (inline v: CorporationId) inline def value: Long = v

// AllianceId
opaque type AllianceId = Long

object AllianceId:
  def apply(i: Long): AllianceId = i

  given Conversion[AllianceId, Long]:
    def apply(s: AllianceId): Long = s

  given Ordering[AllianceId]:
    def compare(x: AllianceId, y: AllianceId): Int = x.compare(y)

  given CanEqual[AllianceId, AllianceId] = CanEqual.derived

  extension (inline v: AllianceId) inline def value: Long = v

// MapId
opaque type MapId = Long

object MapId:
  val Invalid: MapId = MapId(-1)

  def apply(i: Long): MapId = i

  given Conversion[MapId, Long]:
    def apply(s: MapId): Long = s

  given Ordering[MapId]:
    def compare(x: MapId, y: MapId): Int = x.compare(y)

  given CanEqual[MapId, MapId] = CanEqual.derived

  extension (inline v: MapId) inline def value: Long = v

// ConnectionId
opaque type ConnectionId = Long

object ConnectionId:
  val Invalid: ConnectionId = ConnectionId(-1)

  def apply(i: Long): ConnectionId = i

  given Conversion[ConnectionId, Long]:
    def apply(s: ConnectionId): Long = s

  given Ordering[ConnectionId]:
    def compare(x: ConnectionId, y: ConnectionId): Int = x.compare(y)

  given CanEqual[ConnectionId, ConnectionId] = CanEqual.derived

  extension (inline v: ConnectionId) inline def value: Long = v

// NoteId
opaque type IntelNoteId = Int

object IntelNoteId:
  val Invalid: IntelNoteId = IntelNoteId(-1)

  def apply(i: Int): IntelNoteId = i

  given Conversion[IntelNoteId, Int]:
    def apply(s: IntelNoteId): Int = s

  given Ordering[IntelNoteId]:
    def compare(x: IntelNoteId, y: IntelNoteId): Int = x.compare(y)

  given CanEqual[IntelNoteId, IntelNoteId] = CanEqual.derived

  extension (inline v: IntelNoteId) inline def value: Int = v

  // IntelStructureId
opaque type IntelStructureId = Int

object IntelStructureId:
  val Invalid: IntelStructureId = IntelStructureId(-1)

  def apply(i: Int): IntelStructureId = i

  given Conversion[IntelStructureId, Int]:
    def apply(s: IntelStructureId): Int = s

  given Ordering[IntelStructureId]:
    def compare(x: IntelStructureId, y: IntelStructureId): Int = x.compare(y)

  given CanEqual[IntelStructureId, IntelStructureId] = CanEqual.derived

  extension (inline v: IntelStructureId) inline def value: Int = v

// IntelPingId
opaque type IntelPingId = Int

object IntelPingId:
  val Invalid: IntelPingId = IntelPingId(-1)

  def apply(i: Int): IntelPingId = i

  given Conversion[IntelPingId, Int]:
    def apply(s: IntelPingId): Int = s

  given Ordering[IntelPingId]:
    def compare(x: IntelPingId, y: IntelPingId): Int = x.compare(y)

  given CanEqual[IntelPingId, IntelPingId] = CanEqual.derived

  extension (inline v: IntelPingId) inline def value: Int = v

// SigId

opaque type SigId = String

object SigId:
  def apply(s: String): SigId = s

  given Conversion[SigId, String]:
    def apply(s: SigId): String = s

  given Ordering[SigId]:
    def compare(x: SigId, y: SigId): Int = x.compare(y)

  given CanEqual[SigId, SigId] = CanEqual.derived

  extension (inline v: SigId) inline def value: String = v

// SystemId

opaque type SystemId = Long

object SystemId:
  def apply(v: Long): SystemId = v

  given Conversion[SystemId, Long]:
    def apply(v: SystemId): Long = v

  given Ordering[SystemId]:
    def compare(x: SystemId, y: SystemId): Int = x.compare(y)

  given CanEqual[SystemId, SystemId] = CanEqual.derived

  extension (inline v: SystemId) inline def value: Long = v

// UserId

opaque type UserId = Long

object UserId:
  val Invalid = UserId(-1)

  def apply(v: Long): UserId = v

  given Conversion[UserId, Long]:
    def apply(v: UserId): Long = v

  given Ordering[UserId]:
    def compare(x: UserId, y: UserId): Int = x.compare(y)

  given CanEqual[UserId, UserId] = CanEqual.derived

  extension (inline v: UserId) inline def value: Long = v

/** Models partial updates more precisely than Option[A] can
  */
enum UnknownOrUnset[A]:
  case Unknown()
  case Unset()
  case Known(value: A)

  inline def asOption: Option[A] =
    this match
      case Known(value) => Some(value)
      case _            => None

  inline def updateWith(update: UnknownOrUnset[A]) =
    (this, update) match
      case (_, _: Unknown[A]) => this
      case _                  => update

object UnknownOrUnset:
  inline def apply[A](value: A): UnknownOrUnset[A] = UnknownOrUnset.Known(value)
  inline def apply[A](option: Option[A]): UnknownOrUnset[A] =
    option match
      case Some(value) => UnknownOrUnset.Known(value)
      case None        => UnknownOrUnset.Unset()
