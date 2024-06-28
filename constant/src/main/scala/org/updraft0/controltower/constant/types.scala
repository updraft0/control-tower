package org.updraft0.controltower.constant

import scala.language.implicitConversions

// TODO: move to typeclass encoding

// TODO: TypeId

// CharacterId
opaque type CharacterId = Long

object CharacterId:
  val Invalid: CharacterId = CharacterId(-1)
  val System: CharacterId  = CharacterId(1)

  def apply(i: Long): CharacterId = i

  given Conversion[CharacterId, Long] with
    override def apply(s: CharacterId): Long = s

  given Ordering[CharacterId] with
    override def compare(x: CharacterId, y: CharacterId): Int = x.compare(y)

  given CanEqual[CharacterId, CharacterId] = CanEqual.derived

  extension (inline v: CharacterId) inline def value: Long = v

// CorporationId
opaque type CorporationId = Long

object CorporationId:
  def apply(i: Long): CorporationId = i

  given Conversion[CorporationId, Long] with
    override def apply(s: CorporationId): Long = s

  given Ordering[CorporationId] with
    override def compare(x: CorporationId, y: CorporationId): Int = x.compare(y)

  given CanEqual[CorporationId, CorporationId] = CanEqual.derived

  extension (inline v: CorporationId) inline def value: Long = v

// AllianceId
opaque type AllianceId = Long

object AllianceId:
  def apply(i: Long): AllianceId = i

  given Conversion[AllianceId, Long] with
    override def apply(s: AllianceId): Long = s

  given Ordering[AllianceId] with
    override def compare(x: AllianceId, y: AllianceId): Int = x.compare(y)

  given CanEqual[AllianceId, AllianceId] = CanEqual.derived

  extension (inline v: AllianceId) inline def value: Long = v

// MapId
opaque type MapId = Long

object MapId:
  val Invalid: MapId = MapId(-1)

  def apply(i: Long): MapId = i

  given Conversion[MapId, Long] with
    override def apply(s: MapId): Long = s

  given Ordering[MapId] with
    override def compare(x: MapId, y: MapId): Int = x.compare(y)

  given CanEqual[MapId, MapId] = CanEqual.derived

  extension (inline v: MapId) inline def value: Long = v

// ConnectionId
opaque type ConnectionId = Long

object ConnectionId:
  val Invalid: ConnectionId = ConnectionId(-1)

  def apply(i: Long): ConnectionId = i

  given Conversion[ConnectionId, Long] with
    override def apply(s: ConnectionId): Long = s

  given Ordering[ConnectionId] with
    override def compare(x: ConnectionId, y: ConnectionId): Int = x.compare(y)

  given CanEqual[ConnectionId, ConnectionId] = CanEqual.derived

  extension (inline v: ConnectionId) inline def value: Long = v

// SigId

opaque type SigId = String

object SigId:
  def apply(s: String): SigId = s

  given Conversion[SigId, String] with
    override def apply(s: SigId): String = s

  given Ordering[SigId] with
    override def compare(x: SigId, y: SigId): Int = x.compare(y)

  given CanEqual[SigId, SigId] = CanEqual.derived

// SystemId

opaque type SystemId = Long

object SystemId:
  def apply(v: Long): SystemId = v

  given Conversion[SystemId, Long] with
    override def apply(v: SystemId): Long = v

  given Ordering[SystemId] with
    override def compare(x: SystemId, y: SystemId): Int = x.compare(y)

  given CanEqual[SystemId, SystemId] = CanEqual.derived

  extension (inline v: SystemId) inline def value: Long = v

// UserId

opaque type UserId = Long

object UserId:
  val Invalid = UserId(-1)

  def apply(v: Long): UserId = v

  given Conversion[UserId, Long] with
    override def apply(v: UserId): Long = v

  given Ordering[UserId] with
    override def compare(x: UserId, y: UserId): Int = x.compare(y)

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
