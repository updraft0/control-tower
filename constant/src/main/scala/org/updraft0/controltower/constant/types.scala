package org.updraft0.controltower.constant

import scala.language.implicitConversions

// TODO: move to typeclass encoding

// CharacterId
opaque type CharacterId = Long

object CharacterId:
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
