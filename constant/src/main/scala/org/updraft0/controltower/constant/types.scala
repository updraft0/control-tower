package org.updraft0.controltower.constant

import scala.language.implicitConversions

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
