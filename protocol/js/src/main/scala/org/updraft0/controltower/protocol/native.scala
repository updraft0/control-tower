package org.updraft0.controltower.protocol

import org.getshaka.nativeconverter.NativeConverter
import org.getshaka.nativeconverter.ParseState
import org.updraft0.controltower.constant.*

import scala.scalajs.js

/** Conversions for some protocol objects to native JS types (mainly for storing in IndexedDB) */
object native:

  // constants
  given NativeConverter[SpaceType] = NativeConverter.derived
  given NativeConverter[WormholeClass] with
    extension (wc: WormholeClass) def toNative: js.Any = wc.value.toDouble
    def fromNative(ps: ParseState): WormholeClass =
      val idx = ps.json.asInstanceOf[Any] match
        case i: Int    => i
        case s: String => s.toIntOption.getOrElse(ps.fail("Int"))
        case _         => ps.fail("Int")
      WormholeClasses.ById.getOrElse(idx, ps.fail("WormholeClass.Id"))
  given NativeConverter[WormholeEffect] with
    extension (we: WormholeEffect) def toNative: js.Any = we.typeId.toDouble
    def fromNative(ps: ParseState): WormholeEffect =
      val idx = ps.json.asInstanceOf[Any] match
        case i: Int    => i.toLong
        case s: String => s.toLongOption.getOrElse(ps.fail("Long"))
        case _         => ps.fail("Long")
      WormholeEffects.ById.getOrElse(idx, ps.fail("WormholeEffect.Id"))

  // opaque
  given NativeConverter[SystemId]      = NativeConverter.apply[Long].asInstanceOf[NativeConverter[SystemId]]
  given NativeConverter[CorporationId] = NativeConverter.apply[Long].asInstanceOf[NativeConverter[CorporationId]]

  // reference
  given NativeConverter[Planet]           = NativeConverter.derived
  given NativeConverter[Station]          = NativeConverter.derived
  given NativeConverter[WormholeStatic]   = NativeConverter.derived
  given NativeConverter[SolarSystem]      = NativeConverter.derived
  given NativeConverter[Stargate]         = NativeConverter.derived
  given NativeConverter[Faction]          = NativeConverter.derived
  given NativeConverter[StationService]   = NativeConverter.derived
  given NativeConverter[SignatureGroup]   = NativeConverter.derived
  given NativeConverter[SignatureInGroup] = NativeConverter.derived
  given NativeConverter[StationOperation] = NativeConverter.derived
  given NativeConverter[WormholeType]     = NativeConverter.derived
  given NativeConverter[StarType]         = NativeConverter.derived
  given NativeConverter[ShipType]         = NativeConverter.derived
  given NativeConverter[Reference]        = NativeConverter.derived
