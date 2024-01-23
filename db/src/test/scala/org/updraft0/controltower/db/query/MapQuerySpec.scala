package org.updraft0.controltower.db.query

import io.getquill.MappedEncoding
import org.updraft0.controltower.constant.WormholeClass
import org.updraft0.controltower.db.model.*
import zio.test.*
import zio.test.TestAspect.samples

import scala.collection.immutable.BitSet

object MapQuerySpec extends ZIOSpecDefault:
  import map.given

  override def spec =
    // important to test that the constants stored in the db are stable
    suite("MappedEncoding")(
      test("for WormholeClass")(
        encodeDecodeMany(
          1  -> WormholeClass.C1,
          2  -> WormholeClass.C2,
          3  -> WormholeClass.C3,
          4  -> WormholeClass.C4,
          5  -> WormholeClass.C5,
          6  -> WormholeClass.C6,
          7  -> WormholeClass.H,
          8  -> WormholeClass.L,
          9  -> WormholeClass.NS,
          10 -> WormholeClass.Internal10,
          11 -> WormholeClass.Internal11,
          12 -> WormholeClass.Thera,
          13 -> WormholeClass.ShatteredFrig,
          14 -> WormholeClass.SentinelDrifter,
          15 -> WormholeClass.BarbicanDrifter,
          16 -> WormholeClass.VidetteDrifter,
          17 -> WormholeClass.ConfluxDrifter,
          18 -> WormholeClass.RedoubtDrifter,
          19 -> WormholeClass.Void,
          20 -> WormholeClass.Abyssal20,
          21 -> WormholeClass.Abyssal21,
          22 -> WormholeClass.Abyssal22,
          23 -> WormholeClass.Abyssal23,
          25 -> WormholeClass.Pochven
        )
      ),
      test("for ChainNamingStrategy")(
        encodeDecodeMany(0 -> ChainNamingStrategy.Manual)
      ),
      test("for IntelStance")(
        encodeDecodeMany(
          0 -> IntelStance.Unknown,
          1 -> IntelStance.Friendly,
          2 -> IntelStance.Hostile
        )
      ),
      test("for MapDisplayType")(
        encodeDecodeMany(0 -> MapDisplayType.Manual)
      ),
      test("for SignatureGroup")(
        encodeDecodeMany(
          0 -> SignatureGroup.Unknown,
          1 -> SignatureGroup.Combat,
          2 -> SignatureGroup.Data,
          3 -> SignatureGroup.Gas,
          4 -> SignatureGroup.Ghost,
          5 -> SignatureGroup.Ore,
          6 -> SignatureGroup.Relic,
          7 -> SignatureGroup.Wormhole
        )
      ),
      test("for WormholeMassSize")(
        encodeDecodeMany(
          0 -> WormholeMassSize.Unknown,
          1 -> WormholeMassSize.XL,
          2 -> WormholeMassSize.L,
          3 -> WormholeMassSize.M,
          4 -> WormholeMassSize.S
        )
      ),
      test("for WormholeMassStatus")(
        encodeDecodeMany(
          0 -> WormholeMassStatus.Unknown,
          1 -> WormholeMassStatus.Fresh,
          2 -> WormholeMassStatus.Reduced,
          3 -> WormholeMassStatus.Critical
        )
      ),
      test("for WormholeK162Type")(
        encodeDecodeMany(
          0 -> WormholeK162Type.Unknown,
          1 -> WormholeK162Type.Dangerous,
          2 -> WormholeK162Type.Deadly,
          3 -> WormholeK162Type.Hisec,
          4 -> WormholeK162Type.Losec,
          5 -> WormholeK162Type.Nullsec,
          6 -> WormholeK162Type.Thera
        )
      ),
      test("for Set[WormholeClass] - static")(
        encodeDecodeMany(
          0                           -> Set.empty[WormholeClass],
          2                           -> Set(WormholeClass.C1),
          6                           -> Set(WormholeClass.C1, WormholeClass.C2),
          8                           -> Set(WormholeClass.C3),
          (1 << 8 | 1 << 9 | 1 << 25) -> Set(WormholeClass.L, WormholeClass.NS, WormholeClass.Pochven)
        )
      ),
      test("for Set[WormholeClass] - generative")(
        check(Gen.listOf(Gen.elements(WormholeClass.values.toSeq*))) { classes =>
          val set = classes.toSet
          encodeDecode(BitSet(set.map(_.value).toSeq*).toBitMask(0).toInt, set)
        }
      ) @@ samples(10)
    )

private def encodeDecodeMany[I, O](hd: (I, O), xs: (I, O)*)(using MappedEncoding[I, O], MappedEncoding[O, I]) =
  TestResult.allSuccesses(encodeDecode(hd._1, hd._2), xs.map(io => encodeDecode(io._1, io._2))*)

private def encodeDecode[I, O](i: I, o: O)(using in: MappedEncoding[I, O], out: MappedEncoding[O, I]) =
  assertTrue(
    in.f(i) == o,
    out.f(o) == i
  )
