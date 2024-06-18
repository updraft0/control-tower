package org.updraft0.controltower.constant

enum SpaceType derives CanEqual:
  case Known, Wormhole, Pochven, Abyssal, Internal

enum WormholeClass(val tag: String, val value: Int, val spaceType: SpaceType) derives CanEqual:
  // "normal" w-space
  case C1 extends WormholeClass("C1", 1, SpaceType.Wormhole)
  case C2 extends WormholeClass("C2", 2, SpaceType.Wormhole)
  case C3 extends WormholeClass("C3", 3, SpaceType.Wormhole)
  case C4 extends WormholeClass("C4", 4, SpaceType.Wormhole)
  case C5 extends WormholeClass("C5", 5, SpaceType.Wormhole)
  case C6 extends WormholeClass("C6", 6, SpaceType.Wormhole)
  // k-space
  case H  extends WormholeClass("H", 7, SpaceType.Known)
  case L  extends WormholeClass("L", 8, SpaceType.Known)
  case NS extends WormholeClass("NS", 9, SpaceType.Known)
  // internal
  case Internal10 extends WormholeClass("?", 10, SpaceType.Internal)
  case Internal11 extends WormholeClass("?", 11, SpaceType.Internal)
  // thera
  case Thera extends WormholeClass("T", 12, SpaceType.Wormhole)
  // frig shattered
  case ShatteredFrig extends WormholeClass("SF", 13, SpaceType.Wormhole)
  // drifter
  case SentinelDrifter extends WormholeClass("D", 14, SpaceType.Wormhole)
  case BarbicanDrifter extends WormholeClass("D", 15, SpaceType.Wormhole)
  case VidetteDrifter  extends WormholeClass("D", 16, SpaceType.Wormhole)
  case ConfluxDrifter  extends WormholeClass("D", 17, SpaceType.Wormhole)
  case RedoubtDrifter  extends WormholeClass("D", 18, SpaceType.Wormhole)
  // abyssal space
  case Void      extends WormholeClass("V", 19, SpaceType.Abyssal)
  case Abyssal20 extends WormholeClass("A", 20, SpaceType.Abyssal)
  case Abyssal21 extends WormholeClass("A", 21, SpaceType.Abyssal)
  case Abyssal22 extends WormholeClass("A", 22, SpaceType.Abyssal)
  case Abyssal23 extends WormholeClass("A", 23, SpaceType.Abyssal)
  // pochven
  case Pochven extends WormholeClass("P", 25, SpaceType.Pochven)

  def isDrifter: Boolean = value > 13 && value < 19

object WormholeClasses:
  lazy val ById = WormholeClass.values.map(c => c.value -> c).toMap

enum WormholeEffect(val typeId: Long) derives CanEqual:
  case Magnetar    extends WormholeEffect(30574)
  case BlackHole   extends WormholeEffect(30575)
  case RedGiant    extends WormholeEffect(30576)
  case Pulsar      extends WormholeEffect(30577)
  case Cataclysmic extends WormholeEffect(30670)
  case WolfRayet   extends WormholeEffect(30669)

object WormholeEffects:
  lazy val ById = WormholeEffect.values.map(e => e.typeId -> e).toMap
