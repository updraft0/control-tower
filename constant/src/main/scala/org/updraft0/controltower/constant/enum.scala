package org.updraft0.controltower.constant

enum SpaceType derives CanEqual:
  case Known, Wormhole, Pochven, Abyssal, Internal

enum WormholeClass(val value: Int, val spaceType: SpaceType) derives CanEqual:
  // "normal" w-space
  case C1 extends WormholeClass(1, SpaceType.Wormhole)
  case C2 extends WormholeClass(2, SpaceType.Wormhole)
  case C3 extends WormholeClass(3, SpaceType.Wormhole)
  case C4 extends WormholeClass(4, SpaceType.Wormhole)
  case C5 extends WormholeClass(5, SpaceType.Wormhole)
  case C6 extends WormholeClass(6, SpaceType.Wormhole)
  // k-space
  case H  extends WormholeClass(7, SpaceType.Known)
  case L  extends WormholeClass(8, SpaceType.Known)
  case NS extends WormholeClass(9, SpaceType.Known)
  // internal
  case Internal10 extends WormholeClass(10, SpaceType.Internal)
  case Internal11 extends WormholeClass(11, SpaceType.Internal)
  // thera
  case Thera extends WormholeClass(12, SpaceType.Wormhole)
  // frig shattered
  case ShatteredFrig extends WormholeClass(13, SpaceType.Wormhole)
  // drifter
  case SentinelDrifter extends WormholeClass(14, SpaceType.Wormhole)
  case BarbicanDrifter extends WormholeClass(15, SpaceType.Wormhole)
  case VidetteDrifter  extends WormholeClass(16, SpaceType.Wormhole)
  case ConfluxDrifter  extends WormholeClass(17, SpaceType.Wormhole)
  case RedoubtDrifter  extends WormholeClass(18, SpaceType.Wormhole)
  // abyssal space
  case Void      extends WormholeClass(19, SpaceType.Abyssal)
  case Abyssal20 extends WormholeClass(20, SpaceType.Abyssal)
  case Abyssal21 extends WormholeClass(21, SpaceType.Abyssal)
  case Abyssal22 extends WormholeClass(22, SpaceType.Abyssal)
  case Abyssal23 extends WormholeClass(23, SpaceType.Abyssal)
  // pochven
  case Pochven extends WormholeClass(25, SpaceType.Pochven)

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
