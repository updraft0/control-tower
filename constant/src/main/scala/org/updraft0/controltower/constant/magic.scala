package org.updraft0.controltower.constant

/** Magic constants live here instead of being sprinkled around the codebase. Might as well keep them in one place.
  */
object MagicConstant:
  // The grid snapping of a system map
  val GridSnapPx: Int = 10

  // The size of a system box on the map
  val SystemBoxSizeX: Int = 140
  val SystemBoxSizeY: Int = 40

  // Images
  val CharacterImageSize: Int = 32

  // Systems
  val Jita: SystemId = SystemId(30000142)

  // UI elements
  val DropdownDelayMs: Int = 500

  // TODO: figure out an optimal value for this
  val ConnectionCurviness: (Int, Int) = (25, 10)
  val ConnectionEndRadius: Int        = 4
