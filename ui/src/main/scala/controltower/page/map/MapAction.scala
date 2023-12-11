package controltower.page.map

import org.updraft0.controltower.protocol.{IntelStance, MapRequest}

type SystemId = Long

sealed trait SingleSystemAction:
  val systemId: SystemId

/** All actions on the map are represented by a case here. These actions are then transformed to map requests (if they
  * are valid)
  */
enum MapAction:

  /** Directly send a request to the server (fallback case)
    */
  case Direct(req: MapRequest)

  /** Change intel information for a system
    */
  case IntelChange(systemId: SystemId, intelStance: IntelStance) extends MapAction with SingleSystemAction

  /** Rename a system
    */
  case Rename(systemId: SystemId, name: Option[String]) extends MapAction with SingleSystemAction

  /** Remove a system from the map
    */
  case Remove(systemId: SystemId) extends MapAction with SingleSystemAction

  /** Reposition a system (e.g. via dragging)
    */
  case Reposition(systemId: SystemId, x: Double, y: Double) extends MapAction with SingleSystemAction

  /** Select a system
    */
  case Select(systemId: Option[SystemId])

  /** Toggle whether a system is pinned on the map
    */
  case TogglePinned(systemId: SystemId) extends MapAction with SingleSystemAction
