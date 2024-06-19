package controltower.page.map

import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.{IntelStance, MapRequest, NewSystemSignature, SystemId}

sealed trait SingleSystemAction:
  val systemId: SystemId

/** All actions on the map are represented by a case here. These actions are then transformed to map requests (if they
  * are valid)
  */
enum MapAction:

  /** Add a connection between two systems
    */
  case AddConnection(fromSystemId: SystemId, toSystemId: SystemId) extends MapAction

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

  /** Remove a system from the map
    */
  case RemoveMultiple(systemIds: Array[SystemId]) extends MapAction

  /** Remove a single connection from the map
    */
  case RemoveConnection(connectionId: ConnectionId) extends MapAction

  /** Reposition a system (e.g. via dragging)
    */
  case Reposition(systemId: SystemId, x: Double, y: Double) extends MapAction with SingleSystemAction

  /** Select a system
    */
  case Select(systemId: Option[SystemId])

  /** Toggle bulk selection for a system
    */
  case ToggleBulkSelection(systemId: SystemId)

  /** Toggle whether a system is pinned on the map
    */
  case TogglePinned(systemId: SystemId) extends MapAction with SingleSystemAction

  /** Add/update system signature
    */
  case AddSignature(systemId: SystemId, signature: NewSystemSignature) extends MapAction with SingleSystemAction

  /** Update signatures
    */
  case UpdateSignatures(systemId: SystemId, replaceAll: Boolean, signatures: Array[NewSystemSignature])

  /** Remove subset of signatures in system
    */
  case RemoveSignatures(systemId: SystemId, signatureIds: Set[SigId])

  /** Clear all signatures in system
    */
  case RemoveAllSignatures(systemId: SystemId)
