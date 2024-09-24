package controltower.page.map

import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.{
  IntelStance,
  MapRequest,
  NewSystemSignature,
  WormholeMassSize,
  WormholeMassStatus
}

sealed trait SingleSystemAction:
  val systemId: SystemId

sealed trait SingleConnectionAction:
  val connectionId: ConnectionId

/** All actions on the map are represented by a case here. These actions are then transformed to map requests (if they
  * are valid)
  */
enum MapAction derives CanEqual:

  /** Add a connection between two systems
    */
  case AddConnection(fromSystemId: SystemId, toSystemId: SystemId)

  /** Change mass status (crit/reduced/etc.) on a connection
    */
  case ConnectionMassStatusChange(connectionId: ConnectionId, status: WormholeMassStatus)
      extends MapAction
      with SingleConnectionAction

  /** Change mass size (S, M, L, XL) on a connection
    */
  case ConnectionMassSizeChange(connectionId: ConnectionId, size: WormholeMassSize)
      extends MapAction
      with SingleConnectionAction

  /** Toggle the EOL status of a connection
    */
  case ConnectionEolToggle(connectionId: ConnectionId) extends MapAction with SingleConnectionAction

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
  case RemoveMultiple(systemIds: Array[SystemId])

  /** Remove a single connection from the map
    */
  case RemoveConnection(connectionId: ConnectionId)

  /** Reposition a system (e.g. via dragging)
    */
  case Reposition(systemId: SystemId, x: Double, y: Double) extends MapAction with SingleSystemAction

  /** Select a system
    */
  case Select(systemId: Option[SystemId])

  /** Select unpinned systems
    */
  case SelectUnpinned

  /** Toggle bulk selection for a system
    */
  case ToggleBulkSelection(systemId: SystemId) extends MapAction with SingleSystemAction

  /** Toggle whether a system is pinned on the map
    */
  case TogglePinned(systemId: SystemId) extends MapAction with SingleSystemAction

  /** Add/update system signature
    */
  case AddSignature(systemId: SystemId, signature: NewSystemSignature) extends MapAction with SingleSystemAction

  /** Update signatures
    */
  case UpdateSignatures(systemId: SystemId, replaceAll: Boolean, signatures: Array[NewSystemSignature])
      extends MapAction
      with SingleSystemAction

  /** Remove subset of signatures in system
    */
  case RemoveSignatures(systemId: SystemId, signatureIds: Set[SigId]) extends MapAction with SingleSystemAction

  /** Clear all signatures in system
    */
  case RemoveAllSignatures(systemId: SystemId) extends MapAction with SingleSystemAction

/** Error shown to user before they can perform an action
  */
enum MapActionError:
  /** Connection changes without a linked signature cannot be made (due to choice of storage model)
    */
  case UnableToChangeConnectionNoLinkedSignature
