package controltower.page.map

import org.updraft0.controltower.protocol.MapRole

/** Client-side role checks (for disabling certain actions)
  */
object RoleController:
  def canAddSystem(role: MapRole): Boolean         = notViewer(role)
  def canRemoveSystem(role: MapRole): Boolean      = notViewer(role)
  def canRenameSystem(role: MapRole): Boolean      = notViewer(role)
  def canRepositionSystem(role: MapRole): Boolean  = notViewer(role)
  def canPinUnpinSystem(role: MapRole): Boolean    = notViewer(role)
  def canUpdateIntelStance(role: MapRole): Boolean = notViewer(role)

  private inline def notViewer(role: MapRole) = role match
    case MapRole.Viewer => false
    case _              => true
