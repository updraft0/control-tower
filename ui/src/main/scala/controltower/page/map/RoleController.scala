package controltower.page.map

import org.updraft0.controltower.protocol.MapRole

/** Client-side role checks (for disabling certain actions)
  */
object RoleController:
  def canAddSystem(role: MapRole): Boolean            = notViewer(role)
  def canEditSignatures(role: MapRole): Boolean       = notViewer(role)
  def canRemoveSystem(role: MapRole): Boolean         = notViewer(role)
  def canRenameSystem(role: MapRole): Boolean         = notViewer(role)
  def canRepositionSystem(role: MapRole): Boolean     = notViewer(role)
  def canPinUnpinSystem(role: MapRole): Boolean       = notViewer(role)
  def canUpdateIntelStance(role: MapRole): Boolean    = notViewer(role)
  def canChangeConnections(role: MapRole): Boolean    = notViewer(role)
  def canAddIntelNote(role: MapRole): Boolean         = notViewer(role)
  def canEditIntelNote(role: MapRole): Boolean        = notViewer(role)
  def canRemoveIntelNote(role: MapRole): Boolean      = notViewer(role)
  def canPinIntelNote(role: MapRole): Boolean         = notViewer(role)
  def canAddIntelPing(role: MapRole): Boolean         = notViewer(role)
  def canRemoveIntelPing(role: MapRole): Boolean      = notViewer(role)
  def canAddIntelStructure(role: MapRole): Boolean    = notViewer(role)
  def canEditIntelStructure(role: MapRole): Boolean   = notViewer(role)
  def canRemoveIntelStructure(role: MapRole): Boolean = notViewer(role)

  private inline def notViewer(role: MapRole) = role match
    case MapRole.Viewer => false
    case _              => true
