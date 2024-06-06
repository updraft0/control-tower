package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.*

enum MapRole derives CanEqual:
  case Viewer, Editor, Admin

case class UserCharacter(
    name: String,
    characterId: CharacterId,
    corporationId: CorporationId,
    allianceId: Option[AllianceId]
)
case class UserCharacterMap(characterId: CharacterId, mapId: Long, mapName: String, mapRole: MapRole)

case class UserInfo(userId: Long, displayName: String, characters: List[UserCharacter], maps: List[UserCharacterMap])

/** All user-configurable global UI preferences
  */
case class UserPreferences(clickResetsSelection: Boolean)

object UserPreferences:
  // TODO: make configurable
  val Default = UserPreferences(clickResetsSelection = false)
