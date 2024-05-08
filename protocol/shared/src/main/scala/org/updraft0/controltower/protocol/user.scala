package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.CharacterId

enum MapRole derives CanEqual:
  case Viewer, Editor, Admin

case class UserCharacter(name: String, characterId: CharacterId, corporationId: Long, allianceId: Option[Long])
case class UserCharacterMap(characterId: CharacterId, mapId: Long, mapName: String, mapRole: MapRole)

case class UserInfo(userId: Long, displayName: String, characters: List[UserCharacter], maps: List[UserCharacterMap])
