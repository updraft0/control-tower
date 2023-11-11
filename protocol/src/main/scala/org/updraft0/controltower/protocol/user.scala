package org.updraft0.controltower.protocol

enum MapRole:
  case Viewer, Editor, Admin

case class UserCharacter(name: String, characterId: Long, corporationId: Long, allianceId: Option[Long])
case class UserCharacterMap(characterId: Long, mapId: Long, mapName: String, mapRole: MapRole)

case class UserInfo(userId: Long, displayName: String, characters: List[UserCharacter], maps: List[UserCharacterMap])