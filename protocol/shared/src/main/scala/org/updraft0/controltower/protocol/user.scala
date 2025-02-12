package org.updraft0.controltower.protocol

import org.updraft0.controltower.constant.*

enum MapRole derives CanEqual:
  case Viewer, Editor, Admin

case class UserCharacter(
    name: String,
    characterId: CharacterId,
    corporationId: CorporationId,
    allianceId: Option[AllianceId],
    authTokenFresh: Boolean
)
case class UserCharacterMap(characterId: CharacterId, mapId: MapId, mapName: String, mapRole: MapRole)

case class UserInfo(
    userId: UserId,
    displayName: String,
    characters: List[UserCharacter],
    maps: List[UserCharacterMap],
    preferences: UserPreferences
)

/** Map specific preferences
  * @param clickResetsSelection
  *   Whether clicking outside a system node resets the selection
  */
case class MapPreferences(clickResetsSelection: Boolean)

/** Signature handling preferences
  * @param replaceSignaturesByDefault
  *   In the paste signature window, replace all signatures by default
  */
case class SignaturePreferences(replaceSignaturesByDefault: Boolean)

/** All user-configurable global UI preferences
  */
case class UserPreferences(map: MapPreferences, sig: SignaturePreferences)

object UserPreferences:
  val Default: UserPreferences = UserPreferences(
    map = MapPreferences(clickResetsSelection = false),
    sig = SignaturePreferences(replaceSignaturesByDefault = true)
  )
