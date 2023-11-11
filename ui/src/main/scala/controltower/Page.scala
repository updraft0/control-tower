package controltower

import zio.json.*

enum Page:
  case Landing
  case Map(characterId: Long, mapId: Long)

object Page:
  given JsonCodec[Page] = JsonCodec.derived

extension (p: Page)
  def pageTitle: String = p match
    case Page.Landing            => "ControlTower :: Landing"
    case Page.Map(charId, mapId) => s"ControlTower :: Map(${charId}/${mapId})"
