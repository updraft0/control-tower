package controltower

import zio.json.*

enum Page:
  case Landing
  case Map(characterId: Long, name: String)

object Page:
  given JsonCodec[Page] = JsonCodec.derived

extension (p: Page)
  def pageTitle: String = p match
    case Page.Landing              => "ControlTower :: Landing"
    case Page.Map(charId, mapName) => s"ControlTower :: ${mapName}"
