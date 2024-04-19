package controltower

import zio.json.*

enum Page derives CanEqual:
  case Landing
  case Map(characterId: Long, name: String)

object Page:
  given JsonCodec[Page] = JsonCodec.derived

extension (p: Page)
  def pageTitle: String = p match
    case Page.Landing              => "Welcome to ControlTower"
    case Page.Map(charId, mapName) => s"${mapName}/${charId} (CT)"
