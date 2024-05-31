package controltower

import zio.json.*

enum Page derives CanEqual:
  case Landing
  case Map(name: String, character: String)
  case MapEditor

object Page:
  given JsonCodec[Page] = JsonCodec.derived

extension (p: Page)
  def pageTitle: String = p match
    case Page.Landing              => "Welcome :: CT"
    case Page.Map(name, character) => s"${character}@${name} :: CT"
    case Page.MapEditor            => "Map Editor :: CT"
