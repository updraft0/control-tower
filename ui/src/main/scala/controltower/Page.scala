package controltower

import org.getshaka.nativeconverter.NativeConverter

enum Page derives CanEqual, NativeConverter:
  case Landing
  case Map(name: String, character: String)
  case MapEditor

extension (p: Page)
  def pageTitle: String = p match
    case Page.Landing              => "Welcome :: CT"
    case Page.Map(name, character) => s"${character}@${name} :: CT"
    case Page.MapEditor            => "Map Editor :: CT"
