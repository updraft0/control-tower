package controltower.component

import com.raquo.laminar.api.L.*

type Action = String

enum ContextMenuElement derives CanEqual:
  case MenuItem(
      action: Action,
      view: Element,
      disabled: Signal[Boolean] = Val(false),
      mod: Modifier[Element] = emptyMod
  )
  case Divider

final class ContextMenu(id: String, els: ContextMenuElement*):
  def view: Element = ul(
    idAttr := id,
    cls    := "context-menu",
    role   := "menu",
    els.map:
      case cmi: ContextMenuElement.MenuItem =>
        li(
          cls  := "menu-item",
          role := "menuitem",
          cls("disabled") <-- cmi.disabled,
          dataAttr("action") := cmi.action,
          cmi.mod,
          cmi.view
        )
      case ContextMenuElement.Divider =>
        li(cls := "divider")
  )
