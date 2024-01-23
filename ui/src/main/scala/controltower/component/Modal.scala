package controltower.component

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.airstream.ownership.ManualOwner
import org.scalajs.dom
import org.scalajs.dom.MouseEvent

// note: thanks to @yurique for this idea
//
//class Modal(mods: Mod[ReactiveHtmlElement[dom.HTMLDialogElement]]*) extends Base:
//
//  private val contentVar = Var(Option.empty[Element])
//  private val closeBus   = new EventBus[dom.UIEvent]()
//
//  def setContent(el: Element): Unit      = contentVar.set(Some(el))
//  val content: Observer[Option[Element]] = contentVar.writer
//  def clear(): Unit                      = contentVar.set(None)
//  val clearObserver: Observer[Unit]      = contentVar.writer.contramap[Unit](_ => None)
//  val onClose: EventStream[dom.UIEvent]  = closeBus.events
//
//  private val dialog = dialogTag(
//    mods,
//    child.maybe <-- contentVar
//  )
//
//  dialog.amend(
//    contentVar --> {
//      case None    => dialog.ref.close()
//      case Some(_) => dialog.ref.showModal()
//    },
//    Modal.onClose --> closeBus
//  )

//  val el: ReactiveHtmlElement[dom.HTMLDialogElement] = dialog

object Modal:
  lazy val onClose: EventProp[dom.UIEvent] = eventProp("close")

//  def create(mods: Mod[ReactiveHtmlElement[dom.HTMLDialogElement]]*): Modal = new Modal(mods*)

  def show(
      content: (Observer[Unit], Owner) => Element,
      clickCloses: Boolean,
      mods: Mod[ReactiveHtmlElement[dom.HTMLDialogElement]]*
  ): Unit =
    val closeBus = new EventBus[Unit]
    val owner    = new ManualOwner()
    val dialog = dialogTag(
      mods,
      inContext(self => onClick --> (ev => if (clickCloses) onDialogClickClose(ev, self.ref, closeBus))),
      content(closeBus.writer, owner)
    )
    dom.document.body.append(dialog.ref)
    val detached = renderDetached(dialog, activateNow = true)
    dialog.amend(
      onClose --> { _ =>
        owner.killSubscriptions()
        detached.deactivate()
        dom.document.body.removeChild(dialog.ref)
      },
      closeBus.events --> { _ =>
        dialog.ref.close()
      }
    )
    dialog.ref.showModal()

  def showConfirmation(title: String, description: String, onOk: Observer[Unit]) =
    show(
      (closeMe, _) =>
        div(
          cls := "dialog-view",
          cls := "confirm-dialog-view",
          div(cls := "dialog-header", title),
          div(cls := "dialog-body", description),
          button(
            tpe := "button",
            "Cancel",
            onClick.stopPropagation.mapToUnit --> closeMe
          ),
          button(
            tpe := "button",
            "OK",
            onClick.stopPropagation.mapToUnit --> { _ =>
              closeMe.onNext(()); onOk.onNext(())
            }
          )
        ),
      clickCloses = false
    )

// thanks to https://stackoverflow.com/a/57463812
private inline def onDialogClickClose[Ref <: org.scalajs.dom.Element](
    ev: MouseEvent,
    ref: Ref,
    closeBus: EventBus[Unit]
) =
  if (ev.target == ref) {
    val rec = ref.getBoundingClientRect()
    val inDialog =
      rec.top <= ev.clientY && ev.clientY <= rec.top + rec.height && rec.left <= ev.clientX && ev.clientX <= rec.left + rec.width
    if (!inDialog) closeBus.writer.onNext(())
  }
