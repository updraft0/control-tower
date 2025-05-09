package controltower.dialog

import com.raquo.laminar.api.L.*
import controltower.*
import controltower.backend.ControlTowerBackend
import org.updraft0.controltower.protocol.*
import org.updraft0.controltower.constant.UserId
import dev.cheleb.scalamigen.*
import org.scalajs.dom

import scala.annotation.unused
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

object EditUserPreferencesView:
  // TODO: tooltips to explain what settings mean

  given Form[MapPreferences] = Form
    .autoDerived[MapPreferences]
    .withPanelConfig(label = None, asTable = true)

  given Form[SignaturePreferences] = Form
    .autoDerived[SignaturePreferences]
    .withPanelConfig(label = None, asTable = true)

  given Form[UserPreferences] = Form
    .autoDerived[UserPreferences]
    .withPanelConfig(label = None, asTable = false)
    .withFieldName("something?")

  given WidgetFactory = LaminarWidgetFactory

  def apply(@unused userId: UserId, prefs: UserPreferences, closeMe: Observer[Unit])(using
      ct: ControlTowerBackend
  ): HtmlElement =
    val prefsVar = Var(prefs)
    div(
      cls := "dialog-body",
      span(cls := "header", "Preferences"),
      prefsVar.asForm,
      button(
        tpe := "button",
        cls := "update",
        "Save",
        onClick.mapToUnit.compose(_.withCurrentValueOf(prefsVar.signal)) --> (prefs =>
          ct
            .updatePreferences(prefs)
            .onComplete:
              case Failure(ex) =>
                dom.console.error("failed to save preferences", ex)
                closeMe.onNext(())
              case Success(Left(error)) =>
                dom.console.error(s"failed to save preferences: reason $error")
                closeMe.onNext(())
              case Success(Right(_)) =>
                closeMe.onNext(())
        )
      )
    )
