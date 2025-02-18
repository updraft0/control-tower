package controltower.ui

import org.scalajs.dom.{Notification, NotificationOptions}

case class NotifyContent(title: String, body: String)

final class BrowserPermissions:

  private def notificationFrom(msg: NotifyContent): Notification =
    val opts = new NotificationOptions {}
    opts.body = msg.body
    opts.sticky = true
    opts.icon = "/control-tower.svg"
    new Notification(msg.title, opts)

  def notify(contents: NotifyContent): Boolean =
    if Notification.permission == "granted" then
      notificationFrom(contents)
      true
    else if Notification.permission == "denied" then false
    else
      var res = false
      Notification.requestPermission(permission =>
        if permission == "granted" then
          notificationFrom(contents)
          res = true
      )
      res
