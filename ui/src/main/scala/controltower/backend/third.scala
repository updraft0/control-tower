package controltower.backend

import com.raquo.laminar.api.L.*
import sttp.client3.UriContext

object ThirdParty:
  private val Anoikis = uri"http://anoik.is"
  private val Dotlan  = uri"https://evemaps.dotlan.net"
  private val Zkill   = uri"https://zkillboard.com"

  def dotlanSystemLink(name: String) =
    a(
      href   := uri"$Dotlan/system/$name".toString,
      cls    := "dotlan-icon",
      target := "_blank"
    )

  def zkillSystemLink(systemId: Long) =
    a(
      href   := uri"$Zkill/system/$systemId".toString,
      cls    := "zkill-icon",
      target := "_blank"
    )

  def anoikisSystemLink(name: String) =
    a(
      href   := uri"$Anoikis/systems/$name".toString,
      cls    := "anoikis-icon",
      target := "_blank"
    )
