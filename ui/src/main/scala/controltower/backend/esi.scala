package controltower.backend

import com.raquo.laminar.api.L.{*, given}
import sttp.client3.UriContext
import sttp.model.Uri

object ESI:

  private val imageBaseUrl = uri"https://images.evetech.net"

  def characterImage(id: Long, name: String, size: Int = 128) =
    img(
      src := uri"$imageBaseUrl/characters/$id/portrait?size=$size".toString,
      cls := "character-image",
      alt := s"$name"
    )

  def typeIcon(id: Long, size: Int = 16) =
    img(
      src := uri"$imageBaseUrl/types/$id/icon".toString,
      cls := "type-icon",
      cls := s"type-icon-${size}-round",
      alt := s"type $id"
    )
