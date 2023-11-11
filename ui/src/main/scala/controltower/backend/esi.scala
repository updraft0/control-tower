package controltower.backend

import com.raquo.laminar.api.L.{*, given}
import sttp.client3.UriContext
import sttp.model.Uri

object ESI:

  private val imageBaseUrl = uri"https://images.evetech.net"

  def characterImage(id: Long, name: String, size: Int = 128) =
    img(
      src       := uri"$imageBaseUrl/characters/$id/portrait?size=$size".toString,
      className := "character-image",
      alt       := s"$name"
    )
