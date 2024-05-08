package controltower.backend

import com.raquo.laminar.api.L.*
import org.updraft0.controltower.constant.CharacterId
import sttp.client3.UriContext

object ESI:

  private val imageBaseUrl = uri"https://images.evetech.net"

  def characterImage(id: CharacterId, name: String, size: Int = 128) =
    img(
      src := uri"$imageBaseUrl/characters/$id/portrait?size=$size".toString,
      cls := "character-image",
      alt := s"$name"
    )

  def characterImageS(idName: Signal[(CharacterId, String)], size: Int = 128) =
    img(
      src <-- idName.map(_._1).map(cid => uri"$imageBaseUrl/characters/$cid/portrait?size=$size".toString),
      cls := "character-image",
      alt <-- idName.map(_._2)
    )

  def typeIcon(id: Long, size: Int = 16) =
    img(
      src := uri"$imageBaseUrl/types/$id/icon".toString,
      cls := "type-icon",
      cls := s"type-icon-${size}-round",
      alt := s"type $id"
    )
