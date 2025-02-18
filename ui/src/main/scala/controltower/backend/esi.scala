package controltower.backend

import com.raquo.laminar.api.L.*
import org.updraft0.controltower.constant.{AllianceId, CharacterId, CorporationId, TypeId}
import sttp.client3.UriContext

object ESI:

  private val imageBaseUrl = uri"https://images.evetech.net"

  def characterImage(id: CharacterId, name: String, size: Int = 128) =
    img(
      src   := uri"$imageBaseUrl/characters/$id/portrait?size=$size".toString,
      cls   := "character-image",
      alt   := name,
      title := name
    )

  def characterImageS(idName: Signal[(CharacterId, String)], size: Int = 128) =
    img(
      src <-- idName.map(_._1).map(cid => uri"$imageBaseUrl/characters/$cid/portrait?size=$size".toString),
      cls := "character-image",
      alt <-- idName.map(_._2),
      title <-- idName.map(_._2)
    )

  def corporationImage(id: CorporationId, name: String, size: Int = 128) =
    img(
      src   := uri"$imageBaseUrl/corporations/$id/logo?size=$size".toString,
      cls   := "corporation-image",
      alt   := name,
      title := name
    )

  def allianceImage(id: AllianceId, name: String, size: Int = 128) =
    img(
      src   := uri"$imageBaseUrl/alliances/$id/logo?size=$size".toString,
      cls   := "alliance-image",
      alt   := name,
      title := name
    )

  def typeIcon(id: TypeId, description: Option[String] = None, size: Int = 16) =
    img(
      src   := uri"$imageBaseUrl/types/${id.value}/icon".toString,
      cls   := "type-icon",
      cls   := s"type-icon-${size}-round",
      alt   := description.getOrElse(s"type ${id.value}"),
      title := description.getOrElse(s"type ${id.value}")
    )
