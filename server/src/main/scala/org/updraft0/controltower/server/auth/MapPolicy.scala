package org.updraft0.controltower.server.auth

import org.updraft0.controltower.server.db.AuthQueries
import org.updraft0.controltower.server.db.MapQueries
import org.updraft0.controltower.db.model
import zio.*

object MapPolicy:
  type CharacterId = Long
  type MapId       = Long
  type Env         = javax.sql.DataSource

  def getMapsForCharacters(
      characterIds: List[CharacterId]
  ): ZIO[Env, Throwable, Map[CharacterId, List[(model.MapModel, model.MapRole)]]] =
    for
      mapRoleByCharacterId <- allowedMapIdsForCharacters(characterIds)
      mapsById <- MapQueries
        .getMapsById(mapRoleByCharacterId.flatMap(_._2.map(_._1)).toList)
        .map(_.map(m => m.id -> m).toMap)
    yield
      val y = mapRoleByCharacterId.map((c, ml) => c -> ml.map((id, role) => mapsById(id) -> role)).toMap
      y

  /** Fetch the allowed map ids that a set of characters can access
    */
  def allowedMapIdsForCharacters(
      characterIds: List[CharacterId]
  ): ZIO[Env, Throwable, Map[CharacterId, List[(MapId, model.MapRole)]]] =
    AuthQueries.getMapPoliciesForCharacter(characterIds).map(resolveCharacterMapPolicies)

  private[auth] def resolveCharacterMapPolicies(
      map: Map[CharacterId, List[model.MapPolicyMember]]
  ): Map[CharacterId, List[(MapId, model.MapRole)]] =
    map.map((c, p) => c -> resolveCharacterPoliciesToMapIds(c, p))

  private[auth] def resolveCharacterPoliciesToMapIds(
      characterId: CharacterId,
      policies: List[model.MapPolicyMember]
  ): List[(MapId, model.MapRole)] =
    policies
      .groupBy(_.mapId)
      .map { case (mapId, policies) =>
        policies
          .sortBy(p => policyRank(p.isDeny, p.memberType))
          .headOption
          .filter(!_.isDeny)
          .map(pm => pm.mapId -> pm.role)
      }
      .flatten
      .toList

private def policyRank(isDeny: Boolean, pt: model.PolicyMemberType): Int =
  (isDeny, pt) match
    case (true, _)                                   => 1
    case (false, model.PolicyMemberType.Character)   => 2
    case (false, model.PolicyMemberType.Corporation) => 3
    case (false, model.PolicyMemberType.Alliance)    => 4
