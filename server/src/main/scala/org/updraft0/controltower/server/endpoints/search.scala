package org.updraft0.controltower.server.endpoints

import org.updraft0.controltower.constant.*
import org.updraft0.controltower.protocol.*
import org.updraft0.controltower.server.Server.EndpointEnv
import org.updraft0.controltower.server.auth.Users
import org.updraft0.controltower.server.map.{IntelDataSource, toProtoCorporation, toProtoAlliance, toProtoCharacter}
import org.updraft0.esi.client
import sttp.tapir.ztapir.*
import zio.*
import zio.query.*

object SearchErrors:
  val StringTooShort = "search string too short"
  val NoAuthToken    = "no auth token found"

def searchEntity = Endpoints.searchEntity
  .zServerSecurityLogic(validateSessionString)
  .serverLogic(user =>
    (searchType, search) =>
      for
        _ <- ZIO.fail(SearchErrors.StringTooShort).unlessDiscard(search.length >= 3)
        authToken <- Users
          .loadAnyAuthTokenForUser(user.userId)
          .logError("failed to lookup auth token")
          .mapError(_ => SearchErrors.NoAuthToken)
          .someOrFail(SearchErrors.NoAuthToken)
        esiResults <- esiSearch(authToken.characterId, authToken.token, searchCriteria(searchType), search)
          .logError("failed to perform ESI search")
          .orElseSucceed(client.SearchResponse.Empty)
          .flatMap(sr => processEsiResults(sr).run.logError("failed to process ESI results").orElseSucceed(Nil))
      yield esiResults.toArray
  )

private def esiSearch(
    characterId: CharacterId,
    jwt: client.JwtString,
    searchCriteria: List[client.SearchCategory],
    search: String
) =
  ZIO
    .serviceWithZIO[client.EsiClient](esi => esi.search(jwt)((characterId, searchCriteria, search, false)))
    .tap(sr =>
      ZIO.logTrace(
        s"esi search response for '${search}'@[${searchCriteria.mkString(",")}] returned [alli:${sr.alliance.map(_.size).getOrElse(0)}, corp:${sr.corporation.map(_.size).getOrElse(0)}, char:${sr.corporation.map(_.size).getOrElse(0)}]"
      )
    )

// format: off
private def processEsiResults(result: client.SearchResponse) =
  (getCorporations(result.corporation.getOrElse(Nil)).map(_.flatten) <&>
    getAlliances(result.alliance.getOrElse(Nil)).map(_.flatten) <&>
    getCharacters(result.character.getOrElse(Nil)).map(_.flatten)).map:
      (corp, a, char) => corp ::: a ::: char
// format: on

private def getAlliances(ids: List[AllianceId]) =
  ZQuery.foreachBatched(ids): id =>
    IntelDataSource
      .getAllianceById(id)
      .map(_.map(a => SearchEntityResponse.OfAlliance(toProtoAlliance(a))))

private def getCorporations(ids: List[CorporationId]) =
  ZQuery.foreachBatched(ids): id =>
    IntelDataSource
      .getCorporationById(id)
      .flatMap:
        case Some(c) =>
          c.allianceId.fold(
            ZQuery.succeedNow(Some(SearchEntityResponse.OfCorporation(toProtoCorporation(c, alliance = None))))
          )(aId =>
            IntelDataSource
              .getAllianceById(aId)
              .map:
                case None           => Some(SearchEntityResponse.OfCorporation(toProtoCorporation(c, alliance = None)))
                case Some(alliance) => Some(SearchEntityResponse.OfCorporation(toProtoCorporation(c, Some(alliance))))
          )
        case None => ZQuery.succeedNow(None)

private def getCharacters(ids: List[CharacterId]) =
  ZQuery.foreachBatched(ids): id =>
    IntelDataSource
      .getCharacterById(id)
      .map:
        case Some(char) => Some(SearchEntityResponse.OfCharacter(toProtoCharacter(char)))
        case None       => None

private def searchCriteria(tpe: SearchType): List[client.SearchCategory] =
  tpe match
    case SearchType.Any =>
      List(client.SearchCategory.Character, client.SearchCategory.Corporation, client.SearchCategory.Alliance)
    case SearchType.Character   => List(client.SearchCategory.Character)
    case SearchType.Corporation => List(client.SearchCategory.Corporation)
    case SearchType.Alliance    => List(client.SearchCategory.Alliance)

def allSearchEndpoints: List[ZServerEndpoint[EndpointEnv, Any]] =
  List(
    searchEntity.widen[EndpointEnv]
  )
