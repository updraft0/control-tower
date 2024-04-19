package org.updraft0.controltower.server.auth

import org.updraft0.controltower.db.model.{AuthCharacter, AuthUser, CharacterAuthToken, UserCharacter, UserSession}
import org.updraft0.controltower.db.query.auth
import org.updraft0.controltower.db.query
import org.updraft0.controltower.server.Config
import org.updraft0.controltower.server.db.AuthQueries
import org.updraft0.esi.client.{EsiClient, JwtAuthResponse}
import zio.*

import java.util.UUID
import scala.annotation.unused

object Users:
  type Env = Config & javax.sql.DataSource & EsiClient

  def loginCallback(authCode: String, sessionId: UUID): ZIO[Env, Throwable, Unit] =
    Esi.initialTokenAndUserData(authCode).mapError(_.asThrowable).flatMap { (jwt, tokenMeta) =>
      query.transaction(
        (AuthQueries.getUserCharactersBySessionId(sessionId) <*>
          AuthQueries.getUserByCharacterId(tokenMeta.characterId))
          .flatMap {
            case (None, None)                  => newUser(jwt, tokenMeta, sessionId)
            case (None, Some((user, char)))    => newSession(jwt, tokenMeta, user, char, sessionId)
            case (Some((_, user, _)), None)    => addCharacterToUser(jwt, tokenMeta, user)
            case (Some(_), Some((user, char))) => updateRefreshToken(jwt, tokenMeta, user, char)
          }
      )
    }

  private def newUser(jwt: JwtAuthResponse, tokenMeta: EsiTokenMeta, sessionId: UUID): ZIO[Env, Throwable, Unit] =
    for
      char   <- getUserCharacterFromEsi(tokenMeta.characterId, tokenMeta.characterOwnerHash)
      userId <- auth.insertUser(displayName = char.name)
      _      <- auth.insertUserCharacter(UserCharacter(userId, char.id))
      _      <- auth.insertCharacter(char)
      _      <- newUserSession(userId, sessionId).flatMap(auth.insertUserSession)
      _ <- auth.insertAuthToken(
        CharacterAuthToken(tokenMeta.characterId, jwt.accessToken.value, jwt.refreshToken, tokenMeta.expiry, None)
      )
    yield ()

  private def newSession(
      @unused jwt: JwtAuthResponse,
      @unused tokenMeta: EsiTokenMeta,
      user: AuthUser,
      @unused char: AuthCharacter,
      sessionId: UUID
  ): ZIO[Env, Throwable, Unit] =
    newUserSession(user.id, sessionId).flatMap(auth.insertUserSession).unit

  private def addCharacterToUser(
      @unused jwt: JwtAuthResponse,
      @unused tokenMeta: EsiTokenMeta,
      @unused user: AuthUser
  ): ZIO[Env, Throwable, Unit] = ??? // FIXME implement this!

  private def updateRefreshToken(
      @unused jwt: JwtAuthResponse,
      @unused tokenMeta: EsiTokenMeta,
      @unused user: AuthUser,
      @unused char: AuthCharacter
  ): ZIO[Env, Throwable, Unit] = ZIO.unit // FIXME implement this?

  private def newUserSession(userId: Long, sessionId: UUID) =
    for
      conf      <- ZIO.service[Config]
      createdAt <- ZIO.clockWith(_.instant)
      expiresAt = createdAt.plus(conf.auth.sessionExpiry)
    yield UserSession(
      sessionId = sessionId,
      userId = userId,
      createdAt = createdAt,
      expiresAt = expiresAt,
      lastSeenAt = Some(createdAt),
      ipAddress = None,
      userAgent = None
    )

  private def getUserCharacterFromEsi(characterId: Long, ownerHash: String) =
    (EsiClient.withZIO(_.getCharacter(characterId)) <&>
      EsiClient.withZIO(_.getCharacterAffiliations(List(characterId)))).map { (apiChar, apiAffiliations) =>
      assert(apiAffiliations.length == 1, "expecting len(affiliations) == 1")
      AuthCharacter(
        ownerHash = ownerHash,
        id = characterId,
        name = apiChar.name,
        corporationId = apiAffiliations.head.corporationId,
        allianceId = apiAffiliations.head.allianceId,
        bornAt = apiChar.birthday,
        addedAt = None,
        updatedAt = None,
        lastOnlineAt = None
      )
    }
