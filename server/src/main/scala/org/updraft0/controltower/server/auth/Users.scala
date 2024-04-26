package org.updraft0.controltower.server.auth

import org.updraft0.controltower.db.model.{AuthCharacter, AuthUser, UserCharacter, UserSession}
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.query.auth
import org.updraft0.controltower.db.query
import org.updraft0.controltower.server.Config
import org.updraft0.controltower.server.db.AuthQueries
import org.updraft0.esi.client.{EsiClient, JwtAuthResponse}
import zio.*

import java.security.SecureRandom
import java.time.Instant
import java.util.UUID

/** In-memory representation of auth for a character
  */
case class CharacterAuth(userId: Long, characterId: Long, token: String, refreshToken: Base64, expiry: Instant)

object Users:
  type Env = Config & javax.sql.DataSource & EsiClient & SecureRandom & TokenCrypto

  private val NonceLength = 12

  def loginCallback(authCode: String, sessionId: UUID): ZIO[Env, Throwable, Unit] =
    Esi.initialTokenAndUserData(authCode).mapError(_.asThrowable).flatMap { (jwt, tokenMeta) =>
      query.transaction(
        (AuthQueries.getUserCharactersBySessionId(sessionId) <*>
          AuthQueries.getUserByCharacterId(tokenMeta.characterId))
          .flatMap {
            case (None, None)                     => newUser(jwt, tokenMeta, sessionId)
            case (None, Some((user, char)))       => newSession(jwt, tokenMeta, user, char, sessionId)
            case (Some((session, user, _)), None) => addCharacterToUser(jwt, tokenMeta, user, session)
            case (Some(_), Some((user, char)))    => updateRefreshToken(jwt, tokenMeta, user, char)
          }
      )
    }

  def logoutCharacterFromUser(sessionId: UUID, characterId: Long): ZIO[Env, Throwable, Boolean] =
    AuthQueries
      .getUserCharactersBySessionId(sessionId)
      .flatMap:
        case Some((user, _, chars)) if chars.exists(_.id == characterId) =>
          auth.removeCharacterFromUser(user.userId, characterId).map(count => if (count > 0) true else false)
        case _ => ZIO.succeed(false)

  private def newUser(jwt: JwtAuthResponse, tokenMeta: EsiTokenMeta, sessionId: UUID): ZIO[Env, Throwable, Unit] =
    for
      char   <- getUserCharacterFromEsi(tokenMeta.characterId, tokenMeta.characterOwnerHash)
      userId <- auth.insertUser(displayName = char.name)
      _      <- auth.insertUserCharacter(UserCharacter(userId, char.id))
      _      <- auth.upsertCharacter(char)
      _      <- newUserSession(userId, sessionId).flatMap(auth.insertUserSession)
      token  <- encryptJwtResponse(tokenMeta, jwt)
      _      <- auth.upsertAuthToken(token)
    yield ()

  private def newSession(
      jwt: JwtAuthResponse,
      tokenMeta: EsiTokenMeta,
      user: AuthUser,
      char: AuthCharacter,
      sessionId: UUID
  ): ZIO[Env, Throwable, Unit] =
    for
      sess  <- newUserSession(user.id, sessionId)
      _     <- auth.insertUserSession(sess)
      _     <- auth.upsertCharacter(char)
      token <- encryptJwtResponse(tokenMeta, jwt)
      _     <- auth.upsertAuthToken(token)
    yield ()

  private def addCharacterToUser(
      jwt: JwtAuthResponse,
      tokenMeta: EsiTokenMeta,
      user: AuthUser,
      session: UserSession
  ): ZIO[Env, Throwable, Unit] =
    for
      char  <- getUserCharacterFromEsi(tokenMeta.characterId, tokenMeta.characterOwnerHash)
      _     <- auth.insertUserCharacter(UserCharacter(user.id, char.id))
      _     <- auth.upsertCharacter(char)
      token <- encryptJwtResponse(tokenMeta, jwt)
      _     <- auth.upsertAuthToken(token)
    yield ()

  private def updateRefreshToken(
      jwt: JwtAuthResponse,
      tokenMeta: EsiTokenMeta,
      user: AuthUser,
      char: AuthCharacter
  ): ZIO[Env, Throwable, Unit] =
    for
      char  <- getUserCharacterFromEsi(tokenMeta.characterId, tokenMeta.characterOwnerHash)
      _     <- auth.upsertCharacter(char)
      token <- encryptJwtResponse(tokenMeta, jwt)
      _     <- auth.upsertAuthToken(token)
    yield ()

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
      ipAddress = None, // FIXME implement this
      userAgent = None  // FIXME implement this
    )

  private def getUserCharacterFromEsi(characterId: Long, ownerHash: String) =
    (EsiClient.withZIO(_.getCharacter(characterId)) <&> EsiClient.withZIO(
      _.getCharacterAffiliations(List(characterId))
    ))
      .map: (apiChar, apiAffiliations) =>
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
          lastOnlineAt = None // FIXME implement this
        )

  private[auth] def encryptJwtResponse(tokenMeta: EsiTokenMeta, jwt: JwtAuthResponse) =
    for
      nonce      <- secureRandomBytesBase64(NonceLength)
      encAccess  <- ZIO.serviceWith[TokenCrypto](_.encrypt(nonce.toBytes, jwt.accessToken.value.getBytes))
      encRefresh <- ZIO.serviceWith[TokenCrypto](_.encrypt(nonce.toBytes, Base64.raw(jwt.refreshToken).toBytes))
      now        <- ZIO.clockWith(_.instant)
    yield model.CharacterAuthToken(
      characterId = tokenMeta.characterId,
      nonce = nonce.stringValue,
      token = Base64(encAccess).stringValue,
      refreshToken = Base64(encRefresh).stringValue,
      expiresAt = tokenMeta.expiry,
      updatedAt = Some(now)
    )

  private[auth] def decryptAuthToken(token: model.CharacterAuthToken) =
    val nonce = Base64.raw(token.nonce).toBytes
    for
      decAccess  <- ZIO.serviceWith[TokenCrypto](_.decrypt(nonce, Base64.raw(token.token).toBytes))
      decRefresh <- ZIO.serviceWith[TokenCrypto](_.decrypt(nonce, Base64.raw(token.refreshToken).toBytes))
    yield new String(decAccess) -> Base64(decRefresh)
