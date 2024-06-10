package org.updraft0.controltower.server.auth

import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model.{AuthCharacter, AuthUser, UserCharacter, UserSession}
import org.updraft0.controltower.db.model
import org.updraft0.controltower.db.query.auth
import org.updraft0.controltower.db.query
import org.updraft0.controltower.server.Config
import org.updraft0.controltower.server.db.AuthQueries
import org.updraft0.esi.client.{EsiClient, JwtAuthResponse, JwtString}
import zio.*

import java.security.SecureRandom
import java.time.Instant
import java.util.UUID

/** In-memory representation of auth for a character
  */
case class CharacterAuth(
    userId: UserId,
    characterId: CharacterId,
    token: JwtString,
    refreshToken: Base64,
    expiry: Instant
)

object Users:
  type Env = Config & javax.sql.DataSource & EsiClient & SecureRandom & TokenCrypto

  private val NonceLength = 12

  def loginCallback(authCode: String, sessionId: UUID): ZIO[Env, Throwable, CharacterAuth] =
    Esi.initialTokenAndUserData(authCode).mapError(_.asThrowable).flatMap { (jwt, tokenMeta) =>
      query.transaction(
        (AuthQueries.getUserCharactersBySessionId(sessionId) <*>
          AuthQueries.getUserByCharacterId(CharacterId(tokenMeta.characterId)))
          .flatMap {
            case (None, None)                     => newUser(jwt, tokenMeta, sessionId)
            case (None, Some((user, char)))       => newSession(jwt, tokenMeta, user, char, sessionId)
            case (Some((session, user, _)), None) => addCharacterToUser(jwt, tokenMeta, user, session)
            case (Some(_), Some((user, char)))    => updateRefreshToken(jwt, tokenMeta, user.id, char)
          }
      )
    }

  def logoutCharacterFromUser(sessionId: UUID, characterId: CharacterId): ZIO[Env, Throwable, Boolean] =
    AuthQueries
      .getUserCharactersBySessionId(sessionId)
      .flatMap:
        case Some((user, _, chars)) if chars.exists(_.id == characterId) =>
          auth.removeCharacterFromUser(user.userId, characterId).map(count => if (count > 0) true else false)
        case _ => ZIO.succeed(false)

  def allCharacters: ZIO[Env, Throwable, Chunk[CharacterId]] =
    AuthQueries.getAllCharacterIds().map(Chunk.fromIterable(_))

  def updateAffiliations(affiliations: List[(CharacterId, CorporationId, Option[AllianceId])]) =
    AuthQueries.updateCharacterAffiliations(affiliations)

  def loadAll: ZIO[Env, Throwable, Chunk[CharacterAuth]] =
    for
      allTokens <- auth.getAllAuthTokens()
      authTokens <- ZIO.foreach(allTokens)((uc, ecat) =>
        decryptAuthToken(ecat).map((t, r) => CharacterAuth(uc.userId, uc.characterId, JwtString(t), r, ecat.expiresAt))
      )
    yield Chunk.fromIterable(authTokens)

  def refreshToken(refreshToken: Base64): ZIO[Env, Throwable, CharacterAuth] =
    Esi
      .refreshTokenAndUserData(refreshToken)
      .mapError(_.asThrowable)
      .flatMap((jwt, tokenMeta) => refreshTokenMinimal(jwt, tokenMeta))

  private def newUser(
      jwt: JwtAuthResponse,
      tokenMeta: EsiTokenMeta,
      sessionId: UUID
  ): ZIO[Env, Throwable, CharacterAuth] =
    for
      now <- ZIO.clockWith(_.instant)
      char <- getUserCharacterFromEsi(tokenMeta.characterId, tokenMeta.characterOwnerHash, jwt.accessToken, now)
        .logError(s"Failed to get character info for ${tokenMeta.characterId}")
        .absorbWith(err => EsiError.ClientError(err).asThrowable)
      userId <- auth.insertUser(displayName = char.name)
      _      <- auth.insertUserCharacter(UserCharacter(userId, char.id))
      _      <- auth.upsertCharacter(char)
      _      <- newUserSession(userId, sessionId).flatMap(auth.insertUserSession)
      token  <- encryptJwtResponse(tokenMeta, jwt)
      _      <- auth.upsertAuthToken(token)
    yield CharacterAuth(userId, tokenMeta.characterId, jwt.accessToken, Base64.raw(jwt.refreshToken), tokenMeta.expiry)

  private def newSession(
      jwt: JwtAuthResponse,
      tokenMeta: EsiTokenMeta,
      user: AuthUser,
      char: AuthCharacter,
      sessionId: UUID
  ): ZIO[Env, Throwable, CharacterAuth] =
    for
      sess  <- newUserSession(user.id, sessionId)
      _     <- auth.insertUserSession(sess)
      _     <- auth.upsertCharacter(char)
      token <- encryptJwtResponse(tokenMeta, jwt)
      _     <- auth.upsertAuthToken(token)
    yield CharacterAuth(user.id, tokenMeta.characterId, jwt.accessToken, Base64.raw(jwt.refreshToken), tokenMeta.expiry)

  private def addCharacterToUser(
      jwt: JwtAuthResponse,
      tokenMeta: EsiTokenMeta,
      user: AuthUser,
      session: UserSession
  ): ZIO[Env, Throwable, CharacterAuth] =
    for
      now <- ZIO.clockWith(_.instant)
      char <- getUserCharacterFromEsi(tokenMeta.characterId, tokenMeta.characterOwnerHash, jwt.accessToken, now)
        .logError(s"Failed to get character info for ${tokenMeta.characterId}")
        .absorbWith(err => EsiError.ClientError(err).asThrowable)
      _     <- auth.insertUserCharacter(UserCharacter(user.id, char.id))
      _     <- auth.upsertCharacter(char)
      token <- encryptJwtResponse(tokenMeta, jwt)
      _     <- auth.upsertAuthToken(token)
    yield CharacterAuth(user.id, tokenMeta.characterId, jwt.accessToken, Base64.raw(jwt.refreshToken), tokenMeta.expiry)

  private def updateRefreshToken(
      jwt: JwtAuthResponse,
      tokenMeta: EsiTokenMeta,
      userId: UserId,
      char: AuthCharacter
  ): ZIO[Env, Throwable, CharacterAuth] =
    for
      now <- ZIO.clockWith(_.instant)
      char <- getUserCharacterFromEsi(tokenMeta.characterId, tokenMeta.characterOwnerHash, jwt.accessToken, now)
        .logError(s"Failed to get character info for ${tokenMeta.characterId}")
        .absorbWith(err => EsiError.ClientError(err).asThrowable)
      _     <- auth.upsertCharacter(char)
      token <- encryptJwtResponse(tokenMeta, jwt)
      _     <- auth.upsertAuthToken(token)
    yield CharacterAuth(userId, tokenMeta.characterId, jwt.accessToken, Base64.raw(jwt.refreshToken), tokenMeta.expiry)

  private def refreshTokenMinimal(jwt: JwtAuthResponse, tokenMeta: EsiTokenMeta) =
    for
      token   <- encryptJwtResponse(tokenMeta, jwt)
      _       <- auth.upsertAuthToken(token)
      userOpt <- auth.getUserForCharacter(token.characterId)
      _       <- ZIO.fail(new RuntimeException("Character has no user")).unless(userOpt.isDefined)
    yield CharacterAuth(
      userOpt.get.userId,
      tokenMeta.characterId,
      jwt.accessToken,
      Base64.raw(jwt.refreshToken),
      tokenMeta.expiry
    )

  private def newUserSession(userId: UserId, sessionId: UUID) =
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

  private def getUserCharacterFromEsi(characterId: CharacterId, ownerHash: String, jwt: JwtString, now: Instant) =
    (EsiClient.withZIO(_.getCharacter(characterId)) <&>
      EsiClient.withZIO(_.getCharacterAffiliations(List(characterId))) <&>
      EsiClient.withZIO(_.getCharacterOnline(jwt)(characterId)))
      .map:
        case (apiChar, apiAffiliations, online) =>
          assert(apiAffiliations.length == 1, "expecting len(affiliations) == 1")
          AuthCharacter(
            ownerHash = ownerHash,
            id = characterId,
            name = apiChar.name,
            corporationId = apiAffiliations.head.corporationId,
            allianceId = apiAffiliations.head.allianceId,
            bornAt = apiChar.birthday,
            addedAt = Some(now),
            updatedAt = Some(now),
            lastOnlineAt = online.lastLogin
          )

  private[auth] def encryptJwtResponse(tokenMeta: EsiTokenMeta, jwt: JwtAuthResponse) =
    for
      nonce      <- secureRandomBytesBase64(NonceLength)
      encAccess  <- ZIO.serviceWith[TokenCrypto](_.encrypt(nonce.toBytes, jwt.accessToken.value.getBytes))
      encRefresh <- ZIO.serviceWith[TokenCrypto](_.encrypt(nonce.toBytes, Base64.raw(jwt.refreshToken).toBytes))
      now        <- ZIO.clockWith(_.instant)
    yield model.CharacterAuthToken(
      characterId = CharacterId(tokenMeta.characterId),
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
