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
import java.sql.SQLException
import java.time.Instant
import java.util.UUID

/** In-memory representation of auth for a character
  */
case class CharacterAuth(
    userId: UserId,
    characterId: CharacterId,
    characterName: String,
    token: JwtString,
    refreshToken: Base64,
    expiry: Instant
)

object Users:
  type Env   = Config & javax.sql.DataSource & EsiClient & SecureRandom & TokenCrypto
  type Error = EsiError | SQLException
  type Op[T] = ZIO[Env, Error, T]

  private enum LoginState derives CanEqual:
    /** Brand-new user (+ first character)
      */
    case NewUser

    /** Existing user + character set, new browser session
      */
    case NewSession(user: AuthUser, char: AuthCharacter)

    /** Add a new character to existing user + character set
      */
    case UserAddCharacter(session: UserSession, user: AuthUser)

    /** Existing user + character - update the token stored
      */
    case RefreshToken(session: UserSession, user: AuthUser)

  def loginCallback(authCode: String, sessionId: UUID): ZIO[Env, Error, CharacterAuth] =
    for
      jwtMeta <- Esi.initialTokenAndUserData(authCode)
      state   <- dbTransaction(loginCheckState(sessionId, jwtMeta._2.characterId))
      res     <- loginComplete(jwtMeta._1, jwtMeta._2, sessionId, state)
    yield res

  private def loginCheckState(sessionId: UUID, characterId: CharacterId) =
    (AuthQueries.getUserCharactersBySessionId(sessionId) <*>
      AuthQueries.getUserByCharacterId(CharacterId(characterId)))
      .map {
        case (None, None)                                   => LoginState.NewUser
        case (None, Some((user, _, char)))                  => LoginState.NewSession(user, char)
        case (Some((session, user, _, _)), None)            => LoginState.UserAddCharacter(session, user)
        case (Some((session, _, _, _)), Some((user, _, _))) => LoginState.RefreshToken(session, user)
      }

  private def loginComplete(jwt: JwtAuthResponse, tokenMeta: EsiTokenMeta, sessionId: UUID, state: LoginState) =
    ZIO
      .clockWith(_.instant)
      .flatMap: now =>
        state match
          case LoginState.NewUser =>
            getUserCharacterFromEsi(jwt, tokenMeta, now)
              .flatMap(authChar => dbTransaction(newUser(authChar, jwt, tokenMeta, sessionId, now)))
          case ns: LoginState.NewSession =>
            dbTransaction(newSession(jwt, tokenMeta, ns.user, ns.char, sessionId, now))
          case uac: LoginState.UserAddCharacter =>
            getUserCharacterFromEsi(jwt, tokenMeta, now).flatMap(authChar =>
              dbTransaction(addCharacterToUser(authChar, jwt, tokenMeta, uac.user, uac.session, now))
            )
          case rt: LoginState.RefreshToken =>
            getUserCharacterFromEsi(jwt, tokenMeta, now).flatMap(authChar =>
              dbTransaction(updateRefreshToken(authChar, jwt, tokenMeta, rt.user.id, rt.session, now))
            )

  private def dbTransaction[R, T](
      op: ZIO[R & javax.sql.DataSource, Throwable, T]
  ): ZIO[R & javax.sql.DataSource, Error, T] =
    query
      .transactionLoose(op)
      .foldZIO(
        {
          case sqle: SQLException => ZIO.fail(sqle)
          case ex                 => ZIO.die(ex)
        },
        ZIO.succeed
      )

  def logoutCharacterFromUser(sessionId: UUID, characterId: CharacterId): Op[Boolean] =
    dbTransaction(
      AuthQueries
        .getUserCharactersBySessionId(sessionId)
        .flatMap:
          case Some((user, _, _, chars)) if chars.exists(_.id == characterId) =>
            auth
              .removeCharacterFromUser(user.userId, characterId)
              .map(count => if (count > 0) true else false)
              .zipLeft(auth.deleteCharacterAuthToken(characterId))
          case _ => ZIO.succeed(false)
    )

  def allCharacters: Op[Chunk[CharacterId]] =
    AuthQueries.getAllCharacterIds().map(Chunk.fromIterable(_))

  def updateAffiliations(affiliations: List[(CharacterId, CorporationId, Option[AllianceId])]): Op[Long] =
    dbTransaction(AuthQueries.updateCharacterAffiliations(affiliations))

  def loadAll: Op[Chunk[CharacterAuth]] =
    for
      allTokens  <- auth.getAllAuthTokens()
      authTokens <- ZIO.foreach(allTokens)((c, uc, ecat) => decryptAuthTokenTo(uc.userId, c.name, ecat))
    yield Chunk.fromIterable(authTokens)

  def refreshToken(refreshToken: Base64): Op[CharacterAuth] =
    Esi
      .refreshTokenAndUserData(refreshToken)
      .flatMap((jwt, tokenMeta) => dbTransaction(refreshTokenMinimal(jwt, tokenMeta)))

  def removeExpiredTokens(characterIds: Chunk[CharacterId]): RIO[javax.sql.DataSource, Long] =
    query.transaction(auth.deleteCharacterAuthTokens(characterIds))

  def loadAnyAuthTokenForUser(userId: UserId): RIO[javax.sql.DataSource & TokenCrypto, Option[CharacterAuth]] =
    AuthQueries
      .getSomeCharacterAuthToken(userId)
      .flatMap:
        case None                    => ZIO.none
        case Some((char, authToken)) => decryptAuthTokenTo(userId, char.name, authToken).asSome

  private def newUser(
      char: AuthCharacter,
      jwt: JwtAuthResponse,
      tokenMeta: EsiTokenMeta,
      sessionId: UUID,
      now: Instant
  ) =
    for
      userId <- auth.insertUser(displayName = char.name)
      _      <- auth.insertUserCharacter(UserCharacter(userId, char.id))
      _      <- auth.upsertCharacter(char)
      _      <- newUserSession(userId, sessionId, now).flatMap(auth.insertUserSession)
      token  <- encryptJwtResponse(tokenMeta, jwt)
      _      <- auth.upsertAuthToken(token)
    yield CharacterAuth(
      userId,
      tokenMeta.characterId,
      char.name,
      jwt.accessToken,
      Base64.raw(jwt.refreshToken),
      tokenMeta.expiry
    )

  private def newSession(
      jwt: JwtAuthResponse,
      tokenMeta: EsiTokenMeta,
      user: AuthUser,
      char: AuthCharacter,
      sessionId: UUID,
      now: Instant
  ) =
    for
      sess  <- newUserSession(user.id, sessionId, now)
      _     <- auth.insertUserSession(sess)
      _     <- auth.upsertCharacter(char)
      token <- encryptJwtResponse(tokenMeta, jwt)
      _     <- auth.upsertAuthToken(token)
    yield CharacterAuth(
      user.id,
      tokenMeta.characterId,
      char.name,
      jwt.accessToken,
      Base64.raw(jwt.refreshToken),
      tokenMeta.expiry
    )

  private def addCharacterToUser(
      char: AuthCharacter,
      jwt: JwtAuthResponse,
      tokenMeta: EsiTokenMeta,
      user: AuthUser,
      session: UserSession,
      now: Instant
  ) =
    for
      conf  <- ZIO.service[Config]
      _     <- auth.refreshSessionExpiry(session.copy(expiresAt = now.plus(conf.auth.sessionExpiry)))
      _     <- auth.insertUserCharacter(UserCharacter(user.id, char.id))
      _     <- auth.upsertCharacter(char)
      token <- encryptJwtResponse(tokenMeta, jwt)
      _     <- auth.upsertAuthToken(token)
    yield CharacterAuth(
      user.id,
      tokenMeta.characterId,
      char.name,
      jwt.accessToken,
      Base64.raw(jwt.refreshToken),
      tokenMeta.expiry
    )

  private def updateRefreshToken(
      char: AuthCharacter,
      jwt: JwtAuthResponse,
      tokenMeta: EsiTokenMeta,
      userId: UserId,
      session: UserSession,
      now: Instant
  ) =
    for
      conf  <- ZIO.service[Config]
      _     <- auth.refreshSessionExpiry(session.copy(expiresAt = now.plus(conf.auth.sessionExpiry)))
      _     <- auth.upsertCharacter(char)
      token <- encryptJwtResponse(tokenMeta, jwt)
      _     <- auth.upsertAuthToken(token)
    yield CharacterAuth(
      userId,
      tokenMeta.characterId,
      char.name,
      jwt.accessToken,
      Base64.raw(jwt.refreshToken),
      tokenMeta.expiry
    )

  private def refreshTokenMinimal(jwt: JwtAuthResponse, tokenMeta: EsiTokenMeta) =
    for
      token   <- encryptJwtResponse(tokenMeta, jwt)
      _       <- auth.upsertAuthToken(token)
      userOpt <- auth.getUserForCharacter(token.characterId)
      _       <- ZIO.fail(new RuntimeException("Character has no user")).unless(userOpt.isDefined)
      (c, user) = userOpt.get
    yield CharacterAuth(
      user.userId,
      tokenMeta.characterId,
      c.name,
      jwt.accessToken,
      Base64.raw(jwt.refreshToken),
      tokenMeta.expiry
    )

  private def newUserSession(userId: UserId, sessionId: UUID, now: Instant) =
    for conf <- ZIO.service[Config]
    yield UserSession(
      sessionId = sessionId,
      userId = userId,
      createdAt = now,
      expiresAt = now.plus(conf.auth.sessionExpiry),
      lastSeenAt = Some(now),
      ipAddress = None, // FIXME implement this
      userAgent = None  // FIXME implement this
    )

  private def getUserCharacterFromEsi(jwt: JwtAuthResponse, tokenMeta: EsiTokenMeta, now: Instant) =
    getUserCharacterFromEsiRaw(tokenMeta.characterId, tokenMeta.characterOwnerHash, jwt.accessToken, now)
      .logError(s"Failed to get character info for ${tokenMeta.characterId}")
      .mapError(EsiError.ClientError(_))

  private def getUserCharacterFromEsiRaw(characterId: CharacterId, ownerHash: String, jwt: JwtString, now: Instant) =
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
      nonce      <- secureRandomBytesBase64(MagicConstant.NonceLength)
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

  private[auth] def decryptAuthTokenTo(userId: UserId, name: String, token: model.CharacterAuthToken) =
    decryptAuthToken(token).map: (access, refresh) =>
      CharacterAuth(userId, token.characterId, name, JwtString(access), refresh, token.expiresAt)

  private[auth] def decryptAuthToken(token: model.CharacterAuthToken) =
    val nonce = Base64.raw(token.nonce).toBytes
    for
      decAccess  <- ZIO.serviceWith[TokenCrypto](_.decrypt(nonce, Base64.raw(token.token).toBytes))
      decRefresh <- ZIO.serviceWith[TokenCrypto](_.decrypt(nonce, Base64.raw(token.refreshToken).toBytes))
    yield new String(decAccess) -> Base64(decRefresh)
