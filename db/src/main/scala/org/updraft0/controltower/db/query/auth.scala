package org.updraft0.controltower.db.query

import io.getquill.*
import org.updraft0.controltower.constant.*
import org.updraft0.controltower.db.model.*
import zio.Chunk

import java.util.UUID

object auth:
  import schema.*
  import ctx.*

  given CanEqual[UUID, UUID] = CanEqual.derived

  // TODO: move these opaque type mappings somewhere else?
  given MappedEncoding[Long, CharacterId] = MappedEncoding(CharacterId.apply)
  given MappedEncoding[CharacterId, Long] = MappedEncoding(identity)

  given MappedEncoding[Long, CorporationId] = MappedEncoding(CorporationId.apply)
  given MappedEncoding[CorporationId, Long] = MappedEncoding(identity)

  given MappedEncoding[Long, AllianceId] = MappedEncoding(AllianceId.apply)
  given MappedEncoding[AllianceId, Long] = MappedEncoding(identity)

  given MappedEncoding[Long, UserId] = MappedEncoding(UserId.apply)
  given MappedEncoding[UserId, Long] = MappedEncoding(identity)

  given MappedEncoding[Long, MapId] = MappedEncoding(MapId.apply)
  given MappedEncoding[MapId, Long] = MappedEncoding(identity)

  given MappedEncoding[Long, ConnectionId] = MappedEncoding(ConnectionId.apply)
  given MappedEncoding[ConnectionId, Long] = MappedEncoding(identity)

  given MappedEncoding[Long, SystemId] = MappedEncoding(SystemId.apply)
  given MappedEncoding[SystemId, Long] = MappedEncoding(identity)

  given MappedEncoding[String, SigId] = MappedEncoding(SigId.apply)
  given MappedEncoding[SigId, String] = MappedEncoding(identity)

  // TODO: not sure about location of these
  given MappedEncoding[String, MapRole] = MappedEncoding {
    case "viewer" => MapRole.Viewer
    case "editor" => MapRole.Editor
    case "admin"  => MapRole.Admin
  }
  given MappedEncoding[MapRole, String] = MappedEncoding(_.toString.toLowerCase)

  given MappedEncoding[String, PolicyMemberType] = MappedEncoding {
    case "character"   => PolicyMemberType.Character
    case "corporation" => PolicyMemberType.Corporation
    case "alliance"    => PolicyMemberType.Alliance
  }
  given MappedEncoding[PolicyMemberType, String] = MappedEncoding(_.toString.toLowerCase)

  /** Each table lives in the `auth` schema, but Quill has no config annotation/etc. for that
    */
  object schema:
    inline def character          = quote(querySchema[AuthCharacter]("auth.character"))
    inline def characterAuthToken = quote(querySchema[CharacterAuthToken]("auth.character_auth_token"))
    inline def user               = quote(querySchema[AuthUser]("auth.user"))
    inline def userCharacter      = quote(querySchema[UserCharacter]("auth.user_character"))
    inline def userPreference     = quote(querySchema[UserPreference]("auth.user_preference"))
    inline def userSession        = quote(querySchema[UserSession]("auth.user_session"))
    inline def mapPolicy          = quote(querySchema[MapPolicy]("auth.map_policy"))
    inline def mapPolicyMember    = quote(querySchema[MapPolicyMember]("auth.map_policy_member"))

  private inline def insert[T](inline entity: Quoted[EntityQuery[T]], inline value: T): Insert[T] =
    quote(entity.insertValue(value))

  def getAllAuthTokens(): DbOperation[List[(AuthCharacter, UserCharacter, CharacterAuthToken)]] =
    ctx.run(quote {
      for
        c  <- character
        uc <- userCharacter.filter(_.characterId == c.id)
        ac <- characterAuthToken.filter(_.characterId == uc.characterId)
      yield (c, uc, ac)
    })

  def getUserForCharacter(characterId: CharacterId): DbOperation[Option[(AuthCharacter, UserCharacter)]] =
    ctx
      .run(
        quote(character.filter(_.id == lift(characterId)).join(userCharacter).on((ac, uc) => ac.id == uc.characterId))
      )
      .map(_.headOption)

  def upsertCharacter(char: AuthCharacter): DbOperation[Long] =
    ctx.run(
      schema.character
        .insertValue(lift(char))
        .onConflictUpdate(_.id)(
          (t, e) => t.ownerHash     -> e.ownerHash,
          (t, e) => t.corporationId -> e.corporationId,
          (t, e) => t.allianceId    -> e.allianceId,
          (t, e) => t.lastOnlineAt  -> e.lastOnlineAt,
          (t, _) => t.updatedAt     -> Some(unixepoch)
        )
    )

  def insertUserCharacter(userChar: UserCharacter): DbOperation[Long] =
    ctx.run(insert(schema.userCharacter, lift(userChar)))

  def insertUserSession(sess: UserSession): DbOperation[Long] =
    ctx.run(insert(schema.userSession, lift(sess)))

  def refreshSessionExpiry(sess: UserSession): DbOperation[Long] =
    ctx.run(
      schema.userSession
        .filter(us => us.sessionId == lift(sess.sessionId) && us.userId == lift(sess.userId))
        .update(_.expiresAt -> lift(sess.expiresAt), _.lastSeenAt -> Some(unixepoch))
    )

  def upsertAuthToken(tok: CharacterAuthToken): DbOperation[Long] =
    ctx.run(
      schema.characterAuthToken
        .insertValue(lift(tok))
        .onConflictUpdate(_.characterId)(
          (t, e) => t.nonce        -> e.nonce,
          (t, e) => t.refreshToken -> e.refreshToken,
          (t, e) => t.token        -> e.token,
          (t, e) => t.expiresAt    -> e.expiresAt,
          (t, _) => t.updatedAt    -> Some(unixepoch)
        )
    )

  def insertUser(displayName: String): DbOperation[UserId] =
    ctx.run(
      quote(schema.user.insertValue(AuthUser(lift(UserId.Invalid), lift(displayName), None)).returningGenerated(_.id))
    )

  def removeCharacterFromUser(userId: UserId, characterId: CharacterId): DbOperation[Long] =
    ctx.run(
      quote(schema.userCharacter.filter(uc => uc.userId == lift(userId) && uc.characterId == lift(characterId)).delete)
    )

  def deleteCharacterAuthToken(characterId: CharacterId): DbOperation[Long] =
    ctx.run(
      quote(schema.characterAuthToken.filter(_.characterId == lift(characterId)).delete)
    )

  def deleteCharacterAuthTokens(ids: Chunk[CharacterId]): DbOperation[Long] =
    ctx.run(quote(schema.characterAuthToken.filter(cat => liftQuery(ids).contains(cat.characterId)).delete))

  def upsertUserPreference(pref: UserPreference): DbOperation[Long] =
    ctx.run(
      schema.userPreference
        .insertValue(lift(pref))
        .onConflictUpdate(_.userId)((t, e) => t.preferenceJson -> e.preferenceJson)
    )
