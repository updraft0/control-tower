package org.updraft0.controltower.db.query

import io.getquill.*
import org.updraft0.controltower.db.model.*
import zio.ZIO

object auth:
  import schema.*
  import ctx.*

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

  /** Each table lives in the `map` schema, but Quill has no config annotation/etc. for that
    */
  object schema:
    inline def character          = quote(querySchema[AuthCharacter]("auth.character"))
    inline def characterAuthToken = quote(querySchema[CharacterAuthToken]("auth.character_auth_token"))
    inline def user               = quote(querySchema[AuthUser]("auth.user"))
    inline def userCharacter      = quote(querySchema[UserCharacter]("auth.user_character"))
    inline def userSession        = quote(querySchema[UserSession]("auth.user_session"))
    inline def mapPolicy          = quote(querySchema[MapPolicy]("auth.map_policy"))
    inline def mapPolicyMember    = quote(querySchema[MapPolicyMember]("auth.map_policy_member"))

  private inline def insert[T](inline entity: Quoted[EntityQuery[T]], inline value: T): Insert[T] = quote {
    entity.insertValue(value)
  }

  def insertCharacter(char: AuthCharacter): DbOperation[Long] =
    ctx.run(insert(schema.character, lift(char)))

  def insertUserCharacter(userChar: UserCharacter): DbOperation[Long] =
    ctx.run(insert(schema.userCharacter, lift(userChar)))

  def insertUserSession(sess: UserSession): DbOperation[Long] =
    ctx.run(insert(schema.userSession, lift(sess)))

  def insertAuthToken(tok: CharacterAuthToken): DbOperation[Long] =
    ctx.run(insert(schema.characterAuthToken, lift(tok)))

  def insertUser(displayName: String): DbOperation[Long] =
    ctx.run(quote(schema.user.insertValue(lift(AuthUser(0L, displayName, None))).returningGenerated(_.id)))
