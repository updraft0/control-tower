package org.updraft0.controltower.server.auth

import org.updraft0.controltower.constant.CharacterId
import org.updraft0.esi.client.{JwtAuthResponse, JwtString}
import zio.*
import zio.test.*

import java.security.SecureRandom
import java.time.Instant
import java.util as ju
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

object UsersSpec extends ZIOSpecDefault:

  val SampleTokenValue =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
  val SampleRefresh = ju.Base64.getEncoder.encodeToString("refresh1234".getBytes)

  val SampleKey = "encryptionkey123encryptionkey123".getBytes

  def spec = suite("JWT auth token")(
    test("can be encrypted and decrypted"):
        val meta = EsiTokenMeta(CharacterId(1234), "Name1", "abcdef", Instant.EPOCH)
        val jwt  = JwtAuthResponse(JwtString(SampleTokenValue), 1L, "?", SampleRefresh)

        for
          enc <- Users.encryptJwtResponse(meta, jwt)
          dec <- Users.decryptAuthToken(enc)
        yield assertTrue(
          enc.characterId == CharacterId(1234L),
          enc.expiresAt == meta.expiry,
          enc.refreshToken == encrypt(
            Base64.raw(enc.nonce),
            SampleKey,
            Base64.raw(SampleRefresh).toBytes
          ).stringValue,
          enc.token == encrypt(Base64.raw(enc.nonce), SampleKey, SampleTokenValue.getBytes).stringValue,
          dec._1 == SampleTokenValue,
          dec._2.stringValue == SampleRefresh
        )
  ).provide(
    ZLayer(ZIO.attempt(new SecureRandom())),
    ZLayer(ZIO.attempt(TokenCrypto(new SecretKeySpec(SampleKey, "AES"))))
  )

private def encrypt(nonce: Base64, key: Array[Byte], value: Array[Byte]): Base64 =
  val c = Cipher.getInstance("AES/GCM/NoPadding")
  val p = new GCMParameterSpec(128, nonce.toBytes)
  c.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"), p)
  Base64(c.doFinal(value))
