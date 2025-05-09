package org.updraft0.controltower.server.auth

import org.updraft0.controltower.server.Config
import zio.{Config as _, *}

import java.security.SecureRandom
import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

def secureRandom: ZLayer[Any, Throwable, SecureRandom] = ZLayer(ZIO.attempt(new SecureRandom()))
def secureRandomBytes(length: Int): ZIO[SecureRandom, Nothing, Array[Byte]] =
  ZIO.serviceWith[SecureRandom]: sr =>
    val bytes = new Array[Byte](length)
    sr.nextBytes(bytes)
    bytes
def secureRandomBytesBase64(length: Int): ZIO[SecureRandom, Nothing, Base64] =
  secureRandomBytes(length).map(Base64.apply)

/** Low-level interface to encrypt/decrypt access tokens
  */
trait TokenCrypto:
  def encrypt(nonce: Array[Byte], value: Array[Byte]): Array[Byte]
  def decrypt(nonce: Array[Byte], encrypted: Array[Byte]): Array[Byte]

object TokenCrypto:

  private val Aes             = "AES"
  private val GcmLen          = 128
  private val AesGcmNoPadding = "AES/GCM/NoPadding"

  private[auth] def apply(key: SecretKeySpec): TokenCrypto = new TokenCrypto:
    override def encrypt(nonce: Array[Byte], value: Array[Byte]): Array[Byte] =
      val c     = Cipher.getInstance(AesGcmNoPadding)
      val param = new GCMParameterSpec(GcmLen, nonce)
      c.init(Cipher.ENCRYPT_MODE, key, param)
      c.doFinal(value)

    override def decrypt(nonce: Array[Byte], encrypted: Array[Byte]): Array[Byte] =
      val c     = Cipher.getInstance(AesGcmNoPadding)
      val param = new GCMParameterSpec(GcmLen, nonce)
      c.init(Cipher.DECRYPT_MODE, key, param)
      c.doFinal(encrypted)

  def layer: ZLayer[Config, Throwable, TokenCrypto] = ZLayer.fromZIO(
    for
      conf <- ZIO.service[Config]
      keyBytes = Base64.raw(conf.auth.secret.value.value.asString).toBytes
      key      = new SecretKeySpec(keyBytes, Aes)
    yield apply(key)
  )
