package org.updraft0.controltower.server.auth

import java.util as ju

opaque type Base64 = String

object Base64:
  def raw(value: String): Base64 = value

  def apply(bytes: Array[Byte]): Base64 =
    ju.Base64.getEncoder.encodeToString(bytes)

extension (b: Base64)
  def stringValue: String  = b
  def toBytes: Array[Byte] = ju.Base64.getDecoder.decode(b)
