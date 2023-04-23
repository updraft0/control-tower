package com.example.controltower.domain

import java.time.Instant

case class SignatureId(id: String)

/** Type of signature
  */
enum SignatureType:
  case Unknown, Combat, Relic, Data, Gas, Wormhole, Ore, Ghost

sealed trait SignatureDetails

/** Single wormhole signature
  */
case class Signature(id: SignatureId, `type`: SignatureType, note: String, createdAt: Instant, updatedAt: Instant)
