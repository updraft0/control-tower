package org.updraft0.esi.client

import pdi.jwt.JwtClaim
import sttp.tapir.DecodeResult
import zio.json.*
import zio.test.*
import sttp.tapir.json.zio.jsonBody

object JsonEncodingSpec extends ZIOSpecDefault {
  import schema.given
  import jsoncodec.given

  def spec = suite("ESI JSON Decoding")(
    test("for AuthErrorResponse") {
      val s = """{"error":"invalid_grant","error_description":"Authorization code is invalid."}"""
      assertTrue(
        s.fromJson[AuthErrorResponse] == Right(AuthErrorResponse("invalid_grant", "Authorization code is invalid."))
      )
    },
    test("for JwtAuthResponse") {
      // note: not a real token
      val s =
        """{"access_token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c","expires_in":1199,"token_type":"Bearer","refresh_token":"eXRkyJ7SEyNGy9pssAC8bQ=="}"""
      assertTrue(
        s.fromJson[JwtAuthResponse] == Right(
          JwtAuthResponse(
            accessToken = JwtClaim(
              content = """{"name":"John Doe"}""",
              issuer = None,
              subject = Some("1234567890"),
              audience = None,
              expiration = None,
              notBefore = None,
              issuedAt = Some(1516239022),
              jwtId = None
            ),
            expiresIn = 1199,
            tokenType = "Bearer",
            refreshToken = "eXRkyJ7SEyNGy9pssAC8bQ=="
          )
        ),
        // also check codec through tapir
        jsonBody[JwtAuthResponse].codec.decode(s) match
          case DecodeResult.Value(_) => true
          case _                     => false
      )
    }
  )

}
