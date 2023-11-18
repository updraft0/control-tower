package org.updraft0.esi.client

import com.github.plokhotnyuk.jsoniter_scala.core.*
import sttp.tapir.DecodeResult
import sttp.tapir.json.jsoniter.jsonBody
import zio.test.*

object JsonEncodingSpec extends ZIOSpecDefault {
  import jsoncodec.given
  import schema.given

  def spec = suite("ESI JSON Decoding")(
    test("for AuthErrorResponse") {
      val s = """{"error":"invalid_grant","error_description":"Authorization code is invalid."}"""
      assertTrue(
        readFromString[AuthErrorResponse](s) == AuthErrorResponse("invalid_grant", "Authorization code is invalid.")
      )
    },
    test("for JwtAuthResponse") {
      // note: not a real token
      val s =
        """{"access_token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c","expires_in":1199,"token_type":"Bearer","refresh_token":"eXRkyJ7SEyNGy9pssAC8bQ=="}"""
      assertTrue(
        readFromString[JwtAuthResponse](s) ==
          JwtAuthResponse(
            accessToken = JwtString(
              "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
            ),
            expiresIn = 1199,
            tokenType = "Bearer",
            refreshToken = "eXRkyJ7SEyNGy9pssAC8bQ=="
          ),
        // also check codec through tapir
        jsonBody[JwtAuthResponse].codec.decode(s) match
          case DecodeResult.Value(_) => true
          case _                     => false
      )
    }
  )

}
