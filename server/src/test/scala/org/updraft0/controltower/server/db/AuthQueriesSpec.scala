package org.updraft0.controltower.server.db

import org.updraft0.controltower.constant.CharacterId
import org.updraft0.controltower.db.query
import zio.test.*

object AuthQueriesSpec extends ZIOSpecDefault:
  override def spec = suite("AuthQueries::"):
    test("getMapPoliciesForCharacter (is empty)"):
      for res <- AuthQueries.getMapPoliciesForCharacter(List(CharacterId.System))
      yield assertTrue(res.isEmpty)
  .provideLayer(TempDb.empty)

// getMapPoliciesForCharacter
