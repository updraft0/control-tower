package controltower.page

import zio.test.*

object PageSerializationTest extends ZIOSpecDefault:

  override def spec =
    suite("page serialization")(
      test("for all static pages"):
        assertTrue(
          controltower.Page.Landing.toJson == """{"@type":"Landing"}""",
          controltower.Page.MapEditor.toJson == """{"@type":"MapEditor"}""",
          controltower.Page.ControlsDemo.toJson == """{"@type":"ControlsDemo"}"""
        )
    )
