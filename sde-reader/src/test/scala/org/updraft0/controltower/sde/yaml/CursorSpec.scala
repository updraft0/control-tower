package org.updraft0.controltower.sde.yaml

import org.snakeyaml.engine.v2.api.{Load}
import zio.*
import zio.test.*

object CursorSpec extends ZIOSpecDefault:
  def spec =
    suite("YAML Cursor")(
      test("works for fields at the top-level") {
        val yaml =
          """
            |hello: world
            |""".stripMargin

        assertCursor(yaml, _.downField("hello").as[String], "world")
      },
      test("works for nested fields") {
        val yaml =
          """
            |obj:
            |  name: galaxy
            |  props:
            |    x: 1
            |    y: 2
            |    z: 3
            |""".stripMargin

        assertCursor(yaml, _.downField("obj").downField("props").downField("y").as[Int], 2)
      },
      test("works for arrays of primitives") {
        val yaml =
          """
            |coords:
            | - 1.2
            | - 2.3
            | - 3.4
            |names:
            | - a
            | - b
            |""".stripMargin

        assertCursor(yaml, _.downField("coords").as[Vector[Double]], Vector(1.2, 2.3, 3.4)) &&
        assertCursor(yaml, _.downField("names").as[Vector[String]], Vector("a", "b"))
      },
      test("works for optional fields") {
        val yaml = """
          |present: yes
          |inner:
          |  present: yes
          |""".stripMargin

        assertCursor(yaml, _.downField("absent").as[Option[Boolean]], None) &&
        assertCursor(yaml, _.downField("inner").downField("absent").as[Option[Boolean]], Option.empty[Boolean]) &&
        assertCursor(yaml, _.downField("absent").downField("present").as[Option[Int]], Option.empty[Int])
      },
      test("works for optional objects") {
        val yaml = """
            |present:
            |  one: 1
            |  two: 2
            |""".stripMargin

        def getOneTwo(c: Cursor[String]): YamlValue[(Int, Int)] =
          for
            one <- c.downField("one").as[Int]
            two <- c.downField("two").as[Int]
          yield one -> two

        assertCursor[String, Option[(Int, Int)]](yaml, _.downField("present").mapOptional(getOneTwo), Some(1 -> 2)) &&
        assertCursor[String, Option[(Int, Int)]](yaml, _.downField("absent").mapOptional(getOneTwo), None)
      },
      test("works for mapping arrays") {
        val yaml = """
            |planets:
            | -
            |   name: One
            |   coords:
            |     - 1
            |     - 2
            |     - 3
            | -
            |   name: Two
            |   coords:
            |     - 3
            |     - 4
            |     - 5
            |""".stripMargin

        val getCoords = (c: Cursor[String]) => c.downField("coords").as[Vector[Int]]
        assertCursor(yaml, _.downField("planets").mapArray(getCoords), Vector(Vector(1, 2, 3), Vector(3, 4, 5)))
      },
      test("works for mapping keyvalues with int keys in a map") {
        val yaml = """
           |planets:
           |  1:
           |    coords:
           |      - 1
           |      - 2
           |      - 3
           |  2:
           |    coords:
           |      - 4
           |      - 5
           |      - 6
           |""".stripMargin

        val getCoords = (_: Int, c: Cursor[String]) => c.downField("coords").as[Vector[Int]]
        assertCursor(yaml, _.downField("planets").mapObject(getCoords), Map(1 -> Vector(1, 2, 3), 2 -> Vector(4, 5, 6)))
      }
    ).provideSomeShared(YAML.layer)

  private def assertCursor[K <: KeyType, A](y: String, op: Cursor[K] => YamlValue[A], expected: A) =
    YAML.cursor(y).flatMap(op).flatMap(got => assertTrue(got == expected))
