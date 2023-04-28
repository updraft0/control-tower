package org.snakeyaml.engine.v2.resolver

import org.snakeyaml.engine.v2.api.ConstructNode
import org.snakeyaml.engine.v2.constructor.ConstructYamlNull
import org.snakeyaml.engine.v2.constructor.json.{ConstructYamlJsonBool, ConstructYamlJsonFloat, ConstructYamlJsonInt}
import org.snakeyaml.engine.v2.nodes.Tag
import org.snakeyaml.engine.v2.schema.Schema

import java.util as ju

/** @note
  *   The SDE export contains strings in the form of 9E-1... which the default YAML resolver parses as a... float
  */
class ModifiedFloatResolver extends BaseScalarResolver:
  private lazy val Bool  = "^(?:true|false)$".r
  private lazy val Float = "^(-?(0|[1-9][0-9]*)([.][0-9]*)?)(e[-+]?[0-9]+)?$".r // <-- change here to disallow 'E'
  private lazy val Int   = "^-?(0|[1-9][0-9]*)$".r
  private lazy val Null  = "^null$".r

  override def addImplicitResolvers(): Unit =
    addImplicitResolver(Tag.NULL, BaseScalarResolver.EMPTY, null)
    addImplicitResolver(Tag.BOOL, Bool.pattern, "tf")
    addImplicitResolver(Tag.INT, Int.pattern, "-0123456789")
    addImplicitResolver(Tag.FLOAT, Float.pattern, "-0123456789.")
    addImplicitResolver(Tag.NULL, Null.pattern, "n\u0000")
    addImplicitResolver(Tag.ENV_TAG, BaseScalarResolver.ENV_FORMAT, "$")

/** Modifies the float schema to remove the uppercase `E` from being parsed as the SDE export does not use this notation
  * and it prevents correct parsing of strings that are matched by the float regex otherwise.
  */
object ModifiedFloatSchema extends Schema:
  private val Constructors = {
    val m = Map(
      Tag.NULL  -> new ConstructYamlNull(),
      Tag.BOOL  -> new ConstructYamlJsonBool(),
      Tag.INT   -> new ConstructYamlJsonInt(),
      Tag.FLOAT -> new ConstructYamlJsonFloat()
    )
    val jm = new ju.HashMap[Tag, ConstructNode]()
    m.foreach((k, v) => jm.put(k, v))
    jm
  }
  private lazy val Resolver = new ModifiedFloatResolver()

  override def getScalarResolver: ScalarResolver                    = Resolver
  override def getSchemaTagConstructors: ju.Map[Tag, ConstructNode] = Constructors
