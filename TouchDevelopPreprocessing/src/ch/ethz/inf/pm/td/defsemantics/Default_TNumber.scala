
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Number
 *
 * A number (possibly negative and/or fractional)
 *
 * @author Lucas Brutschy
 */

trait Default_TNumber extends AAny {

  lazy val typeName = TypeName("Number")
          
  /** Very frequently used: Multiplies numbers */
  def member_multiply = ApiMember(
    name = "*",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Adds numbers */
  def member_add = ApiMember(
    name = "+",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Subtracts numbers */
  def member_subtract = ApiMember(
    name = "-",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Divides numbers */
  def member_divide = ApiMember(
    name = "/",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Compares numbers for less */
  def member_lt = ApiMember(
    name = "<",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Compares numbers for equality */
  def member_eq = ApiMember(
    name = "=",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Compares numbers for more */
  def member_gt = ApiMember(
    name = ">",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Interprets a number as a unicode value and converts it to the single character string */
  def member_to_character = ApiMember(
    name = "to character",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Interprets the number as a ARGB (alpha, red, green, blue) color */
  def member_to_color = ApiMember(
    name = "to color",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Never used: Converts the value into a json data structure. */
  def member_to_json = ApiMember(
    name = "to json",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Converts a number to a string */
  def member_to_string = ApiMember(
    name = "to string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Frequently used: Compares numbers for disequality */
  def member_neq = ApiMember(
    name = "≠",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Frequently used: Compares numbers for less or equal */
  def member_le = ApiMember(
    name = "≤",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Frequently used: Compares numbers for more or equal */
  def member_ge = ApiMember(
    name = "≥",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "*" -> member_multiply,
    "+" -> member_add,
    "-" -> member_subtract,
    "/" -> member_divide,
    "<" -> member_lt,
    "=" -> member_eq,
    ">" -> member_gt,
    "to character" -> member_to_character,
    "to color" -> member_to_color,
    "to json" -> member_to_json,
    "to string" -> member_to_string,
    "≠" -> member_neq,
    "≤" -> member_le,
    "≥" -> member_ge
  )
            

}
          
