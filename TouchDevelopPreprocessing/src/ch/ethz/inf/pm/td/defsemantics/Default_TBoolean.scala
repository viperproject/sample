
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Boolean
 *
 * true or false
 *
 * @author Lucas Brutschy
 */

trait Default_TBoolean extends AAny {

  lazy val typeName = TypeName("Boolean")
          
  /** Very frequently used: Builds conjunction */
  def member_and = ApiMember(
    name = "and",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Negates the boolean expression */
  def member_not = ApiMember(
    name = "not",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Builds disjunction */
  def member_or = ApiMember(
    name = "or",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TBoolean,
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

  /** Sometimes used: Converts true to 1 and false to 0 */
  def member_to_number = ApiMember(
    name = "to number",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Converts a boolean to a string */
  def member_to_string = ApiMember(
    name = "to string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "and" -> member_and,
    "not" -> member_not,
    "or" -> member_or,
    "to json" -> member_to_json,
    "to number" -> member_to_number,
    "to string" -> member_to_string
  )
            

}
          
