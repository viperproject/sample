
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Number Collection
 *
 * A collection of numbers
 *
 * @author Lucas Brutschy
 */

trait Default_TNumber_Collection extends AMutable_Collection {

  lazy val typeName = TypeName("Number Collection")
          
  def keyType = TNumber

  def valueType = TNumber

  /** Never used: Computes the average of the values */
  def member_avg = ApiMember(
    name = "avg",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Computes the maximum of the values */
  def member_max = ApiMember(
    name = "max",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Computes the minimum of the values */
  def member_min = ApiMember(
    name = "min",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Computes the sum of the values */
  def member_sum = ApiMember(
    name = "sum",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "avg" -> member_avg,
    "max" -> member_max,
    "min" -> member_min,
    "sum" -> member_sum
  )
            

}
          
