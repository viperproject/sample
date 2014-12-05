
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Place Collection
 *
 * A collection of places
 *
 * @author Lucas Brutschy
 */

trait Default_TPlace_Collection extends AMutable_Collection {

  lazy val typeName = TypeName("Place Collection")
          
  def keyType = TNumber

  def valueType = TPlace

  /** Never used: Gets the identifier of the next set of messages */
  def member_continuation = ApiMember(
    name = "continuation",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the identifier of the next set of messages */
  def member_set_continuation = ApiMember(
    name = "set continuation",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sorts the places by distance to the location */
  def member_sort_by_distance = ApiMember(
    name = "sort by distance",
    paramTypes = List(ApiParam(TLocation)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "continuation" -> member_continuation,
    "set continuation" -> member_set_continuation,
    "sort by distance" -> member_sort_by_distance
  )
            

}
          
