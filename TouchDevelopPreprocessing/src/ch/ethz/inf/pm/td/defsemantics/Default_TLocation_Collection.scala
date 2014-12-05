
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Location Collection
 *
 * A list of locations
 *
 * @author Lucas Brutschy
 */

trait Default_TLocation_Collection extends AMutable_Collection {

  lazy val typeName = TypeName("Location Collection")
          
  def keyType = TNumber

  def valueType = TLocation

  /** Never used: [**not implemented**] Sorts by distance to the location */
  def member_sort_by_distance = ApiMember(
    name = "sort by distance",
    paramTypes = List(ApiParam(TLocation)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "sort by distance" -> member_sort_by_distance
  )
            

}
          
