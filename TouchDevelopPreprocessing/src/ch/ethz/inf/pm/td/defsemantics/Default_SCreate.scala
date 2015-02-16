
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Create
 *
 * Create collections of items.
 *
 * @author Lucas Brutschy
 */

trait Default_SCreate extends ASingleton {

  lazy val typeName = TypeName("Create")
          
  /** Never used: Creates an empty collection of arbitrary type */
  def member_Collection_of = ApiMember(
    name = "Collection of",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TUnfinished_Type,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "Collection of" -> member_Collection_of
  )
            

}
          
