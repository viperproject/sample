
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Spring
 *
 * A spring between two sprites.
 *
 * @author Lucas Brutschy
 */

trait Default_TSpring extends AAny {

  lazy val typeName = TypeName("Spring")
          
  /** Never used: Deletes the spring */
  def member_delete_ = ApiMember(
    name = "delete",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the spring stiffness. */
  def member_set_stiffness = ApiMember(
    name = "set stiffness",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "delete" -> member_delete_,
    "set stiffness" -> member_set_stiffness
  )
            

}
          
