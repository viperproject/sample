
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Event Binding
 *
 * A handler attached to an event.
 *
 * @author Lucas Brutschy
 */

trait Default_TEvent_Binding extends AAny {

  lazy val typeName = TypeName("Event Binding")
          
  /** Never used: Detaches the handler from the event. */
  def member_delete_ = ApiMember(
    name = "delete",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "delete" -> member_delete_
  )
            

}
          
