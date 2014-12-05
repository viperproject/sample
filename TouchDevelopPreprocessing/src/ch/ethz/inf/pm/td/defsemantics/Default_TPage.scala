
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Page
 *
 * A page on a wall
 *
 * @author Lucas Brutschy
 */

trait Default_TPage extends AAny {

  lazy val typeName = TypeName("Page")
          
  /** Sometimes used: Gets a value indicating if the page is equal to the other */
  def member_equals = ApiMember(
    name = "equals",
    paramTypes = List(ApiParam(TPage)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Sets a handler that runs when the page is popped. */
  def member_on_navigated_from = ApiMember(
    name = "on navigated from",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "equals" -> member_equals,
    "on navigated from" -> member_on_navigated_from
  )
            

}
          
