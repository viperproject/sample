
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Tutorial
 *
 * Support for interactive tutorials.
 *
 * @author Lucas Brutschy
 */

trait Default_STutorial extends ASingleton {

  lazy val typeName = TypeName("Tutorial")
          
  /** Never used: [**dbg**] Show a suggestion to the user (eg., an error description) */
  def member_show_hint = ApiMember(
    name = "show hint",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Signal that the step is done. */
  def member_step_completed = ApiMember(
    name = "step completed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "show hint" -> member_show_hint,
    "step completed" -> member_step_completed
  )
            

}
          
