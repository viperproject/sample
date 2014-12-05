
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Atomic Action
 *
 * An atomic action with no arguments
 *
 * @author Lucas Brutschy
 */

trait Default_TAtomic_Action extends AAny {

  lazy val typeName = TypeName("Atomic Action")
          
  /** Never used: Run the inline action. */
  def member_run = ApiMember(
    name = "run",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "run" -> member_run
  )
            

}
          
