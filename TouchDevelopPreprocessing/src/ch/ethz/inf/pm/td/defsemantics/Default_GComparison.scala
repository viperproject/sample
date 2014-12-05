
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Comparison
 *
 * An atomic comparison action
 *
 * @author Lucas Brutschy
 */

trait Default_GComparison extends AAny {

  def TElt:AAny
           

  lazy val typeName = TypeName("Comparison", List(TElt.typeName))
          
  /** Never used: Run the inline action. */
  def member_run = ApiMember(
    name = "run",
    paramTypes = List(ApiParam(TElt), ApiParam(TElt)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "run" -> member_run
  )
            

}
          
