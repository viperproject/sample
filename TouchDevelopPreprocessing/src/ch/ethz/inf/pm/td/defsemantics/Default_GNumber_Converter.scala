
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Number Converter
 *
 * An atomic conversion function to number
 *
 * @author Lucas Brutschy
 */

trait Default_GNumber_Converter extends AAny {

  def TElt:AAny
           

  lazy val typeName = TypeName("Number Converter", List(TElt.typeName))
          
  /** Never used: Run the inline action. */
  def member_run = ApiMember(
    name = "run",
    paramTypes = List(ApiParam(TElt)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "run" -> member_run
  )
            

}
          
