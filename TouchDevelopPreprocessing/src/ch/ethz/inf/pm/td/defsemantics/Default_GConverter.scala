
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Converter
 *
 * A generic atomic conversion function
 *
 * @author Lucas Brutschy
 */

trait Default_GConverter extends AAny {

  def TFrom:AAny
  def TTo:AAny
           

  lazy val typeName = TypeName("Converter", List(TFrom.typeName, TTo.typeName))
          
  /** Never used: Run the inline action. */
  def member_run = ApiMember(
    name = "run",
    paramTypes = List(ApiParam(TFrom)),
    thisType = ApiParam(this),
    returnType = TTo,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "run" -> member_run
  )
            

}
          
