
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Printer Collection
 *
 * A collection of printers
 *
 * @author Lucas Brutschy
 */

trait Default_TPrinter_Collection extends ALinearCollection {

  lazy val typeName = TypeName("Printer Collection")
          
  def keyType = TNumber

  def valueType = TPrinter

  /** Never used: [**not implemented**] [**obsolete**] Adds a printer to the collection */
  def member_add = ApiMember(
    name = "add",
    paramTypes = List(ApiParam(TPrinter)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Clears the printers */
  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add" -> member_add,
    "clear" -> member_clear
  )
            

}
          
