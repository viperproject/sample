
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Unknown
 *
 * an unknown value
 *
 * @author Lucas Brutschy
 */

trait Default_TUnknown extends AAny {

  lazy val typeName = TypeName("Unknown")
          
  /** Rarely used:  */
  def member_fun = ApiMember(
    name = "fun",
    paramTypes = List(ApiParam(TUnknown)),
    thisType = ApiParam(this),
    returnType = TUnknown,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "fun" -> member_fun
  )
            

}
          
