
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Action1
 *
 * A possibly non-atomic single argument action
 *
 * @author Lucas Brutschy
 */

trait Default_GAction1 extends AAction {

  def TT:AAny
           

  lazy val typeName = TypeName("Action1", List(TT.typeName))
          
  def actionArguments = List(ApiParam(TT))


}
          
