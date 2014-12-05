
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Vector Action
 *
 * A place to hook up an action to execute in response to an event
 *
 * @author Lucas Brutschy
 */

trait Default_TVector_Action extends AAction {

  lazy val typeName = TypeName("Vector Action")
          
  def actionArguments = List(ApiParam(TNumber),ApiParam(TNumber),ApiParam(TNumber),ApiParam(TNumber))


}
          
