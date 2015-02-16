
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.ApiParam
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Customizes the abstract semantics of Action1
 *
 * A possibly non-atomic single argument action
 *
 * @author Lucas Brutschy
 */

case class GAction (typeName:TypeName,in:List[AAny],out:List[AAny]) extends AAction {

  override def actionReturnValue = {
    assert(out.size <= 1)
    if (out.isEmpty) TNothing else out.head
  }

  override def actionArguments: List[ApiParam] = in.map(ApiParam(_))

}