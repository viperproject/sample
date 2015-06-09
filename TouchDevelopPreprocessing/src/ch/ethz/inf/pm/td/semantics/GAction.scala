
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TypeList, ApiParam}
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Customizes the abstract semantics of Action1
 *
 * A possibly non-atomic single argument action
 *
 * @author Lucas Brutschy
 */

case class GAction (typeName:TypeName, inNames:List[TypeName] = List.empty, outNames:List[TypeName] = List.empty) extends AAction {

  lazy val in:List[AAny]  = TypeList.toTouchTypes(inNames)
  lazy val out:List[AAny] = TypeList.toTouchTypes(outNames)

  override def actionReturnValue = {
    assert(out.size <= 1)
    if (out.isEmpty) TNothing else out.head
  }

  override def actionArguments: List[ApiParam] = in.map(ApiParam(_))

}