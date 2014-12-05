
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.defsemantics.Default_GAction1
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Customizes the abstract semantics of Action1
 *
 * A possibly non-atomic single argument action
 *
 * @author Lucas Brutschy
 */

case class GAction1 (TT:AAny) extends Default_GAction1