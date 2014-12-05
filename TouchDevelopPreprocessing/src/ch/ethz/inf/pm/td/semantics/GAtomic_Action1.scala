
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.defsemantics.Default_GAtomic_Action1
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Customizes the abstract semantics of Atomic Action1
 *
 * An atomic single argument action
 *
 * @author Lucas Brutschy
 */

case class GAtomic_Action1 (TT:AAny) extends Default_GAtomic_Action1
