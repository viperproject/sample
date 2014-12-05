
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.defsemantics.Default_GComparison
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Customizes the abstract semantics of Comparison
 *
 * An atomic comparison action
 *
 * @author Lucas Brutschy
 */

case class GComparison (TElt:AAny) extends Default_GComparison
          
