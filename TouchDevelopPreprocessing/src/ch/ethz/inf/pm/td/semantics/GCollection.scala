
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.defsemantics.Default_GCollection

/**
 * Customizes the abstract semantics of Collection
 *
 * A collection of objects
 *
 * @author Lucas Brutschy
 */

case class GCollection (TT:AAny) extends Default_GCollection
