
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{ValidPureSemantics, ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.defsemantics.Default_GCollection

/**
 * Customizes the abstract semantics of Collection
 *
 * A collection of objects
 *
 * @author Lucas Brutschy
 */

case class GCollection (TT:AAny) extends Default_GCollection {

  override lazy val member_join = super.member_join.copy(semantics = ValidPureSemantics)

}
