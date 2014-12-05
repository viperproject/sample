
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Link Collection
 *
 * A list of links
 *
 * @author Lucas Brutschy
 */

trait Default_TLink_Collection extends AMutable_Collection {

  lazy val typeName = TypeName("Link Collection")
          
  def keyType = TNumber

  def valueType = TLink


}
          
