
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Page Collection
 *
 * A collection of page
 *
 * @author Lucas Brutschy
 */

trait Default_TPage_Collection extends ALinearCollection {

  lazy val typeName = TypeName("Page Collection")
          
  def keyType = TNumber

  def valueType = TPage


}
          
