
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Device Collection
 *
 * A collection of devices
 *
 * @author Lucas Brutschy
 */

trait Default_TDevice_Collection extends ALinearCollection {

  lazy val typeName = TypeName("Device Collection")
          
  def keyType = TNumber

  def valueType = TDevice


}
          
