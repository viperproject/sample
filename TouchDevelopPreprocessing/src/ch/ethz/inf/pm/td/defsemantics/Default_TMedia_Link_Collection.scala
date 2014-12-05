
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Media Link Collection
 *
 * A list of media links on the home network
 *
 * @author Lucas Brutschy
 */

trait Default_TMedia_Link_Collection extends ALinearCollection {

  lazy val typeName = TypeName("Media Link Collection")
          
  def keyType = TNumber

  def valueType = TMedia_Link


}
          
