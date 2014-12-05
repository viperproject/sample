
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Song Albums
 *
 * A collection of albums
 *
 * @author Lucas Brutschy
 */

trait Default_TSong_Albums extends ALinearCollection {

  lazy val typeName = TypeName("Song Albums")
          
  def keyType = TNumber

  def valueType = TSong_Album


}
          
