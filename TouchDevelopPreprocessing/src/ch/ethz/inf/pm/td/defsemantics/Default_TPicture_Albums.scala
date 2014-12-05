
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Picture Albums
 *
 * A collection of picture albums
 *
 * @author Lucas Brutschy
 */

trait Default_TPicture_Albums extends ALinearCollection {

  lazy val typeName = TypeName("Picture Albums")
          
  def keyType = TNumber

  def valueType = TPicture_Album


}
          
