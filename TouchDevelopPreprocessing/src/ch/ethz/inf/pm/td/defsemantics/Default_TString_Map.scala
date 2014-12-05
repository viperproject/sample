
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of String Map
 *
 * A map from strings to strings
 *
 * @author Lucas Brutschy
 */

trait Default_TString_Map extends AMap {

  lazy val typeName = TypeName("String Map")
          
  def keyType = TString

  def valueType = TString


}
          
