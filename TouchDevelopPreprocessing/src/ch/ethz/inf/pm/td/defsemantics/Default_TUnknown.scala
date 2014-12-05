
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Unknown
 *
 * an unknown value
 *
 * @author Lucas Brutschy
 */

trait Default_TUnknown extends AAny {

  lazy val typeName = TypeName("Unknown")
          

}
          
