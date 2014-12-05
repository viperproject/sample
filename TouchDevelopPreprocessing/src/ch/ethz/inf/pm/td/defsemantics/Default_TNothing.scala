
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Nothing
 *
 * Represents no value of interest
 *
 * @author Lucas Brutschy
 */

trait Default_TNothing extends AAny {

  lazy val typeName = TypeName("Nothing")
          

}
          
