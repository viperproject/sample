
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Appointment Collection
 *
 * A collection of appointments
 *
 * @author Lucas Brutschy
 */

trait Default_TAppointment_Collection extends ALinearCollection {

  lazy val typeName = TypeName("Appointment Collection")
          
  def keyType = TNumber

  def valueType = TAppointment


}
          
