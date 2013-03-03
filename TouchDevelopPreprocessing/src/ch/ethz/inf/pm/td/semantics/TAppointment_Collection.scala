
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Appointment Collection
 *
 * A collection of appointments
 *
 * @author Lucas Brutschy
 */ 

object TAppointment_Collection {

  val typName = "Appointment_Collection"
  val typ = new TouchCollection(typName,TNumber.typName,TAppointment.typName)

}

class TAppointment_Collection extends ACollection {

  def getTyp = TAppointment_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
