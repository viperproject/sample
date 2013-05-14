
package ch.ethz.inf.pm.td.semantics

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

  val typName = "Appointment Collection"
  val typ = new TouchCollection(typName,TNumber.typName,TAppointment.typName, immutableCollection = true)

}

class TAppointment_Collection extends ACollection {

  def getTyp = TAppointment_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
