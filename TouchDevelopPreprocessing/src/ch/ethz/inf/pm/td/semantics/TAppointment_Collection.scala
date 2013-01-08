
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
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
  val typ = TouchType(typName,isSingleton = false,List())

}

class TAppointment_Collection extends AAny {

  def getTyp = TAppointment_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the appointment at index */
    // case "at" => 
    //   val List(index) = parameters // Number
    //   Return[S](Valid(TAppointment.typ))

    /** Gets the number of appointments */
    // case "count" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the number of appointments */
    //   val field_count = new TouchField("count",TNumber.typ)

    // FIELDS: , field_count

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
