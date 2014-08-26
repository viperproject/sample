
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Number Converter
 *
 * An atomic conversion function to number
 *
 * @author Lucas Brutschy
 */ 

object TNumber_Converter {

  val typName = "Number Converter"
  val typ = DefaultTouchType(typName)

}

class TNumber_Converter extends AAny {

  def getTyp = TNumber_Converter.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Run the inline action. */
    case "run" =>
    //  val List(elt) = parameters // Elt
      TopWithInvalid[S](TNumber.typ,"inline action may return invalid (not checked)")

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
