
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of String Converter
 *
 * An atomic conversion function to string
 *
 * @author Lucas Brutschy
 */ 

object TString_Converter {

  val typName = "String Converter"
  val typ = DefaultTouchType(typName)

}

class TString_Converter extends AAny {

  def getTyp = TString_Converter.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Run the inline action. */
    case "run" =>
      TopWithInvalid[S](TString.typ,"inline action may return invalid (not checked)")

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
