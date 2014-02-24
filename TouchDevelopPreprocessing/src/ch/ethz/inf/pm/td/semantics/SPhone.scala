
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

/**
 * Specifies the abstract semantics of phone
 *
 * Phone numbers, vibrate, etc...
 *
 * @author Lucas Brutschy
 */ 

object SPhone {

  val typName = "Phone"
  val typ = DefaultTouchType(typName,isSingleton = true)

}

class SPhone extends AAny {

  def getTyp = SPhone.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Chooses an address from the contacts */
    case "choose address" =>
      val state1 = New[S](TLink.typ,Map(
        TLink.field_kind-> String("address"/*TODO*/)
      ))
      Return[S](state1.expr,Invalid(TLink.typ))(state1,pp)

    /** Chooses a phone number from the contact list */
    case "choose phone number" =>
      val state1 = New[S](TLink.typ,Map(
        TLink.field_kind -> String("phone number")
      ))
      Return[S](state1.expr,Invalid(TLink.typ))(state1,pp)

    /** Starts a phone call */
    case "dial phone number" =>
      val List(number) = parameters // String
      Skip

    /** Indicates if the phone is on 'battery' or 'external' power source. */
    case "power source" =>
      Return[S](String("battery"),String("external"))

    /** Allows the user to save the phone number */
    case "save phone number" =>
      val List(phone_number) = parameters // String
      // TODO?
      Skip

    /** Vibrates the phone for ... seconds (0.02 minimum) */
    case "vibrate" =>
      val List(seconds) = parameters // Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        Error[S](toRichExpression(seconds) < 0.02, "vibrate", "Given amount of seconds may be too small (must be >= 0.02)")
      }
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
