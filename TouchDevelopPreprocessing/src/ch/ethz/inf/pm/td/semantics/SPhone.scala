
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of phone
 *
 * Phone numbers, vibrate, etc...
 *
 * @author Lucas Brutschy
 */ 

object SPhone {

  val typName = "phone"
  val typ = new TouchType(typName,isSingleton = true,List())

}

class SPhone extends AAny {

  def getTyp = SPhone.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Chooses an address from the contacts */
    case "choose_address" =>
      val state1 = New[S](TLink.typ,Map(TLink.field_kind.asInstanceOf[Identifier] -> StringCst("address"/*TODO*/)))
      val valid = state1.getExpression()
      val invalid = Invalid(TLink.typ)
      Return[S](valid or invalid)

    /** Chooses a phone number from the contact list */
    case "choose_phone_number" =>
      val state1 = New[S](TLink.typ,Map(TLink.field_kind.asInstanceOf[Identifier] -> StringCst("phone number")))
      val valid = state1.getExpression()
      val invalid = Invalid(TLink.typ)
      Return[S](valid or invalid)

    /** Starts a phone call */
    case "dial_phone_number" =>
      val List(number) = parameters // String
      Skip

    /** Indicates if the phone is on 'battery' or 'external' power source. */
    case "power_source" =>
      Return[S](StringCst("battery") or StringCst("external"))

    /** Allows the user to save the phone number */
    case "save_phone_number" =>
      val List(phone_number) = parameters // String
      // TODO?
      Skip

    /** Vibrates the phone for ... seconds (0.02 minimum) */
    case "vibrate" =>
      val List(seconds) = parameters // Number
      Error[S](toRichExpression(seconds) < 0.02, "vibrate", "Given amout of seconds may be too small")
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
