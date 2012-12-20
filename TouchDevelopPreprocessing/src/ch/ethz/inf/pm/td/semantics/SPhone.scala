
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
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
  val typ = TouchType(typName,isSingleton = true,List())

}

class SPhone extends AAny {

  def getTyp = SPhone.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Chooses an address from the contacts */
    // case "choose_address" => 
    //   Return[S](Valid(TLink.typ))
    // DECLARATION AS FIELD: 
    //   /** Chooses an address from the contacts */
    //   val field_choose_address = new TouchField("choose_address",TLink.typ)

    /** Chooses a phone number from the contact list */
    // case "choose_phone_number" => 
    //   Return[S](Valid(TLink.typ))
    // DECLARATION AS FIELD: 
    //   /** Chooses a phone number from the contact list */
    //   val field_choose_phone_number = new TouchField("choose_phone_number",TLink.typ)

    /** Starts a phone call */
    // case "dial_phone_number" => 
    //   val List(number) = parameters // String
    //   Skip;

    /** Indicates if the phone is on 'battery' or 'external' power source. */
    // case "power_source" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if the phone is on 'battery' or 'external' power source. */
    //   val field_power_source = new TouchField("power_source",TString.typ)

    /** Allows the user to save the phone number */
    // case "save_phone_number" => 
    //   val List(phone_number) = parameters // String
    //   Skip;

    /** Vibrates the phone for ... seconds (0.02 minimum) */
    // case "vibrate" => 
    //   val List(seconds) = parameters // Number
    //   Skip;

    // FIELDS: , field_choose_address, field_choose_phone_number, field_power_source

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
