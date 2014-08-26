
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Gamepad
 *
 * A snapshot of the gamepad state
 *
 * @author Lucas Brutschy
 */ 

object TGamepad {

  val typName = "Gamepad"
  val typ = DefaultTouchType(typName)

}

class TGamepad extends AAny {

  def getTyp = TGamepad.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Gets the `x` and `y` value of the selected axes. */
    // case "axes" => 
    //   val List(name) = parameters // String
    //   TopWithInvalid[S](TVector3.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the `x` and `y` value of the selected axes. */
    //   val field_axes = new TouchField("axes",TVector3.typName)

    /** Gets the pressed value of a button. Returns 0 if button missing. */
    // case "button value" => 
    //   val List(name) = parameters // String
    //   TopWithInvalid[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the pressed value of a button. Returns 0 if button missing. */
    //   val field_button_value = new TouchField("button value",TNumber.typName)

    /** Indicates if the gamepad data are identical */
    // case "equals" => 
    //   val List(other) = parameters // Gamepad
    //   TopWithInvalid[S](TBoolean.typ)
    // DECLARATION AS FIELD: 
    //   /** Indicates if the gamepad data are identical */
    //   val field_equals = new TouchField("equals",TBoolean.typName)

    /** Gets the gamepad identifier */
    // case "id" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the gamepad identifier */
    //   val field_id = new TouchField("id",TString.typName)

    /** Gets the player index */
    // case "index" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the player index */
    //   val field_index = new TouchField("index",TNumber.typName)

    /** Indicates if a button is pressed. Returns false if button missing. */
    // case "is button pressed" => 
    //   val List(name) = parameters // String
    //   TopWithInvalid[S](TBoolean.typ)
    // DECLARATION AS FIELD: 
    //   /** Indicates if a button is pressed. Returns false if button missing. */
    //   val field_is_button_pressed = new TouchField("is button pressed",TBoolean.typName)

    /** Indicates if the gamepad is still connected. */
    // case "is connected" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TBoolean.typ)
    // DECLARATION AS FIELD: 
    //   /** Indicates if the gamepad is still connected. */
    //   val field_is_connected = new TouchField("is connected",TBoolean.typName)

    /** Gets the timestamp of this snapshot */
    // case "timestamp" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the timestamp of this snapshot */
    //   val field_timestamp = new TouchField("timestamp",TNumber.typName)

    // FIELDS: field_axes, field_button_value, field_equals, field_id, field_index, field_is_button_pressed, field_is_connected, field_timestamp

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
