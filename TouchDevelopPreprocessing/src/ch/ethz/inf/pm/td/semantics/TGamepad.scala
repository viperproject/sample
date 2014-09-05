
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Gamepad
 *
 * A snapshot of the gamepad state
 *
 * @author Lucas Brutschy
 */ 

object TGamepad extends AAny {

  lazy val typeName = TypeName("Gamepad")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Gets the `x` and `y` value of the selected axes. */
    // case "axes" => 
    //   val List(name) = parameters // String
    //   TopWithInvalid[S](TVector3)
    // DECLARATION AS FIELD: 
    //   /** Gets the `x` and `y` value of the selected axes. */
    //   lazy val field_axes = new TouchField("axes",TVector3.typeName)

    /** Gets the pressed value of a button. Returns 0 if button missing. */
    // case "button value" => 
    //   val List(name) = parameters // String
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Gets the pressed value of a button. Returns 0 if button missing. */
    //   lazy val field_button_value = new TouchField("button value",TNumber.typeName)

    /** Indicates if the gamepad data are identical */
    // case "equals" => 
    //   val List(other) = parameters // Gamepad
    //   TopWithInvalid[S](TBoolean)
    // DECLARATION AS FIELD: 
    //   /** Indicates if the gamepad data are identical */
    //   lazy val field_equals = new TouchField("equals",TBoolean.typeName)

    /** Gets the gamepad identifier */
    // case "id" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString)
    // DECLARATION AS FIELD: 
    //   /** Gets the gamepad identifier */
    //   lazy val field_id = new TouchField("id",TString.typeName)

    /** Gets the player index */
    // case "index" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Gets the player index */
    //   lazy val field_index = new TouchField("index",TNumber.typeName)

    /** Indicates if a button is pressed. Returns false if button missing. */
    // case "is button pressed" => 
    //   val List(name) = parameters // String
    //   TopWithInvalid[S](TBoolean)
    // DECLARATION AS FIELD: 
    //   /** Indicates if a button is pressed. Returns false if button missing. */
    //   lazy val field_is_button_pressed = new TouchField("is button pressed",TBoolean.typeName)

    /** Indicates if the gamepad is still connected. */
    // case "is connected" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TBoolean)
    // DECLARATION AS FIELD: 
    //   /** Indicates if the gamepad is still connected. */
    //   lazy val field_is_connected = new TouchField("is connected",TBoolean.typeName)

    /** Gets the timestamp of this snapshot */
    // case "timestamp" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Gets the timestamp of this snapshot */
    //   lazy val field_timestamp = new TouchField("timestamp",TNumber.typeName)

    // FIELDS: field_axes, field_button_value, field_equals, field_id, field_index, field_is_button_pressed, field_is_connected, field_timestamp

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
