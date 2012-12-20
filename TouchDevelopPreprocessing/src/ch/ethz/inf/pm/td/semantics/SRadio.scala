
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of radio
 *
 * Access to the radio
 *
 * @author Lucas Brutschy
 */ 

object SRadio {

  val typName = "radio"
  val typ = TouchType(typName,isSingleton = true,List())

}

class SRadio extends AAny {

  def getTyp = SRadio.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the frequency */
    // case "frequency" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the frequency */
    //   val field_frequency = new TouchField("frequency",TNumber.typ)

    /** Indicates if the radio is on */
    // case "is_playing" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if the radio is on */
    //   val field_is_playing = new TouchField("is_playing",TBoolean.typ)

    /** Creates a link to a radio frequency */
    // case "link_frequency" => 
    //   val List(name,frequency) = parameters // String,Number
    //   Return[S](Valid(TLink.typ))

    /** Sets the frequency */
    // case "set_frequency" => 
    //   val List(frequency) = parameters // Number
    //   Skip;

    /** Gets the signal strength */
    // case "signal_strength" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the signal strength */
    //   val field_signal_strength = new TouchField("signal_strength",TNumber.typ)

    /** Turns on the radio */
    // case "start" => 
    //   Skip;

    /** Turns off the radio */
    // case "stop" => 
    //   Skip;

    // FIELDS: , field_frequency, field_is_playing, field_signal_strength

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
