
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of radio
 *
 * Access to the radio
 *
 * @author Lucas Brutschy
 */ 

object SRadio {

  /** Gets the frequency */
  val field_frequency = new TouchField("frequency",TNumber.typ, Valid(TBoolean.typ)(null))

  /** Indicates if the radio is on */
  val field_is_playing = new TouchField("is_playing",TBoolean.typ, Valid(TBoolean.typ)(null))

  val typName = "radio"
  val typ = TouchType(typName,isSingleton = true,List(field_frequency, field_is_playing))

}

class SRadio extends AAny {

  def getTyp = SRadio.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a link to a radio frequency */
    case "link_frequency" =>
      val List(name,frequency) = parameters // String,Number
      New[S](TLink.typ,Map(TLink.field_name.asInstanceOf[Identifier] -> toRichExpression(name), TLink.field_kind -> StringCst("radio")))

    /** Gets the signal strength */
    case "signal_strength" =>
      Return[S](Valid(TNumber.typ))

    /** Turns on the radio */
    case "start" =>
      AssignField[S](this0,SRadio.field_is_playing,True)

    /** Turns off the radio */
    case "stop" =>
      AssignField[S](this0,SRadio.field_is_playing,True)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
