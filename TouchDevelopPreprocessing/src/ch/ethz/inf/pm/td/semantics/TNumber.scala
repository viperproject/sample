package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichExpression._

/**
 * Specifies the abstract semantics of Number
 *
 * A number (possibly negative and/or fractional)
 *
 * @author Lucas Brutschy
 */
object TNumber {

  val typName = "Number"
  val typ = TouchType(typName,isSingleton = false)

}

class TNumber extends AAny {

  def getTyp = TNumber.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String,parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    case "≥" => Return(this0 >= parameters.head)
    case "≤" => Return(this0 <= parameters.head)
    case "=" => Return(this0 equal parameters.head)
    case "≠" => Return(this0 unequal parameters.head)
    case ">" => Return(this0 > parameters.head)
    case "<" => Return(this0 < parameters.head)
    case "+" => Return(this0 + parameters.head)
    case "*" => Return(this0 * parameters.head)
    case "-" => Return(this0 - parameters.head)
    case "/" => Return(this0 / parameters.head)

    /** Interprets a number as a unicode value and converts it to the single character string */
    case "to_character" =>
      Return[S](Valid(TString.typ))

    /** Interprets the number as a ARGB (alpha, red, green, blue) color */
    case "to_color" =>
      Return[S](Valid(TColor.typ)) // TODO: This should be possible to implement

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}
