package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Number
 *
 * A number (possibly negative and/or fractional)
 *
 * @author Lucas Brutschy
 */
object TNumber {

  val typName = "Number"
  val typ = new TouchType(typName,isSingleton = false,isImmutable = true)

}

class TNumber extends AAny {

  def getTyp = TNumber.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String,parameters:List[ExpressionSet],returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

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

    case "and" => Return((this0 unequal 0) && parameters.head)
    case "or" => Return((this0 unequal 0) || parameters.head)
    case "not" => Return(this0 equal 0)

    /** Interprets a number as a unicode value and converts it to the single character string */
    case "to character" =>
      Top[S](TString.typ)

    /** Interprets the number as a ARGB (alpha, red, green, blue) color */
    case "to color" =>
      Top[S](TColor.typ) // TODO: This should be possible to implement

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
