package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */
class SAssert extends Any {

  def getTypeName = "assert"

  def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Break if the given assertion does not hold */
    case "is_true" =>
      Error[S]((parameters.head).not(), "Assertion "+parameters.head+" does not hold!")
      Skip

    /** Break if the given assertion does hold */
    case "is_false" =>
      Error[S](parameters.head, "Assertion not( "+parameters.head+" ) does not hold!")
      Skip

    case _ =>
      Unimplemented[S](this0.getType().toString+"."+method)

  }

}