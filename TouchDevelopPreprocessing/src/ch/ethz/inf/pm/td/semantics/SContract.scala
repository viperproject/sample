
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

/**
 * Specifies the abstract semantics of Contract
 *
 * Correctness helpers
 *
 * @author Lucas Brutschy
 */ 

object SContract {

  val typName = "Contract"
  val typ = new TouchType(typName,isSingleton = true)

}

class SContract extends AAny {

  def getTyp = SContract.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Specifies a precondition contract for the action; if the condition is false, execution fails. Does nothing for published scripts. */
    case "requires" =>
      val List(condition,message) = parameters // Boolean,String
      if (TouchAnalysisParameters.printValuesInWarnings)
        Error[S](condition.not(), "requires", "Precondition "+condition+" does not hold!")
      else
        Error[S](condition.not(), "requires", "Precondition does not hold!")
      Skip

    /** Checks for a condition; if the condition is false, execution fails. Does nothing for published scripts. */
    case "assert" =>
      val List(condition,message) = parameters // Boolean,String
      if (TouchAnalysisParameters.printValuesInWarnings)
        Error[S](condition.not(), "assert", "Assertion "+condition+" does not hold!")
      else
        Error[S](condition.not(), "assert", "Assertion does not hold!")
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
