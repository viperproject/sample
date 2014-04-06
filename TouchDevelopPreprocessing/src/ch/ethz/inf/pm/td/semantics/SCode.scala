package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{TouchCompiler, DefaultTouchType, TouchType}
import RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.interpreter.{ConcreteInterpreter, ConcreteInterpreterState, TouchValue}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.analysis.MethodSummaries
import ch.ethz.inf.pm.sample.reporting.Reporter

/**
 * Specifies the abstract semantics of code
 *
 * Lists actions defined in the current script
 *
 * @author Lucas Brutschy
 */

object SCode {

  val typName = "code"
  val typ = DefaultTouchType(typName, isSingleton = true)

}

class SCode extends AAny {

  def getTyp = SCode.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = {

    CallLocalAction[S](method,parameters)

  }

  override def backwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)(implicit pp: ProgramPoint, post: S, oldPreState: S): S = {
    val context = SystemParameters.analysisUnitContext
    val classType = context.clazzType
    SystemParameters.compiler.asInstanceOf[TouchCompiler].getMethodWithClassDefinition(method,classType, parameters map (_.getType())) match {
      case Some(mdecl) =>
        val res = MethodSummaries.backwardCollect(pp, mdecl, post, parameters)
        res
      case _ =>
        Reporter.reportImprecision("Could not find method "+method,pp)
        post.top()
    }
  }

  override def concreteSemantics(this0: TouchValue, method: String, params: List[TouchValue],
                                 interpreter: ConcreteInterpreter, pp: ProgramPoint): TouchValue = {
    interpreter.resolveAndExecuteMethod(method, params, Some(pp))
  }
}