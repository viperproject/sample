package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, CFGGenerator, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{VariableIdentifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.interpreter.{ConcreteInterpreter, UnitV, ConcreteInterpreterState, TouchValue}

/**
 * This is empty but needs to be there as a type
 *
 * @author Lucas Brutschy
 */

object SData {

  val typName = "data"
  val typ = DefaultTouchType(typName,isSingleton = true)

}

class SData extends AAny {

  def getTyp = SData.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = {

    state.setExpression(ExpressionSet(VariableIdentifier(CFGGenerator.globalReferenceIdent(method))(returnedType, pp)))

  }

  override def concreteSemantics(this0: TouchValue, method: String, params: List[TouchValue],
                                 interpreter: ConcreteInterpreter, pp: ProgramPoint): TouchValue = {
    val state = interpreter.state
    val varId = interpreter.compiler.resolveGlobalData(method)
    state.getVar(varId)
  }
}
