package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, CFGGenerator, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{EmptyScopeIdentifier, VariableIdentifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

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
}
