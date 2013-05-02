package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{CFGGenerator, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{Constant, VariableIdentifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * This is empty but needs to be there as a type
 *
 * @author Lucas Brutschy
 */

object SLibs {

  val typName = "♻"
  val typ = new TouchType(typName,isSingleton = true)

}

class SLibs extends AAny {

  def getTyp = SLibs.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = {

    val typ = new TouchType("♻"+method,isSingleton = true)
    state.setExpression(new ExpressionSet(typ).add(Constant("",typ,pp)))

  }
}
