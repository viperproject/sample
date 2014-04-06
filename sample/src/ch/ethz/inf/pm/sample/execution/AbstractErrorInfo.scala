package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, MethodDeclaration, ProgramPoint}

case class AbstractErrorInfo[S <: State[S]](pp: ProgramPoint, method: MethodDeclaration, errorExpr: ExpressionSet, state: S, errorState: S, cfgPosition: CFGPosition) {
  override def toString: String = {
    s"abstract error in '${method.name}' at $pp, neg. assertion $errorExpr"
  }
}
