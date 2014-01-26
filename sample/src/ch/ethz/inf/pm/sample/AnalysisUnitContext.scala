package ch.ethz.inf.pm.sample

import ch.ethz.inf.pm.sample.oorepresentation.{ControlFlowGraph, MethodDeclaration, Type}

case class AnalysisUnitContext(method: MethodDeclaration) {

  // legacy helpers
  def methodName: String = if (method != null)  method.name.toString else null
  def cfg: ControlFlowGraph = method.body
  def clazzType: Type = method.classDef.typ
  def clazz = method.classDef
}
