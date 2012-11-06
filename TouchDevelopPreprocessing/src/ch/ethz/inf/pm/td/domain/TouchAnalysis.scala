package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.{State, SemanticAnalysis}
import ch.ethz.inf.pm.sample.property.{OutputCollector, DivisionByZero, SingleStatementProperty, Property}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ControlFlowGraphExecution, Type, NativeMethodSemantics}
import ch.ethz.inf.pm.td.semantics.Environment
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Interval

/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 5:50 PM
 * 
 */
class TouchAnalysis[D <: NumericalDomain[D]] extends SemanticAnalysis[TouchDomain[D]] {

  var domain: D = null.asInstanceOf[D]

  def getLabel(): String = "TouchDevelop analysis"

  def parameters(): List[(String, Any)] = List(("Domain", List("Sign", "Interval")))

  /** Initialize with some arbitrary numerical domain. Extend this to APRON later */
  def setParameter(label: String, value: Any) { label match {
    case "Domain" => value match {
      case "Sign" => domain = new BoxedNonRelationalNumericalDomain(new Sign(SignValues.T)).asInstanceOf[D]
      case "Interval" => domain = new BoxedNonRelationalNumericalDomain(new Interval(0, 0)).asInstanceOf[D]
    }
  }}

  /** Initialize the environment to TOP for now. This is were you could implement assumptions about the environment */
  def getInitialState(): TouchDomain[D] = {
    var ret = new TouchDomain(domain.asInstanceOf[D]).top()
    for (e <- Environment.envs) {
      ret = ret.createVariable(e,e.getType())
      ret = ret.setToTop(e)
    }
    ret
  }

  override def reset() { Unit }

  def getProperties(): Set[Property] = Set(new NoProperty())

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil


}

/**
 * Check the empty property
 */
class NoProperty extends Property {
  def getLabel() = ""
  def check[S <: State[S]](classT : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) {}
  def finalizeChecking(printer : OutputCollector) {}
}