package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.oorepresentation.{Statement, ControlFlowGraphExecution, Type, NativeMethodSemantics}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Interval
import ch.ethz.inf.pm.sample.{Reporter, SystemParameters}
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.domain.TouchDomain

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

  def getInitialState(): TouchDomain[D] = {
    new TouchDomain(domain).top()
  }

  override def reset() { Unit }

  def getProperties(): Set[Property] = Set(new ApronProperty().asInstanceOf[Property], new SingleStatementProperty(new BottomVisitor), new NoProperty)

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil

  override def analyze[S <: State[S]](methods: List[String], entryState : S, output : OutputCollector) {
    val methodSet = methods.toSet[String]
    for ((c,x) <- SystemParameters.compiler.asInstanceOf[TouchCompiler].getRunnableMethods) {
      if (methodSet.isEmpty || methodSet.contains(x.name.toString)) {
        if(SystemParameters.progressOutput!=null) SystemParameters.progressOutput.begin("METHOD: "+c.name.toString+"."+x.name.toString)
        SystemParameters.currentMethod = x.name.toString
        val s = x.forwardSemantics[S](entryState)
        if(SystemParameters.progressOutput!=null) SystemParameters.progressOutput.end()
        if(SystemParameters.property!=null) {
          SystemParameters.property.check(c.name.getThisType(), x.name.toString, s, output)
        }
        SystemParameters.currentMethod = null
      }
    }
    if(SystemParameters.property!=null) {
      SystemParameters.property.finalizeChecking(output)
    }
  }

}

/**
 * Check if something is bottom. Might be due to unreachable code.
 */
class BottomVisitor extends Visitor {

  def getLabel() = "BottomChecker"

  /**
   * Check the property over a single state
   *
   * @param state the abstract state
   * @param statement the statement that was executed after the given abstract state
   * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
   */
  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) {
    if (state.lessEqual(state.bottom())) {
      Reporter.reportBottom("State is bottom",statement.getPC())
    }
  }

}

/**
 * Check the empty property
 */
class NoProperty extends Property {
  def getLabel() = ""
  def check[S <: State[S]](classT : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) {}
  def finalizeChecking(printer : OutputCollector) {}
}

