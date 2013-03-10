package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Interval
import ch.ethz.inf.pm.sample.{Reporter, SystemParameters}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.domain.TouchDomain
import ch.ethz.inf.pm.td.semantics.{AAny, RichNativeSemantics}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.td.compiler.TouchSingletonProgramPoint
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.td.output.HTMLExporter

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

  /**
   *
   * The execution model is to
   *
   * (1) Initialize the global state to invalid
   * (2) Repeat:
   *    (2.1) Reset the local state
   *    (2.2) Run the method (interprocedurally)
   *    (2.3) Compute lfp (lambda x -> lub_e\in E(e(x))) where E is the set of events
   *
   */
  override def analyze[S <: State[S]](methods: List[String], entryState : S, output : OutputCollector) {

    val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]

    // Set up the environment
    SystemParameters.resetOutput
    MethodSummaries.reset[S]()
    SystemParameters.progressOutput.begin(" ANALYZING "+compiler.main.name)

    // We discover all fields from the API that are used in this set of classes. We will not instantiate anything else
    if(TouchAnalysisParameters.libraryFieldPruning) {
      compiler.relevantLibraryFields = RequiredLibraryFragmentAnalysis(compiler.parsedScripts)
      SystemParameters.resetOutput
      MethodSummaries.reset[S]()
    }

    // Set global state to invalid
    var curState = entryState
    for (v <- compiler.globalData) {

      val variable = VariableIdentifier(CFGGenerator.globalReferenceIdent(v.name.getName()),v.typ,v.programpoint)
      val leftExpr = new ExpressionSet(v.typ).add(variable)
      curState = curState.createVariable(leftExpr,v.typ,v.programpoint)

      // Numbers, Booleans and Strings are not initialized to invalid but to 0, false, ""
      val rightVal = v.typ.getName() match {
        case "String" => Constant("",v.typ,v.programpoint)
        case "Number" => Constant("0",v.typ,v.programpoint)
        case "Boolean" => Constant("false",v.typ,v.programpoint)
        case _ =>

          // There are three types of global data:
          //  (1) Regular global variables / objects, which are initialized to invalid
          //  (2) Global objects that are read-only and are initialized to some default object (Tile)
          //  (3) Global objects that represents read-only artwork that is initialized from some URL.
          if(v.modifiers.contains(ResourceModifier)) {
            curState = RichNativeSemantics.Top[S](v.typ.asInstanceOf[TouchType])(curState,v.programpoint)
            curState.getExpression().getSetOfExpressions.head
          } else if (v.modifiers.contains(ReadOnlyModifier)) {
            curState = RichNativeSemantics.New[S](v.typ.asInstanceOf[TouchType])(curState,v.programpoint)
            curState.getExpression().getSetOfExpressions.head
          } else {
            Constant("invalid",v.typ.asInstanceOf[TouchType],v.programpoint)
          }

      }

      val rightExpr = new ExpressionSet(v.typ).add(rightVal)
      curState = curState.assignVariable(leftExpr,rightExpr)
    }

    // Initialize the fields of singletons (the environment)
    for (sem <- SystemParameters.compiler.asInstanceOf[TouchCompiler].getNativeMethodsSemantics()) {
      if(sem.isInstanceOf[AAny]) {
        val typ = sem.asInstanceOf[AAny].getTyp
        if(typ.isSingleton &&
          (!TouchAnalysisParameters.libraryFieldPruning ||
          SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.getName))) {
          val singletonProgramPoint = TouchSingletonProgramPoint(typ.getName)
          curState = RichNativeSemantics.Top[S](typ)(curState,singletonProgramPoint)
          val obj = curState.getExpression()
          val variable = new ExpressionSet(typ).add(VariableIdentifier(typ.getName,typ,singletonProgramPoint))
          curState = RichNativeSemantics.Assign[S](variable,obj)(curState,singletonProgramPoint)
        }
      }
    }

    // The first fixpoint, which is computed over several executions of the same script
    val result = lfp(curState, {initialState:S =>

      // TODO: Reset local state

      // Execute abstract semantics of each public method (or the ones selected in the GUI)
      val exitStates = for ((c,x) <- compiler.getPublicMethods) yield {
        if (methods.isEmpty || methods.contains(x.name.toString)) {
          Some(analyzeMethod(c,x,initialState))
        } else None
      }

      // Compute the least upper bound of all public method exit states
      val exitState = exitStates.flatten.foldLeft(initialState.bottom())({(
         stateLeft:S,stateRight:S) => initialState.lub(stateLeft,stateRight)
      })

      // Compute the fixpoint over all events
      lfp(exitState,{s:S =>
        var cur = s
        for ((c,e) <- compiler.events) {
          cur = cur.lub(cur,analyzeMethod(c,e,s))
        }
        cur
      })

    })

    // Check properties on the results
    SystemParameters.progressOutput.end()
    if (SystemParameters.property!=null) {
      for ((pp,(clazz,method,cfgEx)) <- MethodSummaries.getSummaries) {
        SystemParameters.property.check(clazz.name.getThisType(), method, cfgEx.asInstanceOf[ControlFlowGraphExecution[S]], output)
      }
      SystemParameters.property.finalizeChecking(output)
    }

    // Print some html
    if (TouchAnalysisParameters.exportAsHtml) HTMLExporter()


  }

  private def analyzeMethod[S <: State[S]](callType:ClassDefinition,callTarget:MethodDeclaration,entryState:S):S = {


    val exitState = MethodSummaries.collect[S](callTarget.programpoint,callType,callTarget,entryState,Nil)


    exitState

  }

  /**
   * Computes the least fix point for states
   */
  private def lfp[S <: State[S]](initialState:S,singleIteration:(S => S)):S = {

    var iteration = 1
    var prev = initialState
    var cur = prev.lub(prev,singleIteration(prev))
    while(!cur.lessEqual(prev)) {
      prev = cur
      iteration=iteration+1
      if(iteration > SystemParameters.wideningLimit) cur = prev.widening(prev,singleIteration(prev))
      else cur = prev.lub(prev,singleIteration(prev))
    }

    cur

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
  def check[S <: State[S]](classT : Type, methodName : MethodDeclaration, result : ControlFlowGraphExecution[S], printer : OutputCollector) {}
  def finalizeChecking(printer : OutputCollector) {}
}

