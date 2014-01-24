package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Interval
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.domain._
import ch.ethz.inf.pm.td.semantics.{AAny, RichNativeSemantics}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import ch.ethz.inf.pm.td.output.FileSystemExporter
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.{StringValueDomain, Bricks, StringDomain}
import ch.ethz.inf.pm.sample.property.WarningProgramPoint
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import scala.Some
import ch.ethz.inf.pm.td.domain.InvalidExpression
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.td.compiler.TouchSingletonProgramPoint
import ch.ethz.inf.pm.sample.reporting.Reporter

/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 5:50 PM
 * 
 */
class TouchAnalysis[D <: NumericalDomain[D], V<:StringValueDomain[V], S<:StringDomain[V,S]]
  //extends SemanticAnalysis[TouchStringsAnd[InvalidAnd[D],V,S]] {
  extends SemanticAnalysis[StringsAnd[InvalidAnd[D],V,S]] {

  protected val STRING_DOMAIN = "StringDomain"
  protected val NUMERICAL_DOMAIN = "Domain"

  var domain: String = ""

  var stringDomain: String = ""

  def getLabel(): String = "TouchDevelop analysis"

  protected def numericalDomainList = (NUMERICAL_DOMAIN, List("Sign", "Interval"))

  protected def stringDomainList = (STRING_DOMAIN, List("KSet", "Bricks"))

  def parameters(): List[(String, Any)] = List(numericalDomainList, stringDomainList)

  def setParameter(label: String, value: Any) { label match {
    case NUMERICAL_DOMAIN => domain = value.toString
    case STRING_DOMAIN => stringDomain = value.toString
  }}

  def getInitialState(): StringsAnd[InvalidAnd[D],V,S] = {
    val numericSubDomain = domain match{
      case "Sign" => new BoxedNonRelationalNumericalDomain(new Sign(SignValues.T)).asInstanceOf[D]
      case "Interval" => new BoxedNonRelationalNumericalDomain(new Interval(0, 0)).asInstanceOf[D]
    }

    val invalidAndSubDomain = new InvalidAnd(numericSubDomain)

    stringDomain match{
      case "Bricks" => new StringsAnd[InvalidAnd[D],V,S](invalidAndSubDomain, new Bricks().asInstanceOf[S])
      case _ => new StringsAnd[InvalidAnd[D],V,S](invalidAndSubDomain)
    }


  }

  override def reset() { Unit }

  override def getProperties: List[Property] = {

    val bottom = new SingleStatementProperty(new BottomVisitor)
    val alarm = new SingleStatementProperty(new AlarmVisitor)
    val imprecision = new SingleStatementProperty(new ImprecisionVisitor)
    val empty = new NoProperty

    val allChecks = new ComposedProperty("All checks", bottom, new ComposedProperty("", alarm, imprecision))

    List(allChecks, alarm, imprecision, bottom, empty)
  }

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil


  def analyze[S <: State[S]](entryState : S) {
    analyze(Nil, entryState, new OutputCollector)
  }

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
  override def analyze[S <: State[S]](methods: List[String], entryState : S, output : OutputCollector)
     : List[(Type, MethodDeclaration, CFGState[S])] = {
    val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]

    println("version 01.11.13 [1]")

    // Set up the environment
    SystemParameters.resetOutput
    MethodSummaries.reset[S]()
    SystemParameters.progressOutput.begin(" ANALYZING "+compiler.main.name)

    // We discover all fields from the API that are used in this set of classes. We will not instantiate anything else
    //SystemParameters.progressOutput.begin("Library fragment analysis")
    if(TouchAnalysisParameters.libraryFieldPruning) {
      compiler.relevantLibraryFields = RequiredLibraryFragmentAnalysis(compiler.parsedScripts)
      compiler.relevantLibraryFields = compiler.relevantLibraryFields ++ Set("data","art","records","code")
        SystemParameters.resetOutput
      MethodSummaries.reset[S]()
    }
    //SystemParameters.progressOutput.end()


    // Initialize the fields of singletons (the environment)
    var curState = entryState
    for (sem <- SystemParameters.compiler.asInstanceOf[TouchCompiler].getNativeMethodsSemantics()) {
      if(sem.isInstanceOf[AAny]) {
        val typ = sem.asInstanceOf[AAny].getTyp
        if(typ.isSingleton &&
          (!TouchAnalysisParameters.libraryFieldPruning ||
            SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.name))) {
          val singletonProgramPoint = TouchSingletonProgramPoint(typ.name)
          if(typ.name == "records")
            if ( ! TouchAnalysisParameters.singleExecution && !compiler.isInLibraryMode) {
              curState = RichNativeSemantics.New[S](typ)(curState,singletonProgramPoint)
            } else {
              curState = RichNativeSemantics.Top[S](typ)(curState,singletonProgramPoint)
            }
          else
            curState = RichNativeSemantics.Top[S](typ)(curState,singletonProgramPoint)
          val obj = curState.getExpression
          val variable = ExpressionSet(VariableIdentifier(typ.name.toLowerCase, typ, singletonProgramPoint))
          curState = RichNativeSemantics.Assign[S](variable,obj)(curState,singletonProgramPoint)
        }
      }
    }

    // Set global state to invalid
    for (v <- compiler.globalData) {

      val variable = VariableIdentifier(CFGGenerator.globalReferenceIdent(v.variable.getName),v.typ,v.programpoint)
      val leftExpr = ExpressionSet(variable)
      curState = curState.createVariable(leftExpr,v.typ,v.programpoint)

      val rightVal =
        if ( ! TouchAnalysisParameters.singleExecution && !compiler.isInLibraryMode) {

          // We analyze executions separately. In the first execution of the script, global fields are invalid
          // except for the obvious exception (art, read-only, primitives)
          // There are three types of global data:
          //  (1) Regular global variables / objects, which are initialized to invalid
          //  (2) Global objects that are read-only and are initialized to some default object (Tile)
          //  (3) Global objects that represents read-only artwork that is initialized from some URL.
          if(v.modifiers.contains(ResourceModifier)) {
            curState = RichNativeSemantics.Top[S](v.typ.asInstanceOf[TouchType])(curState,v.programpoint)
            curState.getExpression
          } else if (v.modifiers.contains(ReadOnlyModifier)) {
            curState = RichNativeSemantics.New[S](v.typ.asInstanceOf[TouchType])(curState,v.programpoint)
            curState.getExpression
          } else {
            toExpressionSet(toRichExpression(v.typ.name match {
              case "String" => Constant("",v.typ,v.programpoint)
              case "Number" => Constant("0",v.typ,v.programpoint)
              case "Boolean" => Constant("false",v.typ,v.programpoint)
              case _ => InvalidExpression(v.typ.asInstanceOf[TouchType],v.programpoint)
            }))
          }

        } else {

          // We analyze one execution in top state.
          if(v.modifiers.contains(ResourceModifier)) {
            curState = RichNativeSemantics.Top[S](v.typ.asInstanceOf[TouchType])(curState,v.programpoint)
            curState.getExpression
          } else if (v.modifiers.contains(ReadOnlyModifier)) {
            curState = RichNativeSemantics.New[S](v.typ.asInstanceOf[TouchType])(curState,v.programpoint)
            curState.getExpression
          } else {
            curState = RichNativeSemantics.TopWithInvalid[S](v.typ.asInstanceOf[TouchType])(curState,
              if (TouchAnalysisParameters.fullAliasingInGenericInput) new DummyProgramPoint else v.programpoint)
            curState.getExpression
          }

        }

      curState = curState.createVariable(leftExpr,leftExpr.getType(),v.programpoint)
      curState = curState.assignVariable(leftExpr,rightVal)
    }

    // The first fixpoint, which is computed over several executions of the same script
    if ( ! TouchAnalysisParameters.singleExecution && !compiler.isInLibraryMode)
      lfp(curState, analyzeExecution(compiler,methods)(_:S))
    else
      analyzeExecution(compiler,methods)(curState)

    val summaries = MethodSummaries.getSummaries[S]
    // Check properties on the results
    val mustCheck = (s: MethodSummary[S]) => s.method.classDef == compiler.main || !TouchAnalysisParameters.reportOnlyAlarmsInMainScript
    val results = for (s@MethodSummary(_, mdecl, cfgState) <- summaries.values.toList if mustCheck(s))
                  yield (mdecl.classDef.typ, mdecl, cfgState)

    if (SystemParameters.property!=null) {
      SystemParameters.propertyTimer.start()
      SystemParameters.property.check(results, output)
      SystemParameters.property.finalizeChecking(output)
      SystemParameters.propertyTimer.stop()
    }

    FileSystemExporter(compiler)

    SystemParameters.progressOutput.end()
    results
  }

  private def analyzeExecution[S <: State[S]](compiler:TouchCompiler,methods:List[String])(initialState:S):S = {

    var methodsToBeAnalyzed = compiler.getPublicMethods

    // filter out methods that contain anything other than number and string as arguments
    if(!compiler.isInLibraryMode) {
      methodsToBeAnalyzed = methodsToBeAnalyzed filter { tm =>
        !tm.arguments.head.exists{ x:VariableDeclaration => x.typ.isObject }
      }
    }

    if (TouchAnalysisParameters.treatPrivateMethodLikePublicMethods)
      methodsToBeAnalyzed = methodsToBeAnalyzed ++ compiler.getPrivateMethods
    if (!methods.isEmpty) methodsToBeAnalyzed = methodsToBeAnalyzed.filter { tm =>
      val methodId = tm.name
      methods.contains(methodId.toString)
    }

    // Execute abstract semantics of each public method (or the ones selected in the GUI)
    val exitStates = for (mdecl <- methodsToBeAnalyzed) yield {
      Some(analyzeMethod(mdecl, initialState))
    }

    // Compute the least upper bound of all public method exit states
    val exitState = exitStates.flatten.foldLeft(initialState.bottom())({
      (stateLeft:S,stateRight:S) =>
        stateLeft.lub(stateRight)
    })

    // Compute the fixpoint over all events
    var result = if ( ! TouchAnalysisParameters.singleEventOccurrence ) {
      lfp(exitState,analyzeEvents(compiler,methods)(_:S))
    } else {
      analyzeEvents(compiler,methods)(exitState)
    }

    // Join the the normal exit state with all abnormal exit states
    result = MethodSummaries.joinAbnormalExits(result)

    result.removeExpression()
  }

  private def analyzeEvents[S <: State[S]](compiler:TouchCompiler,methods:List[String])(s:S):S = {

    var cur = s
    for (mdecl <- compiler.events) {
      cur = cur.lub(analyzeMethod(mdecl, s, localHandlerScope = MethodSummaries.getClosureEntry[S](mdecl.name.toString)))
    }

    resetEnv(cur)
  }


  private def analyzeMethod[S <: State[S]](callTarget: MethodDeclaration, entryState:S,localHandlerScope:Option[S] = None):S = {

    val exitState = MethodSummaries.collect[S](callTarget.programpoint, callTarget, entryState,Nil,localHandlerScope = localHandlerScope)

    resetEnv(exitState)

  }

  private def resetEnv[S <: State[S]](s:S):S = {

    if (TouchAnalysisParameters.resetEnv) {

      var curState = s

      // Remove Env
      curState = curState.pruneVariables({
        case id:VariableIdentifier =>
          id.getType.asInstanceOf[TouchType].isSingleton &&
          id.getType.name != "art" &&
          id.getType.name != "data" &&
          id.getType.name != "code" &&
          id.getType.name != "records"
        case _ => false
      })
      curState = curState.pruneUnreachableHeap()

      // Init the fields of singletons (the environment)
      for (sem <- SystemParameters.compiler.asInstanceOf[TouchCompiler].getNativeMethodsSemantics()) {
        if(sem.isInstanceOf[AAny]) {
          val typ = sem.asInstanceOf[AAny].getTyp
          if(typ.isSingleton &&
            (!TouchAnalysisParameters.libraryFieldPruning ||
              SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.name))) {
            if(typ.name != "records" && typ.name != "art" && typ.name != "data" && typ.name != "code") {
              val singletonProgramPoint = TouchSingletonProgramPoint(typ.name)
              curState = RichNativeSemantics.Top[S](typ)(curState,singletonProgramPoint)
              val obj = curState.getExpression
              val variable = ExpressionSet(VariableIdentifier(typ.name.toLowerCase, typ, singletonProgramPoint))
              curState = RichNativeSemantics.Assign[S](variable,obj)(curState,singletonProgramPoint)
            }
          }
        }
      }

      curState

    } else s

  }

  /**
   * Computes the least fix point for states
   */
  private def lfp[S <: State[S]](initialState:S,singleIteration:(S => S)):S = {

    var iteration = 1
    var prev = initialState
    var cur = prev.lub(singleIteration(prev))
    while(!cur.lessEqual(prev)) {
      val a1 = prev
      val a2 = cur
      prev = a2
      iteration=iteration+1
      if(iteration > SystemParameters.wideningLimit) {
        if (iteration > SystemParameters.wideningLimit + 10)
          println("Looks like we are not terminating here!")
        cur = prev.widening(singleIteration(prev))
      }
      else cur = prev.lub(singleIteration(prev))
    }

    cur

  }

}

/**
 * We collect alarms found _during_ the analysis using the static class "Reporter". To fit in with the old
 * system of checking properties _after_ the analysis of the script, we use this visitor to collect all alarms
 * found during the analysis
 */
class ImprecisionVisitor extends Visitor {

  def getLabel() = "Imprecision warnings during analysis"

  /**
   * Check the property over a single state
   *
   * @param state the abstract state
   * @param statement the statement that was executed after the given abstract state
   * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
   */
  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) {
    val errors = Reporter.getImprecision(statement.getPC())
    if (!errors.isEmpty) {
      for (mess <- Reporter.getImprecision(statement.getPC())) {
        printer.add(WarningProgramPoint(statement.getPC(),mess))
      }
    }
//    else {
//      printer.add(ValidatedProgramPoint(statement.getPC(),"valid"))
//    }
  }

}

/**
 * We collect alarms found _during_ the analysis using the static class "Reporter". To fit in with the old
 * system of checking properties _after_ the analysis of the script, we use this visitor to collect all alarms
 * found during the analysis
 */
class AlarmVisitor extends Visitor {

  def getLabel() = "Alarms during analysis"

  /**
   * Check the property over a single state
   *
   * @param state the abstract state
   * @param statement the statement that was executed after the given abstract state
   * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
   */
  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) {
    val errors = Reporter.getErrors(statement.getPC())
    if (!errors.isEmpty) {
      for (mess <- Reporter.getErrors(statement.getPC())) {
        printer.add(WarningProgramPoint(statement.getPC(),mess))
      }
    }
//    else {
//      printer.add(ValidatedProgramPoint(statement.getPC(),"valid"))
//    }
  }

}

/**
 * Check if something is bottom. Might be due to unreachable code.
 */
class BottomVisitor extends Visitor {

  var childrenNotToReport : Set[Statement] = Set.empty

  def getLabel() = "BottomChecker"

  /**
   * Check the property over a single state
   *
   * @param state the abstract state
   * @param statement the statement that was executed after the given abstract state
   * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
   */
  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) {
    if (!childrenNotToReport.contains(statement) && state.lessEqual(state.bottom())) {
      // if all children of the statement are bottom, do not report any of them
      def transitive(x:Statement):Set[Statement] = x.getChildren.foldLeft(Set.empty[Statement])(_ ++ transitive(_)) + x
      childrenNotToReport = childrenNotToReport ++ transitive(statement)
      Reporter.reportBottom("State is bottom",statement.getPC())
      printer.add(WarningProgramPoint(statement.getPC(),"State is bottom"))
    }
  }

}

/**
 * Two properties at the same time
 */
class ComposedProperty(name:String,a:Property,b:Property) extends Property {
  def getLabel() = name
  def check[S <: State[S]](classT : Type, methodName : MethodDeclaration, result : CFGState[S], printer : OutputCollector) {
    a.check(classT,methodName,result,printer)
    b.check(classT,methodName,result,printer)
  }


  /**
   * Check the property over the abstract results of multiple method
   *
   * @param results a list of the results, consisting of class type, method declaration and cfg
   * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
   */
  override def check[S <: State[S]](results : List[(Type,MethodDeclaration, CFGState[S])], printer : OutputCollector):Unit = {
    a.check(results,printer)
    b.check(results,printer)
  }

  def finalizeChecking(printer : OutputCollector) {
    a.finalizeChecking(printer)
    b.finalizeChecking(printer)
  }
}


/**
 * Check the empty property
 */
class NoProperty extends Property {
  def getLabel() = ""
  def check[S <: State[S]](classT : Type, methodName : MethodDeclaration, result : CFGState[S], printer : OutputCollector) {}
  def finalizeChecking(printer : OutputCollector) {}
}

