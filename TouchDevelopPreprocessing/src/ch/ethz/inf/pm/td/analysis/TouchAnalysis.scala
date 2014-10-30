package ch.ethz.inf.pm.td.analysis

import apron.{PolkaEq, Polka, OptOctagon, Box}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain._
import ch.ethz.inf.pm.sample.execution.CFGState
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.domain._
import ch.ethz.inf.pm.td.output.Exporters
import RichNativeSemantics._
import ch.ethz.inf.pm.td.semantics.AAny

/**
 *
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 5:50 PM
 *
 */
class TouchAnalysis[D <: NumericalDomain[D], R <: StringDomain[R]]
  extends SemanticAnalysis[StringsAnd[InvalidAnd[D], R]] {

  val STRING_DOMAIN = "StringDomain"
  val NUMERICAL_DOMAIN = "Domain"

  var domain: String = ""

  var stringDomain: String = ""

  def getLabel(): String = "TouchDevelop analysis"

  protected def numericalDomainList = (NUMERICAL_DOMAIN, List("Sign", "Interval","ApronInterval", "ApronOctagons", "ApronPolka", "ApronLinearEqualities"))

  protected def stringDomainList = (STRING_DOMAIN, List("KSet", "Bricks"))

  def parameters(): List[(String, Any)] = List(numericalDomainList, stringDomainList)

  def setParameter(label: String, value: Any) {
    label match {
      case NUMERICAL_DOMAIN => domain = value.toString
      case STRING_DOMAIN => stringDomain = value.toString
    }
  }

  def getInitialState(): StringsAnd[InvalidAnd[D], R] = {
    val numericSubDomain = domain match {
      case "Sign" => new BoxedNonRelationalNumericalDomain(new Sign(SignValues.T)).asInstanceOf[D]
      case "Interval" => new BoxedNonRelationalNumericalDomain(new Interval(0, 0)).asInstanceOf[D]
      case "ApronInterval" =>
        val man = new Box()
        ApronInterface.Default(None, man, env = Set.empty).factory().asInstanceOf[D]
      case "ApronOctagons" =>
        val man = new OptOctagon()
        ApronInterface.Default(None, man, env = Set.empty).factory().asInstanceOf[D]
      case "ApronPolka" =>
        val man = new Polka(false)
        ApronInterface.Default(None, man, env = Set.empty).factory().asInstanceOf[D]
      case "ApronPolkaStrict" =>
        val man = new Polka(true)
        ApronInterface.Default(None, man, env = Set.empty).factory().asInstanceOf[D]
      case "ApronLinearEqualities" =>
        val man = new PolkaEq()
        ApronInterface.Default(None, man, env = Set.empty).factory().asInstanceOf[D]
    }

    val invalidAndSubDomain = new InvalidAnd(numericSubDomain)

    stringDomain match {
      case "Bricks" => new StringsAnd[InvalidAnd[D], R](invalidAndSubDomain, new Bricks().asInstanceOf[R])
      case _ => new StringsAnd[InvalidAnd[D], R](invalidAndSubDomain,
        new NonrelationalStringDomain(new StringKSetDomain(TouchAnalysisParameters.stringRepresentationBound)).asInstanceOf[R])
    }


  }

  override def reset() {
    Unit
  }

  override def getProperties: List[Property] = {

    val bottom = new SingleStatementProperty(new BottomVisitor)
    val alarm = new SingleStatementProperty(new AlarmVisitor)
    val imprecision = new SingleStatementProperty(new ImprecisionVisitor)
    val empty = new NoProperty

    val allChecks = new ComposedProperty("All checks", bottom, new ComposedProperty("", alarm, imprecision))

    List(allChecks, alarm, imprecision, bottom, empty)
  }

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil


  def analyze[S <: State[S]](entryState: S): List[(Type, MethodDeclaration, CFGState[S])] = {
    analyze(Nil, entryState, new OutputCollector)
  }

  /**
   *
   * The execution model is to
   *
   * (1) Initialize the global state to invalid
   * (2) Repeat:
   * (2.1) Reset the local state
   * (2.2) Run the method (interprocedurally)
   * (2.3) Compute lfp (lambda x -> lub_e\in E(e(x))) where E is the set of events
   *
   */
  override def analyze[S <: State[S]](methods: List[String], entryState: S, output: OutputCollector)
  : List[(Type, MethodDeclaration, CFGState[S])] = {
    val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]

    // Set up the environment
    SystemParameters.resetOutput()
    MethodSummaries.reset[S]()
    SystemParameters.progressOutput.begin(" ANALYZING " + compiler.main.name)

    // We discover all fields from the API that are used in this set of classes. We will not instantiate anything else
    //SystemParameters.progressOutput.begin("Library fragment analysis")
    if (TouchAnalysisParameters.libraryFieldPruning) {
      compiler.relevantLibraryFields = RequiredLibraryFragmentAnalysis(compiler.parsedScripts)
      compiler.relevantLibraryFields = compiler.relevantLibraryFields ++ Set("data", "art", "records", "code")
      SystemParameters.resetOutput()
      MethodSummaries.reset[S]()
    }
    //SystemParameters.progressOutput.end()


    // Initialize the fields of singletons (the environment)
    var curState = entryState
    for (sem <- compiler.getSingletons) {
      sem match {
        case any: AAny =>
          val typ = any
          if (typ.isSingleton &&
            (!TouchAnalysisParameters.libraryFieldPruning ||
              compiler.relevantLibraryFields.contains(typ.name))) {
            val singletonProgramPoint = TouchSingletonProgramPoint(typ.name)
            if (typ.name == "records")
              if (!TouchAnalysisParameters.generalPersistentState && !compiler.isInLibraryMode) {
                curState = RichNativeSemantics.New[S](typ)(curState, singletonProgramPoint)
              } else {
                curState = RichNativeSemantics.Top[S](typ)(curState, singletonProgramPoint)
              }
            else
              curState = RichNativeSemantics.Top[S](typ)(curState, singletonProgramPoint)
            val obj = curState.expr
            val variable = ExpressionSet(VariableIdentifier(typ.name.toLowerCase)(typ, singletonProgramPoint))
            curState = RichNativeSemantics.Assign[S](variable, obj)(curState, singletonProgramPoint)
          }
        case _ =>
      }
    }

    // Set global state to invalid
    for (v <- compiler.globalData) {

      val variable = VariableIdentifier(CFGGenerator.globalReferenceIdent(v.variable.getName))(v.typ, v.programpoint)
      val leftExpr = ExpressionSet(variable)
      curState = curState.createVariable(leftExpr, v.typ, v.programpoint)

      val rightVal =
        if (!TouchAnalysisParameters.generalPersistentState && !compiler.isInLibraryMode) {

          // We analyze executions separately. In the first execution of the script, global fields are invalid
          // except for the obvious exception (art, read-only, primitives)
          // There are three types of global data:
          //  (1) Regular global variables / objects, which are initialized to invalid
          //  (2) Global objects that are read-only and are initialized to some default object (Tile)
          //  (3) Global objects that represents read-only artwork that is initialized from some URL.
          if (v.modifiers.contains(ResourceModifier)) {
            curState = RichNativeSemantics.Top[S](v.typ.asInstanceOf[TouchType])(curState, v.programpoint)
            curState.expr
          } else if (v.modifiers.contains(ReadOnlyModifier)) {
            curState = RichNativeSemantics.New[S](v.typ.asInstanceOf[TouchType])(curState, v.programpoint)
            curState.expr
          } else {
            toExpressionSet(toRichExpression(v.typ.name match {
              case "String" => Constant("", v.typ, v.programpoint)
              case "Number" => Constant("0", v.typ, v.programpoint)
              case "Boolean" => Constant("false", v.typ, v.programpoint)
              case _ => InvalidExpression(v.typ.asInstanceOf[TouchType], "global data may be uninitialized", v.programpoint)
            }))
          }

        } else {

          // We analyze one execution in top state.
          if (v.modifiers.contains(ResourceModifier)) {
            curState = RichNativeSemantics.Top[S](v.typ.asInstanceOf[TouchType])(curState, v.programpoint)
            curState.expr
          } else if (v.modifiers.contains(ReadOnlyModifier)) {
            curState = RichNativeSemantics.New[S](v.typ.asInstanceOf[TouchType])(curState, v.programpoint)
            curState.expr
          } else {
            curState = RichNativeSemantics.TopWithInvalid[S](v.typ.asInstanceOf[TouchType], "global variable may be invalid")(curState,
              if (TouchAnalysisParameters.fullAliasingInGenericInput) DummyProgramPoint else v.programpoint)
            curState.expr
          }

        }

      curState = curState.createVariable(leftExpr, leftExpr.getType(), v.programpoint)
      curState = curState.assignVariable(leftExpr, rightVal)
      curState
    }

    // The first fixpoint, which is computed over several executions of the same script
    if (!TouchAnalysisParameters.generalPersistentState && !TouchAnalysisParameters.singleExecution && !compiler.isInLibraryMode)
      Lattice.lfp(curState, analyzeExecution(compiler, methods)(_: S), SystemParameters.wideningLimit)
    else
      analyzeExecution(compiler, methods)(curState)

    val summaries = MethodSummaries.getSummaries[S]
    // Check properties on the results
    val mustCheck = (s: MethodSummary[S]) => s.method.classDef == compiler.main || !TouchAnalysisParameters.reportOnlyAlarmsInMainScript
    val results = for (s@MethodSummary(_, mdecl, cfgState) <- summaries.values.toList if mustCheck(s))
    yield (mdecl.classDef.typ, mdecl, cfgState)

    if (SystemParameters.property != null) {
      SystemParameters.propertyTimer.start()
      SystemParameters.property.check(results, output)
      SystemParameters.property.finalizeChecking(output)
      SystemParameters.propertyTimer.stop()
    }

    Exporters(compiler)

    SystemParameters.progressOutput.end()
    results
  }

  private def analyzeExecution[S <: State[S]](compiler: TouchCompiler, methods: List[String])(initialState: S): S = {

    var methodsToBeAnalyzed = compiler.getPublicMethods

    // filter out methods that contain anything other than number and string as arguments
    if (!compiler.isInLibraryMode) {
      methodsToBeAnalyzed = methodsToBeAnalyzed filter {
        tm =>
          !tm.arguments.head.exists {
            x: VariableDeclaration => x.typ.isObject
          }
      }
    }

    if (TouchAnalysisParameters.treatPrivateMethodLikePublicMethods)
      methodsToBeAnalyzed = methodsToBeAnalyzed ++ compiler.getPrivateMethods
    if (methods.nonEmpty) methodsToBeAnalyzed = methodsToBeAnalyzed.filter {
      tm =>
        val methodId = tm.name
        methods.contains(methodId.toString)
    }

    // Execute abstract semantics of each public method (or the ones selected in the GUI)
    val exitStates = for (mdecl <- methodsToBeAnalyzed) yield {
      Some(analyzeMethod(mdecl, initialState))
    }

    // Compute the least upper bound of all public method exit states
    val exitState = exitStates.flatten.foldLeft(initialState.bottom())({
      (stateLeft: S, stateRight: S) =>
        stateLeft.lub(stateRight)
    })

    // Compute the fixpoint over all events
    var result = if (!TouchAnalysisParameters.singleEventOccurrence) {
      Lattice.lfp(exitState, analyzeEvents(compiler, methods)(_: S), SystemParameters.wideningLimit)
    } else {
      analyzeEvents(compiler, methods)(exitState)
    }

    // Join the the normal exit state with all abnormal exit states
    result = MethodSummaries.joinAbnormalExits(result)

    result.removeExpression()
  }

  private def analyzeEvents[S <: State[S]](compiler: TouchCompiler, methods: List[String])(s: S): S = {

    var cur = s
    for (mdecl <- compiler.events) {
      cur = cur.lub(analyzeMethod(mdecl, s, localHandlerScope = MethodSummaries.getClosureEntry[S](mdecl.name.toString)))
    }

    resetEnv(cur)
  }


  private def analyzeMethod[S <: State[S]](callTarget: MethodDeclaration, entryState: S, localHandlerScope: Option[S] = None): S = {

    val exitState = MethodSummaries.collect[S](callTarget.programpoint, callTarget, entryState, Nil, localHandlerScope = localHandlerScope)

    resetEnv(exitState)

  }

  private def resetEnv[S <: State[S]](s: S): S = {

    if (TouchAnalysisParameters.resetEnv) {

      var curState = s

      // Remove Env
      curState = curState.pruneVariables({
        case id: VariableIdentifier =>
          id.typ.asInstanceOf[TouchType].isSingleton &&
            id.typ.name != "art" &&
            id.typ.name != "data" &&
            id.typ.name != "code" &&
            id.typ.name != "records"
        case _ => false
      })
      curState = curState.pruneUnreachableHeap()

      // Init the fields of singletons (the environment)
      for (sem <- SystemParameters.compiler.asInstanceOf[TouchCompiler].getNativeMethodsSemantics()) {
        sem match {
          case typ: AAny =>
            if (typ.isSingleton &&
              (!TouchAnalysisParameters.libraryFieldPruning ||
                SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.name))) {
              if (typ.name != "records" && typ.name != "art" && typ.name != "data" && typ.name != "code") {
                val singletonProgramPoint = TouchSingletonProgramPoint(typ.name)
                curState = RichNativeSemantics.Top[S](typ)(curState, singletonProgramPoint)
                val obj = curState.expr
                val variable = ExpressionSet(VariableIdentifier(typ.name.toLowerCase)(typ, singletonProgramPoint))
                curState = RichNativeSemantics.Assign[S](variable, obj)(curState, singletonProgramPoint)
              }
            }
          case _ =>
        }
      }

      curState

    } else s

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
  def checkSingleStatement[S <: State[S]](state: S, statement: Statement, printer: OutputCollector) {
    val errors = Reporter.getImprecision(statement.getPC())
    if (errors.nonEmpty) {
      for (mess <- Reporter.getImprecision(statement.getPC())) {
        printer.add(WarningProgramPoint(statement.getPC(), mess))
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
  def checkSingleStatement[S <: State[S]](state: S, statement: Statement, printer: OutputCollector) {
    val errors = Reporter.getErrors(statement.getPC())
    if (errors.nonEmpty) {
      for (mess <- Reporter.getErrors(statement.getPC())) {
        printer.add(WarningProgramPoint(statement.getPC(), mess))
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

  var childrenNotToReport: Set[Statement] = Set.empty

  def getLabel() = "BottomChecker"

  /**
   * Check the property over a single state
   *
   * @param state the abstract state
   * @param statement the statement that was executed after the given abstract state
   * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
   */
  def checkSingleStatement[S <: State[S]](state: S, statement: Statement, printer: OutputCollector) {
    if (!childrenNotToReport.contains(statement) && state.lessEqual(state.bottom())) {
      // if all children of the statement are bottom, do not report any of them
      def transitive(x: Statement): Set[Statement] = x.getChildren.foldLeft(Set.empty[Statement])(_ ++ transitive(_)) + x
      childrenNotToReport = childrenNotToReport ++ transitive(statement)
      Reporter.reportBottom("Unreachable code", statement.getPC())
      printer.add(WarningProgramPoint(statement.getPC(), "Unreachable code"))
    }
  }

}

/**
 * Two properties at the same time
 */
class ComposedProperty(name: String, a: Property, b: Property) extends Property {
  def getLabel() = name

  def check[S <: State[S]](classT: Type, methodName: MethodDeclaration, result: CFGState[S], printer: OutputCollector) {
    a.check(classT, methodName, result, printer)
    b.check(classT, methodName, result, printer)
  }


  /**
   * Check the property over the abstract results of multiple method
   *
   * @param results a list of the results, consisting of class type, method declaration and cfg
   * @param printer the output collector that has to be used to signal warning, validate properties, or inferred contracts
   */
  override def check[S <: State[S]](results: List[(Type, MethodDeclaration, CFGState[S])], printer: OutputCollector): Unit = {
    a.check(results, printer)
    b.check(results, printer)
  }

  def finalizeChecking(printer: OutputCollector) {
    a.finalizeChecking(printer)
    b.finalizeChecking(printer)
  }
}


/**
 * Check the empty property
 */
class NoProperty extends Property {
  def getLabel() = ""

  def check[S <: State[S]](classT: Type, methodName: MethodDeclaration, result: CFGState[S], printer: OutputCollector) {}

  def finalizeChecking(printer: OutputCollector) {}
}

