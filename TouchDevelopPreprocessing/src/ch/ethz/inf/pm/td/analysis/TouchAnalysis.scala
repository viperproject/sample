/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain._
import ch.ethz.inf.pm.sample.execution.CFGState
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.sample.util.{Relation, AccumulatingTimer}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.domain._
import ch.ethz.inf.pm.td.output.Exporters
import ch.ethz.inf.pm.td.semantics.{SRecords, AAny}
import com.typesafe.scalalogging.{LazyLogging, StrictLogging}

import scala.collection.mutable

/**
 *
 * Analysis for TouchDevelop scripts
 *
 * @author Lucas Brutschy
 */
class TouchAnalysis[D <: NumericalDomain[D], R <: StringDomain[R]]
  extends SemanticAnalysis[StringsAnd[InvalidAnd[D], R]] with StrictLogging {

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

  def getInitialState: StringsAnd[InvalidAnd[D], R] = {
    val numericSubDomain:D = domain match {
      case "Sign" => new BoxedNonRelationalNumericalDomain(Sign.Top.asInstanceOf[Sign]).asInstanceOf[D]
      case "Interval" => new BoxedNonRelationalNumericalDomain(IntegerInterval.Top.asInstanceOf[IntegerInterval]).asInstanceOf[D]
      case "ApronInterval" => Apron.Box.Bottom.asInstanceOf[D]
      case "ApronOctagons" => Apron.Octagons.Bottom.asInstanceOf[D]
      case "ApronPolka"    => Apron.Polyhedra.Bottom.asInstanceOf[D]
      case "ApronPolkaStrict" => Apron.StrictPolyhedra.Bottom.asInstanceOf[D]
      case "ApronLinearEqualities" => Apron.LinearEqualities.Bottom.asInstanceOf[D]
    }

    val invalidAndSubDomain = new InvalidAnd(numericSubDomain)

    stringDomain match {
      case "Bricks" => new StringsAnd[InvalidAnd[D], R](invalidAndSubDomain, new Bricks().asInstanceOf[R])
      case _ => new StringsAnd[InvalidAnd[D], R](invalidAndSubDomain,
        new NonrelationalStringDomain(StringKSetDomain.Top(TouchAnalysisParameters.get.stringRepresentationBound).asInstanceOf[StringKSetDomain]).asInstanceOf[R])
    }


  }

  override def reset() = ()

  override def getProperties: List[Property] = {

    val bottom = new SingleStatementProperty(new BottomVisitor)
    val alarm = new SingleStatementProperty(new AlarmVisitor)
    val imprecision = new SingleStatementProperty(new ImprecisionVisitor)
    val empty = new NoProperty

    val allChecks = new ComposedProperty("All checks", bottom, new ComposedProperty("", alarm, imprecision))

    List(allChecks, alarm, imprecision, bottom, empty)
  }

  def getNativeMethodsSemantics: List[NativeMethodSemantics] = Nil

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
  override def analyze[S <: State[S]](methods: List[String], entryState: S, output: OutputCollector = new OutputCollector)
      : List[(Type, MethodDeclaration, CFGState[S])] = {
    val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]

    // Set up the environment
    logger.info(" Analyzing " + compiler.main.name)
    if(SystemParameters.TIME) AccumulatingTimer.reset()

    // Shall we compute a fixpoint over the whole script (only in case we have persistent data)
    val outMostFixpoint =
      !TouchAnalysisParameters.get.generalPersistentState &&
        !compiler.isInLibraryMode &&
        (
          SRecords.mutableFields.exists { x =>
            !x._2.contains(TransientModifier) &&
              !x._2.contains(ReadOnlyModifier) &&
              !x._2.contains(ResourceModifier)
          } ||
            compiler.globalData.exists{ x =>
              !x.modifiers.contains(TransientModifier)
            }
          )

    // Set up things like records
    for (sem <- TypeList.getSingletons) sem.setUp(compiler,outMostFixpoint)

    // PRE-ANALYSIS: Discover required fragment
    //
    // We discover all fields from the API that are used in this set of classes. We will not instantiate anything else
    // For more information, see Challenge 5 in OOPSLA 2014, Brutschy/Ferrara/Müller
    //
    if (TouchAnalysisParameters.get.libraryFieldPruning) {
      SystemParameters.resetOutput()
      MethodSummaries.reset[TouchState.PreAnalysis]()
      Reporter.disableAllOutputs()
      if(SystemParameters.TIME) AccumulatingTimer.start("TouchAnalysis.LibraryFieldAnalysis")
      RequiredLibraryFragmentAnalysis(compiler.parsedScripts,output)
      if(SystemParameters.TIME) AccumulatingTimer.stopAndWrite("TouchAnalysis.LibraryFieldAnalysis")
      logger.debug("Relevant Fields: "+compiler.relevantLibraryFields.mkString(","))
    }

    // PRE-ANALYSIS: Run only the heap analysis
    //
    // We collect
    // (1) For each function which variables are accessed (for access-based localization, Oh/Brutschy/Yi, VMCAI 2011)
    // (2) For each numerical variable, with which values it is compared (for variable packing, as in Astree)
    //
    val newEntryState = if (TouchAnalysisParameters.get.variablePacking) {
      Localization.reset()
      TouchVariablePacking.reset()
      SystemParameters.resetOutput()
      MethodSummaries.reset[TouchState.PreAnalysis]()
      Reporter.disableAllOutputs()
      val oldNumber = TouchAnalysisParameters.get.numberOfVersions
      TouchAnalysisParameters.set(TouchAnalysisParameters.get.copy(numberOfVersions = 1))
      if(SystemParameters.TIME) AccumulatingTimer.start("TouchAnalysis.HeapPreanalysis")
      analyzeScript[TouchState.PreAnalysis](compiler,methods,outMostFixpoint)(TouchState.PreAnalysis())
      //if (SystemParameters.TIME) println(AccumulatingTimer)
      if(SystemParameters.TIME) AccumulatingTimer.stopAndWrite("TouchAnalysis.HeapPreanalysis")
      TouchAnalysisParameters.set(TouchAnalysisParameters.get.copy(numberOfVersions = oldNumber))
      logger.debug("Variable packing: "+TouchVariablePacking)
      val classifier = TouchVariablePacking.makeClassifier
      if (TouchAnalysisParameters.get.accessBasedLocalization) {
        logger.debug("Localization "+Localization)
        Localization.startPruning(variablePacker = Some(classifier))
      }
      TouchEntryStateBuilder(TouchAnalysisParameters.get).topStateWithClassifier(classifier).asInstanceOf[S]
    } else entryState

    //
    // MAIN ANALYSIS
    //
    if(SystemParameters.TIME) AccumulatingTimer.start("TouchAnalysis.MainAnalysis")
    SystemParameters.resetOutput()
    MethodSummaries.reset[S]()
    analyzeScript(compiler,methods,outMostFixpoint)(newEntryState)
    if(SystemParameters.TIME) AccumulatingTimer.stopAndWrite("TouchAnalysis.MainAnalysis")

    //
    // CHECK PROPERTIES
    //
    // Most importantly, we are reporting on bottom values in the CFG here, for debugging and reporting of
    // unreachable code.
    //
    val summaries = MethodSummaries.getSummaries[S]
    val mustCheck = (s: MethodSummary[S]) => s.method.classDef == compiler.main || TouchAnalysisParameters.get.libraryErrorReportingMode == LibraryErrorReportingMode.Report
    val results = for (s@MethodSummary(_, mDecl, cfgState) <- summaries.values.toList if mustCheck(s)) yield (mDecl.classDef.typ, mDecl, cfgState)
    if (TouchAnalysisParameters.get.reportUnanalyzedFunctions) {
      val unAnalyzed = compiler.allMethods.toSet -- summaries.values.map(_.method)
      for (un <- unAnalyzed) {
        logger.debug(" Did not analyze "+un.name+" (may be unreachable)")
      }
    }
    if (SystemParameters.property != null) {
      SystemParameters.property.check(results, output)
      SystemParameters.property.finalizeChecking(output)
    }

    Exporters(compiler)

    // Reset singletons
    for (sem <- TypeList.getSingletons) sem.reset()

    results
  }

  /**
   * Initializes global data, then calls analyzeExecution to analyze one or many executions of the script
   */
  private def analyzeScript[S <: State[S]](compiler: TouchCompiler, methods: List[String], outMostFixpoint:Boolean)(entryState: S): S = {

    // Initialize the fields of singletons (the environment)
    var curState = entryState
    for (
      typ <- TypeList.getSingletons
      if !TouchAnalysisParameters.get.libraryFieldPruning || compiler.relevantLibraryFields.contains(typ.name)
    ) {
      curState = typ.initialize(curState)
    }

    // == CORE ANALYSIS IS STARTING HERE
    // The first fixpoint, which is computed over several executions of the same script
    val result =
      if (outMostFixpoint)
        Lattice.lfp(curState, analyzeExecution(compiler, methods)(_: S), SystemParameters.wideningLimit)
      else
        analyzeExecution(compiler, methods)(curState)

    result
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

    if (TouchAnalysisParameters.get.treatPrivateMethodLikePublicMethods)
      methodsToBeAnalyzed = methodsToBeAnalyzed ++ compiler.getPrivateMethods
    if (methods.nonEmpty) methodsToBeAnalyzed = methodsToBeAnalyzed.filter {
      tm =>
        val methodId = tm.name
        methods.contains(methodId.toString)
    }

    // Execute abstract semantics of each public method (or the ones selected in the GUI)
    val exitStates = for (mDecl <- methodsToBeAnalyzed) yield {
      Some(analyzeMethod(mDecl, initialState))
    }

    // Compute the least upper bound of all public method exit states
    val exitState = exitStates.flatten.foldLeft(initialState.bottom())({
      (stateLeft: S, stateRight: S) =>
        stateLeft.lub(stateRight)
    })

    // Compute the fixpoint over all events
    var result = if (!TouchAnalysisParameters.get.singleEventOccurrence) {
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
    for (methodDeclaration <- compiler.events) {
      cur = cur.lub(analyzeMethod(methodDeclaration, s))
    }

    resetEnv(cur)
  }


  private def analyzeMethod[S <: State[S]](callTarget: MethodDeclaration, entryState: S): S = {

    val exitState = MethodSummaries.collect[S](callTarget.programpoint, callTarget, entryState, Nil)

    resetEnv(exitState)

  }

  private def resetEnv[S <: State[S]](s: S): S = {

    if (TouchAnalysisParameters.get.resetEnv) {

      var curState = s

      // Remove Env
      curState = curState.pruneVariables({
        id: VariableIdentifier =>
          id.typ.asInstanceOf[TouchType].isSingleton &&
            id.typ.name != "art" &&
            id.typ.name != "data" &&
            id.typ.name != "code" &&
            id.typ.name != "records"
      })
      curState = curState.pruneUnreachableHeap()

      // Init the fields of singletons (the environment)
      for (sem <- TypeList.getSingletons) {
        sem match {
          case typ: AAny =>
            if (typ.isSingleton &&
              (!TouchAnalysisParameters.get.libraryFieldPruning ||
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
    if (!childrenNotToReport.contains(statement) && state.isBottom) {
      // if all children of the statement are bottom, do not report any of them
      def transitive(x: Statement): Set[Statement] = x.getChildren.foldLeft(Set.empty[Statement])(_ ++ transitive(_)) + x
      childrenNotToReport = childrenNotToReport ++ transitive(statement)
      Reporter.reportBottom("Unreachable code at statement "+statement.toString, statement.getPC())
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

/** Defines how variables should be packed */
object TouchVariablePacking extends LazyLogging {

  private val variablesAssignedInLoop:scala.collection.mutable.HashMap[ProgramPoint,IdentifierSet] = mutable.HashMap.empty
  private var packed:Relation[Identifier] = Relation.empty[Identifier]

  def makeClassifier:Classifier = {
    var toPack = Map.empty[Identifier,VariablePack]
    var toAssign = packed.getAll
    while (toAssign.nonEmpty) {
      val closure = packed.closure(toAssign.head)
      toPack = closure.foldLeft(toPack)( (x,y) => x + (y -> Pack(IdentifierSet.Inner(closure))))
      toAssign = toAssign -- closure
    }
    Classifier(toPack)
  }

  def packLoopHeads(loopHeads: Set[ProgramPoint], loopHeadIds: IdentifierSet, capIds:IdentifierSet) = {
    val loopAssigns = Lattice.bigLub(loopHeads.map(variablesAssignedInLoop.getOrElse(_,IdentifierSet.Bottom))) glb capIds
    if (!loopAssigns.isBottom) {
      logger.debug("Packing " + loopAssigns + " at loop head " + loopHeads.mkString(","))
      TouchVariablePacking.pack(loopAssigns ++ loopHeadIds)
    }
  }

  def collectLoopAssign(ids: IdentifierSet, inLoops: Set[ProgramPoint]) = {
    for (x <- inLoops) {
      variablesAssignedInLoop += (x -> (variablesAssignedInLoop.getOrElse(x,IdentifierSet.Bottom) ++ ids))
    }
  }

  def pack(ids: IdentifierSet) = {
    ids match {
      case IdentifierSet.Bottom => ()
      case IdentifierSet.Top => () // TODO
      case IdentifierSet.Inner(idsx) =>
        val numerical = idsx.map(normalize).filter(_.typ.isNumericalType)
        if (numerical.size > 1)
          packed = packed.add(numerical, numerical)
    }
  }

  def reset() = {
    packed = Relation.empty[Identifier]
    variablesAssignedInLoop.clear()
  }

  override def toString:String = {
    var toDump = packed.getAll
    var str = Set.empty[String]
    while (toDump.nonEmpty) {
      val closure = packed.closure(toDump.head)
      str += "{"+closure.mkString(",")+"}"
      toDump = toDump -- closure
    }
    str.mkString(",")
  }

  def normalize(id:Identifier):Identifier = {
    id match {
      case HeapIdentifier(pp,typ,_,_) => HeapIdentifier(pp,typ,summary = false,0)
      case FieldIdentifier(HeapIdentifier(pp1,typ1,_,_),f,typ) => FieldIdentifier(HeapIdentifier(pp1,typ1,summary = false,0),f,typ)
      case x:Identifier => x
    }
  }

  case class Classifier(toPack:Map[Identifier,VariablePack]) extends VariablePackingClassifier {

    override def classify(id: Set[Identifier]): Set[VariablePack] =
      id.flatMap(x => toPack.get(TouchVariablePacking.normalize(x)))

  }

  case class Pack(ids:IdentifierSet) extends VariablePack

}
