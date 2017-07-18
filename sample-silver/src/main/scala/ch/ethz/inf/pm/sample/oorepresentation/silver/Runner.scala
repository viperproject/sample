/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import java.io.{File, PrintWriter}

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.InterproceduralSilverInterpreter.CallGraphMap
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import org.jgrapht.DirectedGraph
import org.jgrapht.alg.StrongConnectivityInspector
import org.jgrapht.graph.DefaultDirectedGraph
import org.jgrapht.traverse.TopologicalOrderIterator
import viper.carbon.CarbonVerifier
import viper.silver.ast.utility.Functions
import viper.silver.ast.utility.Functions.Factory
import viper.silver.{ast => sil}

import scala.collection.JavaConverters._

//import viper.silicon.Silicon

trait AbstractAnalysisRunner[S <: State[S]] {
  val compiler: SilverCompiler

  val analysis: SilverAnalysis[S]

  def program: SilverProgramDeclaration = compiler.program

  /**
    * Returns the sequence of functions to analyze. By default these are all
    * functions.
    *
    * @return The sequence of functions to analyze.
    */
  def functionsToAnalyze: Seq[SilverFunctionDeclaration] = compiler.allFunctions

  /**
    * Returns the sequence of methods to analyze. By default these are all
    * methods.
    *
    * @return The sequence of methods to analyze.
    */
  def methodsToAnalyze: Seq[SilverMethodDeclaration] = compiler.allMethods

  def run(compilable: Compilable): ProgramResult[S] = {
    compiler.compile(compilable)
    _run()
  }

  protected def prepareContext(): Unit = {
    SystemParameters.analysisOutput = new StringCollector
    SystemParameters.progressOutput = new StringCollector
    SystemParameters.resetOutput()

    SystemParameters.wideningLimit = 3
    //SystemParameters.compiler = compiler

    SystemParameters.resetNativeMethodsSemantics()
    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodSemantics)
  }

  protected def _run(): ProgramResult[S] = {
    prepareContext()
    analysis.analyze(program)
  }

  def main(args: Array[String]): Unit = {
    run(Compilable.Path(new File(args(0)).toPath))
  }
}

/** Analysis runner for Silver programs. */
trait SilverAnalysisRunner[S <: State[S]]
  extends AbstractAnalysisRunner[S] {
  val compiler = new SilverCompiler()

  /** Runs the analysis on a given Silver program. */
  def run(program: sil.Program): ProgramResult[S] = {
    compiler.compileProgram(program)
    _run()
  }

  /** Runs the analysis on the Silver program whose name is passed as first argument and reports errors and warnings. */
  override def main(args: Array[String]): Unit = {
    val result = run(Compilable.Path(new File(args(0)).toPath)) // run the analysis

    println("\n******************\n* AnalysisResult *\n******************\n")
    if (Reporter.assertionViolations.isEmpty) println("No errors")
    for (e <- Reporter.assertionViolations) {
      println(e)
    } // error report
    println()
    if (Reporter.genericWarnings.isEmpty) println("No warnings")
    for (w <- Reporter.genericWarnings) {
      println(w)
    } // warning report
    println("\n***************\n* Entry States *\n***************\n")
    result.identifiers.foreach(ident => println(ident.name + "() -> " + result.getResult(ident).entryState()))
    println("\n***************\n* Exit States *\n***************\n")
    result.identifiers.foreach(ident => println(ident.name + "() -> " + result.getResult(ident).exitState()))
  }
}

/**
  * Interprocedural analysis runner for Silver programs.
  *
  * Methods are added to the worklist according to the callgraph (top down)
  *
  * @author Flurin Rindisbacher
  *
  **/
trait InterproceduralSilverAnalysisRunner[S <: State[S]]
  extends SilverAnalysisRunner[S] {

  override val analysis: InterproceduralSilverAnalysis[S]

  override protected def _run(): ProgramResult[S] = {
    prepareContext()
    val (condensedCallGraph, callsInProgram) = analyzeCallGraph(program)
    // search for "main methods". these are either methods that are not called from other methods,
    // or they are methods in a strongly connected component where the component itself is not called by other methods
    //
    // e.g program has methods foo, bar and baz. foo() calls bar(), bar() calls foo(), bar() calls baz
    // in this program foo and bar should be treated as main methods. baz is always called from another method
    // so it won't be added to the set of main methods.
    var mainMethods = Set[SilverIdentifier]()
    for (condensation <- new TopologicalOrderIterator(condensedCallGraph).asScala
         if condensedCallGraph.inDegreeOf(condensation) == 0) {
      for (method <- condensation.asScala)
        mainMethods += method.name
    }
    analysis.analyze(program, mainMethods, callsInProgram)
  }

  /**
    * Analyze the given program and return a tuple of condensed Callgraph and a map containing all calls to each method.
    * The condensed callgraph uses sets of method declarations as nodes. These nodes are the connected components inside a call graph.
    * E.g. foo() calls bar(), bar() calls foo(), bar() calls baz(). The condensed graph will be:
    * set(foo, bar) -> set(baz)
    *
    * @param program     The program to be analyzed
    * @param invertEdges Whether the condensed call should invert the edges or not (default: false)
    * @return Tuple of condensed call graph and map containing all method calls
    */
  protected def analyzeCallGraph(program: SilverProgramDeclaration, invertEdges: Boolean = false): (DirectedGraph[java.util.Set[SilverMethodDeclaration], Functions.Edge[java.util.Set[SilverMethodDeclaration]]], CallGraphMap) = {
    // Most code below was taken from ast.utility.Functions in silver repo!
    val callGraph = new DefaultDirectedGraph[SilverMethodDeclaration, Functions.Edge[SilverMethodDeclaration]](Factory[SilverMethodDeclaration]())
    var callsInProgram: CallGraphMap = Map().withDefault(_ => Set())

    for (f <- program.methods) {
      callGraph.addVertex(f)
    }

    def process(m: SilverMethodDeclaration, e: Statement) {
      e match {
        case MethodCall(_, method: Variable, _, _, _, _) =>
          if (invertEdges)
            callGraph.addEdge(program.methods.filter(_.name.name == method.getName).head, m)
          else
            callGraph.addEdge(m, program.methods.filter(_.name.name == method.getName).head)
          val pp = m.body.getBlockPosition(ProgramPointUtils.identifyingPP(e))
          val methodIdent = SilverIdentifier(method.getName)
          callsInProgram += (methodIdent -> (callsInProgram(methodIdent) + pp))
        case _ => e.getChildren foreach (process(m, _))
      }
    }

    for (m <- program.methods;
         block <- m.body.blocks;
         statement <- block.elements) {
      process(m, statement.merge)
    }

    val stronglyConnectedSets = new StrongConnectivityInspector(callGraph).stronglyConnectedSets().asScala
    val condensedCallGraph = new DefaultDirectedGraph(Factory[java.util.Set[SilverMethodDeclaration]]())

    /* Add each SCC as a vertex to the condensed call-graph */
    for (v <- stronglyConnectedSets) {
      condensedCallGraph.addVertex(v)
    }

    def condensationOf(m: SilverMethodDeclaration): java.util.Set[SilverMethodDeclaration] =
      stronglyConnectedSets.find(_ contains m).get

    /* Add edges from the call-graph (between individual functions) as edges
     * between their corresponding SCCs in the condensed call-graph, but only
     * if this does not result in a cycle.
     */
    for (e <- callGraph.edgeSet().asScala) {
      val sourceSet = condensationOf(e.source)
      val targetSet = condensationOf(e.target)

      if (sourceSet != targetSet)
        condensedCallGraph.addEdge(sourceSet, targetSet)
    }
    (condensedCallGraph, callsInProgram)
  }
}

/**
  * Interprocedural bottom up analysis runner for Silver programs.
  *
  * Methods at the  bottom of the callGraph (callee's) are analyzed first. Then their analysis result is reused for
  * the analysis of the callers
  *
  * @author Flurin Rindisbacher
  *
  **/
trait InterproceduralSilverBottomUpAnalysisRunner[S <: State[S]]
  extends InterproceduralSilverAnalysisRunner[S] {

  override val analysis: BottomUpAnalysis[S]

  override protected def _run(): ProgramResult[S] = {
    prepareContext()

    // Analyze the program and create an inverted condensed call-graph.
    // See the InterproceduralSilverAnalysisRunner for an explanation of the condensed call-graph.
    // Here we invert the edges to get the methods at the bottom of the call-graph.
    val (condensedCallGraph, callsInProgram) = analyzeCallGraph(program, invertEdges = true)

    var callees = Set[SilverIdentifier]()
    for (condensation <- new TopologicalOrderIterator(condensedCallGraph).asScala
         if condensedCallGraph.inDegreeOf(condensation) == 0) {
      for (method <- condensation.asScala)
        callees += method.name
    }
    // run the analysis starting at the bottom of the call-graph
    analysis.analyze(program, callees, callsInProgram)
  }

}

/** Specification Inference for Silver Programs.
  *
  * @author Caterina Urban
  * @tparam T The type of the specification.
  * @tparam S The type of the state.
  */
trait SilverInferenceRunner[T, S <: State[S] with SilverSpecification[T]]
  extends SilverAnalysisRunner[S] with SilverExtender[T, S] {

  /**
    * Extends a Silver program whose name is passed as first argument with
    * specifications inferred by the analysis.
    */
  def extend(args: Array[String]): sil.Program = {
    val results = run(Compilable.Path(new File(args(0)).toPath)) // run the analysis
    // extend the Silver program with inferred permission
    extendProgram(DefaultSilverConverter.prog, results)
  }

  /**
    * Exports a Silver program extended with inferred specifications.
    */
  def export(args: Array[String]): Unit = {
    val program = extend(args) // extend the program with permission inferred by the analysis
    println("\n********************\n* Extended Program *\n********************\n\n" + program)
    // create a file with the extended program
    val outName = args(0).split('.')(0) + "X.sil"
    val pw = new PrintWriter(new File(outName))
    pw.write(program.toString())
    pw.close()
  }

  /** Verifies a Silver program extended with inferred specifications using the Viper symbolic execution backend. */
  def verify(args: Array[String]): Unit = {
    val program = extend(args)
    // extend the program with permission inferred by the analysis
    // verified the extended program with Silicon/Carbon
    //val verifier = new Silicon()
    val verifier = CarbonVerifier()
    verifier.parseCommandLine(Seq("dummy.sil"))
    verifier.start()
    val result: viper.silver.verifier.VerificationResult = verifier.verify(program)
    println("\n***********************\n* Verification Result * " + result + "\n***********************")
  }

  override def main(args: Array[String]) {
    // run the analysis
    val extended = extend(args)

    // report errors and warnings
    println("\n******************\n* AnalysisResult *\n******************\n")
    if (Reporter.assertionViolations.isEmpty) println("No errors")
    for (e <- Reporter.assertionViolations) {
      println(e)
    } // error report
    println()
    if (Reporter.genericWarnings.isEmpty) println("No warnings")
    for (w <- Reporter.genericWarnings) {
      println(w)
    } // warning report

    // report extended program
    println("\n********************\n* Extended Program *\n********************\n\n" + extended)
    // create a file with the extended program
    //val copyName = args(0).split('.')(0) + ".sil.orig"
    //val cw = new PrintWriter(new File(copyName))
    //cw.write(DefaultSilverConverter.prog.toString); cw.close
    val outName = args(0).split('.')(0) + "X.sil"
    val ow = new PrintWriter(new File(outName))
    ow.write(extended.toString())
    ow.close()

    // verify the extended program with Silicon/Carbon
    // val verifier = new Silicon()
    var verifier = CarbonVerifier()
    verifier.parseCommandLine(Seq("dummy.sil"))
    verifier.start()
    val result: viper.silver.verifier.VerificationResult = verifier.verify(extended)
    println("\n***********************\n* Verification Result *\n***********************\n\n" + result)
  }

  override def toString = "Specification Inference"
}

/** Interprocedural Specification Inference for Silver Programs.
  *
  * @author Flurin Rindisbacher
  * @tparam T The type of the specification.
  * @tparam S The type of the state.
  */
trait InterproceduralSilverInferenceRunner[T, S <: State[S] with SilverSpecification[T]]
  extends SilverInferenceRunner[T, S] with InterproceduralSilverAnalysisRunner[S] {

  // TODO @flurin kinda hacky, keep the list of all CfgResults for the currently processed method
  var resultsToWorkWith: Seq[CfgResult[S]] = Seq()

  /**
    * Extends the given program using the given results of the analysis.
    *
    * @param program The program to extend.
    * @param results The result of the analysis.
    * @return The extended program
    */
  override def extendProgram(program: sil.Program, results: ProgramResult[S]): sil.Program = {
    // extend methods
    val extendedMethods = program.methods.map { method =>
      val identifier = SilverIdentifier(method.name)
      resultsToWorkWith = results.getTaggedResults(identifier).map(_._2).toSeq
      extendMethod(method, resultsToWorkWith.head) //TODO @flurin
    }

    // return extended program
    program.copy(methods = extendedMethods)(program.pos, program.info, program.errT)
  }

}