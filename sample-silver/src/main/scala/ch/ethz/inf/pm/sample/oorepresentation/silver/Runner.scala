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
import viper.silver.ast.utility.Functions
import viper.silver.ast.utility.Functions.Factory
import viper.silver.{ast => sil}

import scala.collection.JavaConverters._

//import viper.silicon.Silicon

trait AbstractAnalysisRunner[S <: State[S]] {
  val compiler: SilverCompiler

  val analysis: SilverAnalysis[S]

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
    _run(compiler.program)
  }

  def run(program: sil.Program): ProgramResult[S] = {
    compiler.compileProgram(program)
    _run(compiler.program)
  }

  def run(program: SilverProgramDeclaration): ProgramResult[S] = {
    SystemParameters.tm = SilverTypeMap
    _run(program)
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

  protected def _run(program: SilverProgramDeclaration): ProgramResult[S] = {
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

  override protected def _run(program: SilverProgramDeclaration): ProgramResult[S] = {
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
    * @param program The program to be analyzed
    * @return Tuple of condensed call graph and map containing all method calls
    */
  private def analyzeCallGraph(program: SilverProgramDeclaration): (DirectedGraph[java.util.Set[SilverMethodDeclaration], Functions.Edge[java.util.Set[SilverMethodDeclaration]]], CallGraphMap) = {
    // Most code below was taken from ast.utility.Functions in silver repo!
    val callGraph = new DefaultDirectedGraph[SilverMethodDeclaration, Functions.Edge[SilverMethodDeclaration]](Factory[SilverMethodDeclaration]())
    var callsInProgram: CallGraphMap = Map().withDefault(_ => Set())

    for (f <- program.methods) {
      callGraph.addVertex(f)
    }

    def process(m: SilverMethodDeclaration, e: Statement) {
      e match {
        case MethodCall(_, method: Variable, _, _, _, _) =>
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