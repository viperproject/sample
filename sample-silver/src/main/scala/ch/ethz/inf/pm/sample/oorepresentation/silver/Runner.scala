/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import java.io.{File, PrintWriter}

import ch.ethz.inf.pm.sample.{StringCollector, SystemParameters}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.abstractdomain._
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
import ch.ethz.inf.pm.sample.reporting.Reporter
import viper.silicon.Silicon

import scala.collection.JavaConverters._
import scala.collection.mutable

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
    val results = DefaultProgramResult[S](program)
    for (method <- methodsToAnalyze) {
      val identifier = method.name
      val result = analysis.analyze(program, method)
      results.setResult(identifier, result)
    }
    results
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
    println("\n***************\n* Exit States *\n***************\n")
    result.identifiers.foreach(ident => println(ident.name + "() -> " + result.getResult(ident).exitState()))
  }
}

/** Interprocedural analysis runner for Silver programs. */
trait InterproceduralSilverAnalysisRunner[S <: State[S]]
extends SilverAnalysisRunner[S] {

  override val analysis: InterproceduralSilverForwardAnalysis[S]

  override protected def _run(): ProgramResult[S] = {
    prepareContext()
    val result = DefaultProgramResult[S](program)
    val (condensedCallGraph, callsInProgram) = analyzeCallGraph(program)
    // analyze the methods in topological order of the condensed callgraph
    for(condensation <- new TopologicalOrderIterator(condensedCallGraph).asScala; method <- condensation.asScala) {
      val res = analysis.analyze(program, method, callsInProgram)
      for(ident <- res.identifiers) {
        assert(result.identifiers.count(_ == ident) == 0)
        result.setResult(ident, res.getResult(ident))
      }
    }
    result
  }

  /**
    * Analyze the given program an return a tuple of condensed Callgraph and a map containing all calls to each method.
    * The condensed callgraph uses sets of method declarations as nodes. These nodes are the connected components inside a call graph.
    * E.g. foo() calls bar(), bar() calls foo(), bar() calls baz(). The condensed graph will be:
    * set(foo, bar) -> set(baz)
    *
    * @param program
    * @return tuple of condensed call graph and map containing all method calls
    */
  private def analyzeCallGraph(program: SilverProgramDeclaration) : (DirectedGraph[java.util.Set[SilverMethodDeclaration], Functions.Edge[java.util.Set[SilverMethodDeclaration]]], Map[String, Set[BlockPosition]]) = {
    // Most code was taken from ast.utility.Functions in silver repo!
    val callGraph = new DefaultDirectedGraph[SilverMethodDeclaration, Functions.Edge[SilverMethodDeclaration]](Factory[SilverMethodDeclaration]())
    var callsInProgram : Map[String, Set[BlockPosition]] = Map().withDefault(_ => Set())

    for (f <- program.methods) {
      callGraph.addVertex(f)
    }

    def process(m: SilverMethodDeclaration, e: Statement) {
      e match {
        case MethodCall(_, method: Variable, _, _, _, _) => {
          callGraph.addEdge(m, program.methods.filter(_.name.name == method.getName).head)
          val pp = m.body.getBlockPosition(ProgramPointUtils.identifyingPP(e))
          callsInProgram = (callsInProgram + (method.getName -> (callsInProgram(method.getName) + pp)))
        }
        case _ => e.getChildren foreach(process(m, _))
      }
    }

    for (m <- program.methods;
         block <- m.body.blocks;
         statement<- block.elements){
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
    //TODO do we care about new CycleDetector(condensedCallGraph).detectCycles()
    (condensedCallGraph, callsInProgram)
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
    val program = extend(args) // extend the program with permission inferred by the analysis
    // verified the extended program with Silicon
    val silicon = new Silicon(Seq(("startedBy", "viper.silicon.SiliconTests")))
    silicon.parseCommandLine(Seq("dummy.sil"))
    silicon.start()
    val result: viper.silver.verifier.VerificationResult = silicon.verify(program)
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

    // verify the extended program with Silicon
    val silicon = new Silicon(Seq(("startedBy", "viper.silicon.SiliconTests")))
    silicon.parseCommandLine(Seq("dummy.sil"))
    silicon.start()
    val result: viper.silver.verifier.VerificationResult = silicon.verify(extended)
    println("\n***********************\n* Verification Result *\n***********************\n\n" + result)
  }

  override def toString = "Specification Inference"
}
