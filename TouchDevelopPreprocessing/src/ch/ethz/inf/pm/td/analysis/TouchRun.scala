/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.analysis


import java.io.{PrintWriter, StringWriter}
import java.lang.Thread.UncaughtExceptionHandler
import java.nio.file.Paths

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.{NonrelationalStringDomain, StringKSetDomain}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.{Compilable, WeightedGraph}
import ch.ethz.inf.pm.sample.property.SingleStatementProperty
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleMessage}
import ch.ethz.inf.pm.sample.util.AccumulatingTimer
import ch.ethz.inf.pm.td.cloud.AbstractEventGraph
import ch.ethz.inf.pm.td.cloud.AbstractEventGraph.AbstractEventWithState
import ch.ethz.inf.pm.td.compiler.{TouchCompiler, TouchProgramPointRegistry, UnsupportedLanguageFeatureException}
import ch.ethz.inf.pm.td.domain.TouchState.CollectingDomain
import ch.ethz.inf.pm.td.domain._
import ch.ethz.inf.pm.td.output.Exporters
import com.typesafe.scalalogging.LazyLogging

object TouchEntryStateBuilder {

  type PreAnalysisValueState =
    StringsAnd[
      CollectingDomain,
      NonrelationalStringDomain[StringKSetDomain]
      ]

  type ValueState =
    StringsAnd[
      InvalidAnd[
        StaticVariablePackingDomain[
          BoxedNonRelationalNumericalDomain[DoubleInterval],
          SummaryNodeWrapper[NonDeterminismWrapper[Apron.FloatOptOctagons]]
        ]
      ],
      NonrelationalStringDomain[StringKSetDomain]
    ]

  type State = TouchState.Default[ValueState]

  type PreAnalysisState = TouchState.PreAnalysis[PreAnalysisValueState]

}

case class TouchEntryStateBuilder(touchParams:TouchAnalysisParameters)
  extends ForwardEntryStateBuilder[TouchEntryStateBuilder.State] {

  def preAnalysisTopState:TouchEntryStateBuilder.PreAnalysisState = {
    TouchState.PreAnalysis(valueState =
      StringsAnd(
        CollectingDomain.Top,
        NonrelationalStringDomain(
          StringKSetDomain.Top(TouchAnalysisParameters.get.stringRepresentationBound).asInstanceOf[StringKSetDomain]
        )
      )
    )
  }


  override def topState = {

    TouchState.Default(valueState = numerical(None))

  }

  def topStateWithClassifier(c:VariablePackingClassifier) = {

    TouchState.Default(valueState = numerical(Some(c)))

  }

  def numerical(c:Option[VariablePackingClassifier]):TouchEntryStateBuilder.ValueState = {
    val classifier =
      c match {
        case Some(x) => x
        case None => VariablePackingClassifier.OnePacker
      }
    val relationalDomain = SummaryNodeWrapper(NonDeterminismWrapper[Apron.FloatOptOctagons](Apron.FloatOptOctagons.Bottom))
    val nonRelationalDomain = BoxedNonRelationalNumericalDomain[DoubleInterval](DoubleInterval.Top)

    StringsAnd(
      InvalidAnd(
        StaticVariablePackingDomain(nonRelationalDomain,classifier,relationalDomain,Map.empty)
      ),
      NonrelationalStringDomain(
        StringKSetDomain.Top(TouchAnalysisParameters.get.stringRepresentationBound).asInstanceOf[StringKSetDomain]
      )
    )
  }

}


trait TouchDevelopAnalysisRunner[S <: State[S]] extends AnalysisRunner[TouchEntryStateBuilder.State] with LazyLogging {

  def touchParams:TouchAnalysisParameters

  override val compiler = new TouchCompiler

  override def prepareContext() = {
    super.prepareContext()

    AbstractEventGraph.reset()
    MethodSummaries.reset()
    SystemParameters.reset()
    TouchVariablePacking.reset()
    Localization.reset()
    TouchProgramPointRegistry.reset()
    AccumulatingTimer.reset()
    Reporter.enableAllOutputs()
    Reporter.reset()
    RequiredLibraryFragmentAnalysis.spottedFields = Set.empty

    SystemParameters.compiler = compiler
    SystemParameters.compiler.generateTopType()
    SystemParameters.analysisOutput = if (touchParams.silent) new StringCollector() else new StdOutOutput()
    SystemParameters.progressOutput = if (touchParams.silent) new StringCollector() else new StdOutOutput()

    SystemParameters.compiler.reset()
    SystemParameters.resetNativeMethodsSemantics()
    SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics)

  }

  override def run(comp: Compilable): List[AnalysisResult] = {
    prepareContext()
    logger.info(" Compiling " + comp.label)
    compiler.compile(comp)
    Exporters.setStatus("Analyzing")
    val results = analyze()
    Exporters.setStatus("Done")
    results
  }

  private def analyze(): List[AnalysisResult] = {
    val entryState = new TouchEntryStateBuilder(TouchAnalysisParameters.get).topState
    SystemParameters.addNativeMethodsSemantics(compiler.getNativeMethodsSemantics)
    val analyzer = new TouchAnalysis[Apron.FloatOptOctagons, NonrelationalStringDomain[StringKSetDomain]]
    val methods:List[MethodAnalysisResult[S]] =
      analyzer.analyze(Nil,entryState) map { x => MethodAnalysisResult[S](x._2,x._3.asInstanceOf[TrackingCFGState[S]]) }
    val abs:WeightedGraph[NodeWithState[S],AbstractEventGraph.EdgeLabel.Value] =
      AbstractEventGraph.toWeightedGraph
    val messages = Reporter.messages
    WeightedGraphAnalysisResult("Abstract Event Graph",abs)::methods:::messages.toList
  }

}

object TouchDevelopAnalysisRunner {

  case class Default(
                      touchParams:TouchAnalysisParameters = TouchAnalysisParameters.get,
                      analysis:Analysis[TouchEntryStateBuilder.State] = new DefaultAnalysis
                    )
    extends TouchDevelopAnalysisRunner[TouchEntryStateBuilder.State] {

    override def toString:String = "Default TouchDevelop Analysis Runner"

  }

  class DefaultAnalysis
    extends SimpleForwardAnalysis[TouchEntryStateBuilder.State](new TouchEntryStateBuilder(TouchAnalysisParameters.get)) {
    override def toString:String = "Default TouchDevelop Analysis"
  }

}


case class AnalysisThread(comp: Compilable, customTouchParams: Option[TouchAnalysisParameters] = None)
  extends Thread with LazyLogging {

  var results: List[AnalysisResult] = Nil

  override def run() {
    try {

      results = TouchDevelopAnalysisRunner.Default().run(comp)

    } catch {

      case x: ThreadDeath => Exporters.setStatus("Timeout")

      case x: UnsupportedLanguageFeatureException =>
        println("FAILED: Skipping this script, as is contains unsupported language constructs")
        Exporters.setDebugInformation(x.toString + x.getMessage)
        Exporters.setStatus("Failed")
        TouchRun.threadFailed = true

      case x: Throwable =>
        val sw: StringWriter = new StringWriter()
        val pw: PrintWriter = new PrintWriter(sw)
        x.printStackTrace(pw)
        Exporters.setDebugInformation(x.toString + x.getMessage + sw.toString)
        Exporters.setStatus("Failed")
        TouchRun.threadFailed = true
        throw x

    }
  }

}

object TouchRun extends LazyLogging {

  /**
   * We use this to communicate if something bad happened inside the analysis thread.
   * We then assert that this flag is false, if we want to crash for failed analyses (e.g. in tests)
   */
  var threadFailed:Boolean = false

  def runInThread(comp: Compilable): List[AnalysisResult] = {

    threadFailed = false

    this.synchronized {
      val t = AnalysisThread(comp)
      val initialTime = System.currentTimeMillis()
      t.start()
      while (t.isAlive && (TouchAnalysisParameters.get.timeout.isEmpty
        || System.currentTimeMillis() - initialTime < TouchAnalysisParameters.get.timeout.get * 1000))
        this.wait(1000)
      while (t.isAlive) {
        System.out.println("TIME IS UP! Trying to stop a thread")
        Exporters.setStatus("Timeout")
        for (i <- 0 to 100) t.stop()
        this.wait(1000)
      }
      t.results
    }

  }

  def main(files: Array[String]) {

    if (files.isEmpty) {
      println("No arguments given!")
      sys.exit()
    }

    files foreach (
      f => {
        try {
          val comp =
            if (f.startsWith("td://")) {
              Compilable.Identifier(f)
            } else {
              Compilable.Path(Paths.get(f))
            }
          runInThread(comp)
        } catch {
          case x: Throwable =>
            val sw: StringWriter = new StringWriter()
            val pw: PrintWriter = new PrintWriter(sw)
            x.printStackTrace(pw)
            Exporters.setDebugInformation(x.toString + x.getMessage + sw.toString)
            Exporters.setStatus("Failed")
            throw x
        }

      })
  }

}