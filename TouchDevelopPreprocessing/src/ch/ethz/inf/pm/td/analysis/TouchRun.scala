package ch.ethz.inf.pm.td.analysis


import java.io.{PrintWriter, StringWriter}

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.{NonrelationalStringDomain, StringKSetDomain}
import ch.ethz.inf.pm.sample.execution.EntryStateBuilder
import ch.ethz.inf.pm.sample.property.SingleStatementProperty
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleMessage}
import ch.ethz.inf.pm.td.compiler.{TouchProgramPointRegistry, TouchCompiler}
import ch.ethz.inf.pm.td.domain._
import ch.ethz.inf.pm.td.output.Exporters

object TouchEntryStateBuilder {

  type ValueState =
    StringsAnd[
      InvalidAnd[
        StaticVariablePackingDomain[
          BoxedNonRelationalNumericalDomain[DoubleInterval],
          SummaryNodeWrapper[NonDeterminismWrapper[Apron.OptOctagons]]
        ]
      ],
      NonrelationalStringDomain[StringKSetDomain]
    ]

  type State = TouchState.Default[ValueState]

}

case class TouchEntryStateBuilder(touchParams:TouchAnalysisParameters)
  extends EntryStateBuilder[TouchEntryStateBuilder.State] {

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
    val relationalDomain = SummaryNodeWrapper(NonDeterminismWrapper[Apron.OptOctagons](Apron.OptOctagons.Bottom))
    val nonRelationalDomain = BoxedNonRelationalNumericalDomain[DoubleInterval](DoubleInterval.Top)

    StringsAnd(
      InvalidAnd(
        StaticVariablePackingDomain(nonRelationalDomain,VariablePackMap(classifier,relationalDomain,Map.empty))
      ),
      NonrelationalStringDomain(
        StringKSetDomain.Top(TouchAnalysisParameters.get.stringRepresentationBound).asInstanceOf[StringKSetDomain]
      )
    )
  }

}

case class AnalysisThread(file: String, customTouchParams: Option[TouchAnalysisParameters] = None) extends Thread {

  var messages: Set[SampleMessage] = Set.empty


  override def run() {
    try {

      customTouchParams.foreach(p => TouchAnalysisParameters.set(p))
      val touchParams = TouchAnalysisParameters.get
      TouchProgramPointRegistry.reset()

      Exporters.setStatus("Analyzing")

      SystemParameters.compiler = new TouchCompiler
      SystemParameters.property = new SingleStatementProperty(new BottomVisitor)
      SystemParameters.analysisOutput = if (touchParams.silent) new StringCollector() else new StdOutOutput()
      SystemParameters.progressOutput = if (touchParams.silent) new StringCollector() else new StdOutOutput()

      SystemParameters.compiler.reset()
      SystemParameters.resetNativeMethodsSemantics()
      SystemParameters.compiler.compile(file)
      SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics())

      val entryState = new TouchEntryStateBuilder(touchParams).topState
      val analysis = new TouchAnalysis
      analysis.analyze(entryState)

      Exporters.setStatus("Done")
      messages = Reporter.seenErrors ++ Reporter.seenInfos
      SystemParameters.resetOutput()

    } catch {

      case x: ThreadDeath => Exporters.setStatus("Timeout")

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

object TouchRun {

  /**
   * We use this to communicate if something bad happened inside the analysis thread.
   * We then assert that this flag is false, if we want to crash for failed analyses (e.g. in tests)
   */
  var threadFailed:Boolean = false

  def runSingle(file: String, customTouchParams: Option[TouchAnalysisParameters] = None): Seq[SampleMessage] = {

    threadFailed = false

    this.synchronized {
      val t = new AnalysisThread(file, customTouchParams)
      val initialTime = System.currentTimeMillis()
      t.start()
      while (t.isAlive && (TouchAnalysisParameters.get.timeout.isEmpty || System.currentTimeMillis() - initialTime < TouchAnalysisParameters.get.timeout.get * 1000))
        this.wait(1000)
      while (t.isAlive) {
        System.out.println("TIME IS UP! Trying to stop a thread")
        Exporters.setStatus("Timeout")
        for (i <- 0 to 100) t.stop()
        this.wait(1000)
      }
      t.messages
    }.toSeq

  }

  def main(files: Array[String]) {

    if (files.isEmpty) {
      println("No arguments given!")
      sys.exit()
    }

    files foreach (
      f =>
        try {
          runSingle(f)
        } catch {
          case x: Throwable =>
            val sw: StringWriter = new StringWriter()
            val pw: PrintWriter = new PrintWriter(sw)
            x.printStackTrace(pw)
            Exporters.setDebugInformation(x.toString + x.getMessage + sw.toString)
            Exporters.setStatus("Failed")
            throw x
        }

      )
  }

}