package ch.ethz.inf.pm.td.analysis


import java.io.{PrintWriter, StringWriter}

import apron.{OptOctagon, Polka}
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.{NonrelationalStringDomain, StringKSetDomain}
import ch.ethz.inf.pm.sample.property.SingleStatementProperty
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleMessage}
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.domain._
import ch.ethz.inf.pm.td.output.Exporters

case class AnalysisThread(file: String, customTouchParams: Option[TouchAnalysisParameters] = None) extends Thread {

  var messages: Set[SampleMessage] = Set.empty

  type HeapId = ProgramPointHeapIdentifier

  type SemanticDomainType = StringsAnd[InvalidAnd[ApronInterface.Default], StringKSetDomain, NonrelationalStringDomain[StringKSetDomain]]

  type NonRelHeapType = NonRelationalHeapDomain[HeapId]
  type SummaryHeapType = NonRelationalSummaryCollectionHeapDomain[HeapId]
  type MayMustHeapType = NonRelationalMayAndMustHeapDomain[HeapId]

  // Three different AbstractState instantiations (choice of heap)
  type AnalysisStateType = AbstractState[SemanticDomainType, NonRelationalHeapDomain[HeapId], HeapId]
  type AnalysisSummaryHeapStateType = AbstractState[SemanticDomainType, NonRelationalSummaryCollectionHeapDomain[HeapId], HeapId]
  type AnalysisStateMustHeapType = AbstractState[SemanticDomainType, NonRelationalMayAndMustHeapDomain[HeapId], HeapId]

  // "lub" type of our different AbstractState type instantiations (ignores the heap)
  type AnalysisBasicStateType = AbstractState[SemanticDomainType, _ <: HeapDomain[_, HeapId], HeapId]

  override def run {
    try {

      customTouchParams.foreach(p => TouchAnalysisParameters.set(p))
      val touchParams = TouchAnalysisParameters.get

      Exporters.setStatus("Analyzing")

      SystemParameters.compiler = new TouchCompiler
      SystemParameters.property = new SingleStatementProperty(new BottomVisitor)
      SystemParameters.analysisOutput = if (touchParams.reporting.silent) new StringCollector() else new StdOutOutput()
      SystemParameters.progressOutput = if (touchParams.reporting.silent) new StringCollector() else new StdOutOutput()

      SystemParameters.compiler.reset()
      SystemParameters.resetNativeMethodsSemantics()
      SystemParameters.compiler.compile(file)
      SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics())

      //EntryState
      val numericalDomainChoice = touchParams.domains.numericalDomain
      val domain =
        numericalDomainChoice match {
          case NumericDomainChoice.Octagons => new OptOctagon()
          case NumericDomainChoice.Polyhedra => new Polka(false)
          case NumericDomainChoice.StrictPolyhedra => new Polka(true)
        }
      val numerical: SemanticDomainType = new StringsAnd(new InvalidAnd(new ApronInterface.Default(None, domain, env = Set.empty).factory()))
      val heapID = new SimpleProgramPointHeapIdentifier(null, SystemParameters.typ)

      val entryValue = ExpressionSet()

      if (TouchAnalysisParameters.enableCollectionSummaryAnalysis || TouchAnalysisParameters.lowPrecision) {
        type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, SummaryHeapType, HeapId]

        val heapDomain = new NonRelationalSummaryCollectionHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
        heapDomain.setParameter("UnsoundEntryState", false)

        val entryDomain = HeapAndAnotherDomain[SemanticDomainType, SummaryHeapType, HeapId](numerical, heapDomain)
        val entryState: AnalysisSummaryHeapStateType = new AbstractState(entryDomain, entryValue)

        val analysis = new TouchAnalysisWithApron[ApronInterface.Default, StringKSetDomain, NonrelationalStringDomain[StringKSetDomain]]
        analysis.analyze(entryState)
      }
      else if (TouchAnalysisParameters.enableCollectionMustAnalysis) {
        type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, MayMustHeapType, HeapId]

        val mustHeapDomain = new NonRelationalMustHeapDomain[HeapId](new TupleIdSetDomain(), heapID)
        val mayHeapDomain = new NonRelationalHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
        val heapDomain: MayMustHeapType = new NonRelationalMayAndMustHeapDomain[HeapId](mayHeapDomain, mustHeapDomain)
        heapDomain.setParameter("UnsoundEntryState", false)

        val entryDomain = HeapAndAnotherDomain[SemanticDomainType, MayMustHeapType, HeapId](numerical, heapDomain)
        val entryState: AnalysisStateMustHeapType = new AbstractState(entryDomain, entryValue)

        val analysis = new TouchAnalysisWithApron[ApronInterface.Default, StringKSetDomain, NonrelationalStringDomain[StringKSetDomain]]
        analysis.analyze(entryState)
      } else {
        type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, NonRelHeapType, HeapId]

        val heapDomain = new NonRelationalHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
        heapDomain.setParameter("UnsoundEntryState", false)

        val entryDomain = HeapAndAnotherDomain[SemanticDomainType, NonRelHeapType, HeapId](numerical, heapDomain)
        val entryState: AnalysisStateType = new AbstractState(entryDomain, entryValue)

        val analysis = new TouchAnalysisWithApron[ApronInterface.Default, StringKSetDomain, NonrelationalStringDomain[StringKSetDomain]]
        analysis.analyze(entryState)
      }

      Exporters.setStatus("Done")
      messages = Reporter.seenErrors ++ Reporter.seenInfos
      SystemParameters.resetOutput()

    } catch {

      case x: ThreadDeath => Exporters.setStatus("Timeout")

    }
  }

}

object TouchApronRun {

  def runSingle(file: String, customTouchParams: Option[TouchAnalysisParameters] = None): Seq[SampleMessage] = {

    this.synchronized {
      val t = new AnalysisThread(file, customTouchParams)
      val initialTime = System.currentTimeMillis()
      t.start()
      while (t.isAlive && (TouchAnalysisParameters.timeout.isEmpty || System.currentTimeMillis() - initialTime < TouchAnalysisParameters.timeout.get * 1000))
        this.wait(1000)
      while (t.isAlive) {
        System.out.println("TIME IS UP! Trying to stop a thread")
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