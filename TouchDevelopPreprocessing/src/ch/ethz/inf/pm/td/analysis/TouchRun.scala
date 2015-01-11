package ch.ethz.inf.pm.td.analysis


import java.io.{PrintWriter, StringWriter}

import apron.{Box, OptOctagon, Polka}
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.{NonrelationalStringDomain, StringKSetDomain}
import ch.ethz.inf.pm.sample.execution.EntryStateBuilder
import ch.ethz.inf.pm.sample.property.SingleStatementProperty
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleMessage}
import ch.ethz.inf.pm.td.analysis.TouchDevelopEntryStateBuilder._
import ch.ethz.inf.pm.td.compiler.{TouchProgramPointRegistry, TouchCompiler}
import ch.ethz.inf.pm.td.domain._
import ch.ethz.inf.pm.td.output.Exporters

object TouchDevelopEntryStateBuilder {

  type HeapId = ProgramPointHeapIdentifier

  type SemanticDomainType = StringsAnd[InvalidAnd[SummaryNodeWrapper[NonDeterminismWrapper[DoublePentagons]]], NonrelationalStringDomain[StringKSetDomain]]

  type NonRelHeapType = NonRelationalHeapDomain[HeapId]
  type SummaryHeapType = NonRelationalSummaryCollectionHeapDomain[HeapId]
  type MayMustHeapType = NonRelationalMayAndMustHeapDomain[HeapId]

  // Three different AbstractState instantiations (choice of heap)
  type AnalysisMayCollectionStateType = AbstractState[SemanticDomainType, NonRelationalHeapDomain[HeapId], HeapId]
  type AnalysisSummaryCollectionHeapStateType = AbstractState[SemanticDomainType, NonRelationalSummaryCollectionHeapDomain[HeapId], HeapId]
  type AnalysisMayMustCollectionStateHeapType = AbstractState[SemanticDomainType, NonRelationalMayAndMustHeapDomain[HeapId], HeapId]

  // "lub" type of our different AbstractState type instantiations (ignores the heap)
  type AnalysisBasicStateType = AbstractState[SemanticDomainType, _ <: HeapDomain[_, HeapId], HeapId]

}

//case class SummaryEntryStateBuilder(touchParams:TouchAnalysisParameters) extends TouchDevelopEntryStateBuilder[AnalysisSummaryCollectionHeapStateType](touchParams) {
//
//  override def topState = {
//
//    val entryValue = ExpressionSet()
//    type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, SummaryHeapType, HeapId]
//
//    val heapDomain = new NonRelationalSummaryCollectionHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
//    heapDomain.setParameter("UnsoundEntryState", false)
//
//    val entryDomain = HeapAndAnotherDomain[SemanticDomainType, SummaryHeapType, HeapId](numerical, heapDomain)
//    new AbstractState(entryDomain, entryValue)
//
//  }
//
//}


case class TouchEntryStateBuilder(touchParams:TouchAnalysisParameters)
  extends TouchDevelopEntryStateBuilder[TouchState.Default[SemanticDomainType]](touchParams) {

  override def topState = {

    TouchState.Default[SemanticDomainType](valueState = numerical)

  }

}

//case class MayMustEntryStateBuilder(touchParams:TouchAnalysisParameters) extends TouchDevelopEntryStateBuilder[AnalysisMayMustCollectionStateHeapType](touchParams) {
//
//  override def topState = {
//
//    val entryValue = ExpressionSet()
//    type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, MayMustHeapType, HeapId]
//
//    val mustHeapDomain = new NonRelationalMustHeapDomain[HeapId](new TupleIdSetDomain(), heapID)
//    val mayHeapDomain = new NonRelationalHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
//    val heapDomain: MayMustHeapType = new NonRelationalMayAndMustHeapDomain[HeapId](mayHeapDomain, mustHeapDomain)
//    heapDomain.setParameter("UnsoundEntryState", false)
//
//    val entryDomain = HeapAndAnotherDomain[SemanticDomainType, MayMustHeapType, HeapId](numerical, heapDomain)
//    new AbstractState(entryDomain, entryValue)
//
//  }
//
//}

//case class MayEntryStateBuilder(touchParams:TouchAnalysisParameters) extends TouchDevelopEntryStateBuilder[AnalysisMayCollectionStateType](touchParams) {
//
//  override def topState = {
//
//    val entryValue = ExpressionSet()
//    type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, NonRelHeapType, HeapId]
//
//    val heapDomain = new NonRelationalHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
//    heapDomain.setParameter("UnsoundEntryState", false)
//
//    val entryDomain = HeapAndAnotherDomain[SemanticDomainType, NonRelHeapType, HeapId](numerical, heapDomain)
//    new AbstractState(entryDomain, entryValue)
//
//  }
//
//}

abstract class TouchDevelopEntryStateBuilder[S <: State[S]](touchParams:TouchAnalysisParameters) extends EntryStateBuilder[S]() {

  def heapID:HeapId = new SimpleProgramPointHeapIdentifier(null, SystemParameters.typ)

  def numerical:SemanticDomainType = {
    val numericalDomainChoice = touchParams.domains.numericalDomain
    val domain =
      numericalDomainChoice match {
        case NumericDomainChoice.Pentagons => DoublePentagons(BoxedNonRelationalNumericalDomain[DoubleInterval](DoubleInterval(1,1)),UpperBound())
//        case NumericDomainChoice.Intervals => ApronInterface.Default(None, new Box(), env = Set.empty).factory()
//        case NumericDomainChoice.Octagons => ApronInterface.Default(None, new OptOctagon(), env = Set.empty).factory()
//        case NumericDomainChoice.Polyhedra => ApronInterface.Default(None, new Polka(false), env = Set.empty).factory()
//        case NumericDomainChoice.StrictPolyhedra => ApronInterface.Default(None, new Polka(true), env = Set.empty).factory()
      }
    StringsAnd(
      InvalidAnd(
        SummaryNodeWrapper(NonDeterminismWrapper(domain))
      ),
      NonrelationalStringDomain(
        StringKSetDomain(TouchAnalysisParameters.stringRepresentationBound)
      )
    )
  }
}


case class AnalysisThread(file: String, customTouchParams: Option[TouchAnalysisParameters] = None) extends Thread {

  var messages: Set[SampleMessage] = Set.empty


  override def run {
    try {

      customTouchParams.foreach(p => TouchAnalysisParameters.set(p))
      val touchParams = TouchAnalysisParameters.get
      TouchProgramPointRegistry.reset()

      Exporters.setStatus("Analyzing")

      SystemParameters.compiler = new TouchCompiler
      SystemParameters.property = new SingleStatementProperty(new BottomVisitor)
      SystemParameters.analysisOutput = if (touchParams.reporting.silent) new StringCollector() else new StdOutOutput()
      SystemParameters.progressOutput = if (touchParams.reporting.silent) new StringCollector() else new StdOutOutput()

      SystemParameters.compiler.reset()
      SystemParameters.resetNativeMethodsSemantics()
      SystemParameters.compiler.compile(file)
      SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics())

//      if (TouchAnalysisParameters.enableCollectionSummaryAnalysis) {
//        val entryState = new SummaryEntryStateBuilder(touchParams).topState
//        val analysis = new TouchAnalysis[ApronInterface.Default, NonrelationalStringDomain[StringKSetDomain]]
//        analysis.analyze(entryState)
//      } else if (TouchAnalysisParameters.enableCollectionMustAnalysis) {
        val entryState = new TouchEntryStateBuilder(touchParams).topState
        val analysis = new TouchAnalysis[ApronInterface.Default, NonrelationalStringDomain[StringKSetDomain]]
        analysis.analyze(entryState)
//      } else {
//        val entryState = new MayEntryStateBuilder(touchParams).topState
//        val analysis = new TouchAnalysis[ApronInterface.Default,NonrelationalStringDomain[StringKSetDomain]]
//        analysis.analyze(entryState)
//      }

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
        throw x

    }
  }

}

object TouchRun {

  def runSingle(file: String, customTouchParams: Option[TouchAnalysisParameters] = None): Seq[SampleMessage] = {

    this.synchronized {
      val t = new AnalysisThread(file, customTouchParams)
      val initialTime = System.currentTimeMillis()
      t.start()
      while (t.isAlive && (TouchAnalysisParameters.timeout.isEmpty || System.currentTimeMillis() - initialTime < TouchAnalysisParameters.timeout.get * 1000))
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