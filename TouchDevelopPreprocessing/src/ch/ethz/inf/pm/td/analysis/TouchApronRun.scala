package ch.ethz.inf.pm.td.analysis


import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.property.SingleStatementProperty

import apron._
import numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.{NonrelationalStringDomain, StringKSetDomain}
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleMessage}
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.SimpleProgramPointHeapIdentifier
import ch.ethz.inf.pm.td.domain._

object TouchApronRun {

  type HeapId = ProgramPointHeapIdentifier

  type SemanticDomainType = StringsAnd[InvalidAnd[ApronInterface.Default],StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]]

  type NonRelHeapType = NonRelationalHeapDomain[HeapId]
  type SummaryHeapType = NonRelationalSummaryCollectionHeapDomain[HeapId]
  type MayMustHeapType = NonRelationalMayAndMustHeapDomain[HeapId]

  // Three different AbstractState instantiations (choice of heap)
  type AnalysisStateType = AbstractState[SemanticDomainType, NonRelationalHeapDomain[HeapId], HeapId]
  type AnalysisSummaryHeapStateType = AbstractState[SemanticDomainType, NonRelationalSummaryCollectionHeapDomain[HeapId], HeapId]
  type AnalysisStateMustHeapType = AbstractState[SemanticDomainType, NonRelationalMayAndMustHeapDomain[HeapId], HeapId]

  // "lub" type of our different AbstractState type instantiations (ignores the heap)
  type AnalysisBasicStateType = AbstractState[SemanticDomainType, _ <: HeapDomain[_, HeapId], HeapId]




  def runSingle(file: String, customTouchParams: Option[TouchAnalysisParameters] = None): Seq[SampleMessage] = {
    customTouchParams.foreach(p => TouchAnalysisParameters.set(p))
    val touchParams = TouchAnalysisParameters.get

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
        case NumericDomainChoice.Octagons => new Octagon()
        case NumericDomainChoice.Polyhedra => new Polka(false)
        case NumericDomainChoice.StrictPolyhedra => new Polka(true)
      }
    val numerical: SemanticDomainType = new StringsAnd(new InvalidAnd(new ApronInterface.Default(None, domain, env = Set.empty).factory()))
    val heapID = new SimpleProgramPointHeapIdentifier(null,SystemParameters.typ)

    val entryValue = ExpressionSet()

    if (TouchAnalysisParameters.enableCollectionSummaryAnalysis) {
      type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, SummaryHeapType, HeapId]

      val heapDomain = new NonRelationalSummaryCollectionHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
      heapDomain.setParameter("UnsoundEntryState",false)

      val entryDomain = HeapAndAnotherDomain[SemanticDomainType, SummaryHeapType, HeapId](numerical, heapDomain)
      val entryState: AnalysisSummaryHeapStateType = new AbstractState(entryDomain, entryValue)

      val analysis = new TouchAnalysisWithApron[ApronInterface.Default,StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]]
      analysis.analyze(entryState)
    }
    else if (TouchAnalysisParameters.enableCollectionMustAnalysis) {
      type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, MayMustHeapType, HeapId]

      val mustHeapDomain = new NonRelationalMustHeapDomain[HeapId](new TupleIdSetDomain(), heapID)
      val mayHeapDomain = new NonRelationalHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
      val heapDomain: MayMustHeapType = new NonRelationalMayAndMustHeapDomain[HeapId](mayHeapDomain, mustHeapDomain)
      heapDomain.setParameter("UnsoundEntryState",false)

      val entryDomain = HeapAndAnotherDomain[SemanticDomainType, MayMustHeapType, HeapId](numerical, heapDomain)
      val entryState: AnalysisStateMustHeapType = new AbstractState(entryDomain, entryValue)

      val analysis = new TouchAnalysisWithApron[ApronInterface.Default,StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]]
      analysis.analyze(entryState)
    } else {
      type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, NonRelHeapType, HeapId]

      val heapDomain = new NonRelationalHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
      heapDomain.setParameter("UnsoundEntryState",false)

      val entryDomain = HeapAndAnotherDomain[SemanticDomainType, NonRelHeapType, HeapId](numerical, heapDomain)
      val entryState: AnalysisStateType = new AbstractState(entryDomain, entryValue)

      val analysis = new TouchAnalysisWithApron[ApronInterface.Default,StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]]
      analysis.analyze(entryState)
    }


    val messages: Set[SampleMessage] = Reporter.seenErrors ++ Reporter.seenInfos
    SystemParameters.resetOutput()
    messages.toSeq
  }

  def extractNumericalState[S <: State[S]](s: S): ApronInterface.Default = {
    val state = s.asInstanceOf[AnalysisBasicStateType]
    val apronState = state._1._1._1._1 // tuple-induced insanity - should introduce proper names in sample domains
    apronState
  }

  def extractVariableEnv[S <: State[S]](s: S): VariableEnv[HeapId] = {
    val state = s.asInstanceOf[AnalysisBasicStateType]
    if (TouchAnalysisParameters.enableCollectionMustAnalysis) {
      val heapState = state._1._2.asInstanceOf[MayMustHeapType]
      heapState._1._1
    } else {
      val heapState = state._1._2.asInstanceOf[AbstractNonRelationalHeapDomain[HeapId, _]]
      heapState._1
    }
  }

  def extractInvalidValueInfo[S <: State[S]](s: S): Map[Identifier, PositionedInvalidValueDomain] = {
    val state = s.asInstanceOf[AnalysisBasicStateType]
    // tuple-induced insanity - should introduce proper names in sample domains
    val stringAnd = state._1._1
    val invalidAnd = stringAnd._1
    val invalidDomain = invalidAnd._2
    invalidDomain.map
  }



  def main(files: Array[String]) {

    if(files.isEmpty) {
      println("No arguments given!")
      sys.exit()
    }

    files foreach (f => runSingle(f))
  }

}