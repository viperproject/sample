package ch.ethz.inf.pm.td.domain


import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, BottomVisitor, TouchAnalysis, TouchAnalysisWithApron}
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.property.SingleStatementProperty

import apron._
import numericaldomain.{BoxedNonRelationalNumericalDomain, Interval, ApronInterface}
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.{NonrelationalStringDomain, StringKSetDomain}
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.SimpleProgramPointHeapIdentifier
import ch.ethz.inf.pm.td.compiler.UnsupportedLanguageFeatureException
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleMessage}

object TouchRun {

  type HeapId = ProgramPointHeapIdentifier

  def main(files: Array[String]) {

    if(files.isEmpty) {
      println("No arguments given!")
      sys.exit()
    }


    SystemParameters.compiler = new TouchCompiler
    SystemParameters.property = new SingleStatementProperty(new BottomVisitor)
    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()

    for (file <- files) {

      try {
        SystemParameters.compiler.reset()
        SystemParameters.resetNativeMethodsSemantics()
        SystemParameters.compiler.compile(file)
        SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics())

        //EntryState
        val numerical : StringsAnd[InvalidAnd[BoxedNonRelationalNumericalDomain[Interval]],StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]] = new StringsAnd(new InvalidAnd(new BoxedNonRelationalNumericalDomain(new Interval(0,0))))
        val heapID = new SimpleProgramPointHeapIdentifier(null,SystemParameters.typ)

        val heapDomain: NonRelationalHeapDomain[HeapId] =
          new NonRelationalHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
        heapDomain.setParameter("UnsoundEntryState",false)

        val entryDomain =
          new HeapAndAnotherDomain[StringsAnd[InvalidAnd[BoxedNonRelationalNumericalDomain[Interval]],StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]],
            NonRelationalHeapDomain[HeapId], HeapId](numerical, heapDomain)

        val entryValue = ExpressionSet()

        val entryState = new AbstractState[StringsAnd[InvalidAnd[BoxedNonRelationalNumericalDomain[Interval]],StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]], NonRelationalHeapDomain[HeapId], HeapId](entryDomain, entryValue)

        val analysis = new TouchAnalysis[BoxedNonRelationalNumericalDomain[Interval],StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]]
        analysis.analyze(entryState)
      } catch {
        case e:UnsupportedLanguageFeatureException =>
          SystemParameters.progressOutput.put("UNSUPPORTED: Unsupported Language Feature: "+e.toString)
          SystemParameters.progressOutput.reset()
        case e:Exception =>
          SystemParameters.progressOutput.put("ANALYSIS ERROR: Exception during analysis of "+file+": "+e.toString)
          for(line <- e.getStackTrace) SystemParameters.progressOutput.put(line.toString)
          SystemParameters.progressOutput.reset()
      }

    }

  }

}


object TouchApronRun {

  type HeapId = ProgramPointHeapIdentifier

  type SemanticDomainType = StringsAnd[InvalidAnd[ApronInterface],StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]]

  type NonRelHeapType = NonRelationalHeapDomain[HeapId]
  type SummaryHeapType = NonRelationalSummaryCollectionHeapDomain[HeapId]
  type MayMustHeapType = NonRelationalMayAndMustHeapDomain[HeapId]

  // Three different AbstractState instantiations (choice of heap)
  type AnalysisStateType = AbstractState[SemanticDomainType, NonRelationalHeapDomain[HeapId], HeapId]
  type AnalysisSummaryHeapStateType = AbstractState[SemanticDomainType, NonRelationalSummaryCollectionHeapDomain[HeapId], HeapId]
  type AnalysisStateMustHeapType = AbstractState[SemanticDomainType, NonRelationalMayAndMustHeapDomain[HeapId], HeapId]

  // "lub" type of our different AbstractState type instantiations (ignores the heap)
  type AnalysisBasicStateType = AbstractState[SemanticDomainType, _ <: HeapDomain[_, HeapId], HeapId]




  def runSingle(file: String): Seq[SampleMessage] = {
    SystemParameters.compiler = new TouchCompiler
    SystemParameters.property = new SingleStatementProperty(new BottomVisitor)
    SystemParameters.analysisOutput = new StdOutOutput()
    SystemParameters.progressOutput = new StdOutOutput()

    SystemParameters.compiler.reset()
    SystemParameters.resetNativeMethodsSemantics()
    SystemParameters.compiler.compile(file)
    SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics())

    //EntryState
    val domain = new Octagon()
    //val domain = new Polka(true)
    val numerical: SemanticDomainType = new StringsAnd(new InvalidAnd(new ApronInterface(None, domain, env = Set.empty).factory()))
    val heapID = new SimpleProgramPointHeapIdentifier(null,SystemParameters.typ)

    val entryValue = ExpressionSet()

    if (TouchAnalysisParameters.enableCollectionSummaryAnalysis) {
      type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, SummaryHeapType, HeapId]

      val heapDomain = new NonRelationalSummaryCollectionHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
      heapDomain.setParameter("UnsoundEntryState",false)

      val entryDomain = new HeapAndAnotherDomain[SemanticDomainType, SummaryHeapType, HeapId](numerical, heapDomain)
      val entryState: AnalysisSummaryHeapStateType = new AbstractState(entryDomain, entryValue)

      val analysis = new TouchAnalysisWithApron[ApronInterface,StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]]
      analysis.analyze(entryState)
    }
    else if (TouchAnalysisParameters.enableCollectionMustAnalysis) {
      type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, MayMustHeapType, HeapId]

      val mustHeapDomain = new NonRelationalMustHeapDomain[HeapId](new TupleIdSetDomain(), heapID)
      val mayHeapDomain = new NonRelationalHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
      val heapDomain: MayMustHeapType = new NonRelationalMayAndMustHeapDomain[HeapId](mayHeapDomain, mustHeapDomain)
      heapDomain.setParameter("UnsoundEntryState",false)

      val entryDomain = new HeapAndAnotherDomain[SemanticDomainType, MayMustHeapType, HeapId](numerical, heapDomain)
      val entryState: AnalysisStateMustHeapType = new AbstractState(entryDomain, entryValue)

      val analysis = new TouchAnalysisWithApron[ApronInterface,StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]]
      analysis.analyze(entryState)
    } else {
      type HeapAndOtherType = HeapAndAnotherDomain[SemanticDomainType, NonRelHeapType, HeapId]

      val heapDomain = new NonRelationalHeapDomain[HeapId](new MaybeHeapIdSetDomain(), heapID)
      heapDomain.setParameter("UnsoundEntryState",false)

      val entryDomain = new HeapAndAnotherDomain[SemanticDomainType, NonRelHeapType, HeapId](numerical, heapDomain)
      val entryState: AnalysisStateType = new AbstractState(entryDomain, entryValue)

      val analysis = new TouchAnalysisWithApron[ApronInterface,StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]]
      analysis.analyze(entryState)
    }


    val messages: Set[SampleMessage] = Reporter.seenErrors ++ Reporter.seenInfos
    SystemParameters.resetOutput
    messages.toSeq
  }

  def extractNumericalState[S <: State[S]](s: S): ApronInterface = {
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
    invalidDomain.value
  }



  def main(files: Array[String]) {

    if(files.isEmpty) {
      println("No arguments given!")
      sys.exit()
    }

    files foreach runSingle
  }

}