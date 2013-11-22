package ch.ethz.inf.pm.td.domain


import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, BottomVisitor, TouchAnalysis, TouchAnalysisWithApron}
import ch.ethz.inf.pm.td.compiler.UnsupportedLanguageFeatureException
import ch.ethz.inf.pm.sample._
import abstractdomain._
import abstractdomain.heapanalysis._
import property.SingleStatementProperty

import apron._
import heapanalysis.SimpleProgramPointHeapIdentifier
import numericaldomain.{BoxedNonRelationalNumericalDomain, Interval, ApronInterface}

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
        val numerical = new StringsAnd(new InvalidAnd(new BoxedNonRelationalNumericalDomain(new Interval(0,0))))
        val heapID = new SimpleProgramPointHeapIdentifier(null,SystemParameters.typ)

        val heapDomain: NonRelationalHeapDomain[HeapId] =
          new NonRelationalHeapDomain[HeapId](heapID.getType(), new MaybeHeapIdSetDomain(), heapID)
        heapDomain.setParameter("UnsoundEntryState",false)

        val entryDomain =
          new HeapAndAnotherDomain[StringsAnd[InvalidAnd[BoxedNonRelationalNumericalDomain[Interval]]], NonRelationalHeapDomain[HeapId], HeapId](numerical, heapDomain)

        val entryValue = new ExpressionSet(SystemParameters.typ.top())

        val entryState = new AbstractState[StringsAnd[InvalidAnd[BoxedNonRelationalNumericalDomain[Interval]]], NonRelationalHeapDomain[HeapId], HeapId](entryDomain, entryValue)

        val analysis = new TouchAnalysis[BoxedNonRelationalNumericalDomain[Interval]]
        analysis.analyze(entryState)
      } catch {
        case e:UnsupportedLanguageFeatureException =>
          SystemParameters.progressOutput.put("UNSUPPORTED: Unsupported Language Feature: "+e.toString)
          SystemParameters.progressOutput.reset()
        case e:Exception =>
          SystemParameters.progressOutput.put("ANALYSIS ERROR: Exception during analysis of "+file+": "+e.toString())
          for(line <- e.getStackTrace) SystemParameters.progressOutput.put(line.toString)
          SystemParameters.progressOutput.reset()
      }

    }

  }

}


object TouchApronRun {

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
        val domain = new Octagon()
        val numerical = new StringsAnd(new InvalidAnd(new ApronInterface(None, domain, env = Set.empty).factory()))
        val heapID = new SimpleProgramPointHeapIdentifier(null,SystemParameters.typ)

        val entryValue = new ExpressionSet(SystemParameters.typ.top())

        if (TouchAnalysisParameters.enableCollectionSummaryAnalysis) {
          val heapDomain = new NonRelationalSummaryCollectionHeapDomain[HeapId](heapID.getType(), new MaybeHeapIdSetDomain(), heapID)
          heapDomain.setParameter("UnsoundEntryState",false)

          val entryDomain = new HeapAndAnotherDomain[StringsAnd[InvalidAnd[ApronInterface]], NonRelationalSummaryCollectionHeapDomain[HeapId], HeapId](numerical, heapDomain)
          val entryState = new AbstractState[StringsAnd[InvalidAnd[ApronInterface]], NonRelationalSummaryCollectionHeapDomain[HeapId], HeapId](entryDomain, entryValue)

          val analysis = new TouchAnalysisWithApron[ApronInterface]
          analysis.analyze(entryState)
        }
        else if (TouchAnalysisParameters.enableCollectionMustAnalysis) {
          val mustHeapDomain: NonRelationalMustHeapDomain[HeapId] =
            new NonRelationalMustHeapDomain[HeapId](heapID.getType(), new TupleIdSetDomain(), heapID)

          val mayHeapDomain: NonRelationalHeapDomain[HeapId] =
            new NonRelationalHeapDomain[HeapId](heapID.getType(), new MaybeHeapIdSetDomain(), heapID)

          val heapDomain = new NonRelationalMayAndMustHeapDomain[HeapId](mayHeapDomain, mustHeapDomain)
          heapDomain.setParameter("UnsoundEntryState",false)

          val entryDomain = new HeapAndAnotherDomain[StringsAnd[InvalidAnd[ApronInterface]], NonRelationalMayAndMustHeapDomain[HeapId], HeapId](numerical, heapDomain)
          val entryState = new AbstractState[StringsAnd[InvalidAnd[ApronInterface]], NonRelationalMayAndMustHeapDomain[HeapId], HeapId](entryDomain, entryValue)

          val analysis = new TouchAnalysisWithApron[ApronInterface]
          analysis.analyze(entryState)
        } else {
          val heapDomain = new NonRelationalHeapDomain[HeapId](heapID.getType(), new MaybeHeapIdSetDomain(), heapID)
          heapDomain.setParameter("UnsoundEntryState",false)

          val entryDomain = new HeapAndAnotherDomain[StringsAnd[InvalidAnd[ApronInterface]], NonRelationalHeapDomain[HeapId], HeapId](numerical, heapDomain)
          val entryState = new AbstractState[StringsAnd[InvalidAnd[ApronInterface]], NonRelationalHeapDomain[HeapId], HeapId](entryDomain, entryValue)

          val analysis = new TouchAnalysisWithApron[ApronInterface]
          analysis.analyze(entryState)
        }
      } catch {
        case e:UnsupportedLanguageFeatureException =>
          SystemParameters.progressOutput.put("UNSUPPORTED: Unsupported Language Feature: "+e.toString)
          SystemParameters.progressOutput.reset()
        case e:Exception =>
          SystemParameters.progressOutput.put("ANALYSIS ERROR: Exception during analysis of "+file+": "+e.toString())
          for(line <- e.getStackTrace) SystemParameters.progressOutput.put(line.toString)
          SystemParameters.progressOutput.reset()
      }
    }
  }

}