package ch.ethz.inf.pm.td.domain


import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.analysis.{BottomVisitor, TouchAnalysis, TouchAnalysisWithApron}
import ch.ethz.inf.pm.td.compiler.UnsupportedLanguageFeatureException
import ch.ethz.inf.pm.sample._
import abstractdomain._
import abstractdomain.heapanalysis._
import property._
import oorepresentation.ControlFlowGraphExecution
import oorepresentation.MethodDeclaration
import oorepresentation.Type
import property.OutputCollector
import property.SingleStatementProperty
import userinterfaces.ShowGraph

import apron._
import heapanalysis.SimpleProgramPointHeapIdentifier
import numericaldomain.{BoxedNonRelationalNumericalDomain, Interval, ApronInterface}

class TouchProperty extends Property {
  override def getLabel(): String = "Show graph"

  override def check[S <: State[S]](className: Type, methodName: MethodDeclaration, result: ControlFlowGraphExecution[S], printer: OutputCollector): Unit =
    ShowGraph.check(className, methodName, result, printer)

  override def finalizeChecking(printer: OutputCollector): Unit = Unit

}

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
        analysis.analyze(Nil,entryState, new OutputCollector)
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
        val numerical = new StringsAnd(new InvalidAnd(new ApronInterface(None, domain).factory()))
        val heapID = new SimpleProgramPointHeapIdentifier(null,SystemParameters.typ)

        val heapDomain: NonRelationalHeapDomain[HeapId] =
          new NonRelationalHeapDomain[HeapId](heapID.getType(), new MaybeHeapIdSetDomain(), heapID)
        heapDomain.setParameter("UnsoundEntryState",false)

        val entryDomain =
          new HeapAndAnotherDomain[StringsAnd[InvalidAnd[ApronInterface]], NonRelationalHeapDomain[HeapId], HeapId](numerical, heapDomain)

        val entryValue = new ExpressionSet(SystemParameters.typ.top())

        val entryState = new AbstractState[StringsAnd[InvalidAnd[ApronInterface]], NonRelationalHeapDomain[HeapId], HeapId](entryDomain, entryValue)

        val analysis = new TouchAnalysisWithApron[ApronInterface]
        analysis.analyze(Nil, entryState, new OutputCollector)

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