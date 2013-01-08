package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.userinterfaces._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import apron.{Environment, Abstract1, Octagon}
import numericaldomain.{BoxedNonRelationalNumericalDomain, Interval, NonRelationalNumericalDomain, ApronInterface}
import ch.ethz.inf.pm.td.analysis.{TouchAnalysis, TouchAnalysisWithApron}

class TouchProperty extends ch.ethz.inf.pm.sample.property.Property {
  override def getLabel(): String = "Show graph"

  override def check[S <: State[S]](className: Type, methodName: String, result: ControlFlowGraphExecution[S], printer: OutputCollector): Unit =
    ShowGraph.check(className, methodName, result, printer)

  override def finalizeChecking(printer: OutputCollector): Unit = Unit

}

object TouchRun {

  type HeapId = ProgramPointHeapIdentifier

  def main(files: List[String]) {

    if(files.isEmpty) {
      println("No arguments given!")
      exit()
    }

    SystemParameters.compiler = new TouchCompiler
    SystemParameters.property = null

    SystemParameters.compiler.reset()
    SystemParameters.resetNativeMethodsSemantics()
    SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics())

    SystemParameters.analysisOutput = new TouchOutput()
    SystemParameters.progressOutput = new TouchOutput()

    ch.ethz.inf.pm.sample.Main.compile(files)

    //EntryState
    val numerical = new TouchDomain(new BoxedNonRelationalNumericalDomain(new Interval(0,0)))
    val heapID = new SimpleProgramPointHeapIdentifier(null,null)
    heapID.typ = SystemParameters.typ

    val heapDomain: NonRelationalHeapDomain[HeapId] =
      new NonRelationalHeapDomain[HeapId](heapID.getType(), new MaybeHeapIdSetDomain(), heapID)
    heapDomain.setParameter("UnsoundEntryState",false)

    val entryDomain =
      new HeapAndAnotherDomain[TouchDomain[BoxedNonRelationalNumericalDomain[Interval]], NonRelationalHeapDomain[HeapId], HeapId](numerical, heapDomain)

    val entryValue = new ExpressionSet(SystemParameters.typ.top())

    val entryState = new AbstractState[TouchDomain[BoxedNonRelationalNumericalDomain[Interval]], NonRelationalHeapDomain[HeapId], HeapId](entryDomain, entryValue)

    val analysis = new TouchAnalysis[TouchDomain[BoxedNonRelationalNumericalDomain[Interval]]]
    analysis.fixpointComputation(entryState, new OutputCollector)

  }

}


object TouchApronRun {

  type HeapId = ProgramPointHeapIdentifier

  def main(files: List[String]) {

    if(files.isEmpty) {
      println("No arguments given!")
      exit()
    }

    SystemParameters.compiler = new TouchCompiler
    SystemParameters.property = null

    SystemParameters.compiler.reset()
    SystemParameters.resetNativeMethodsSemantics()
    SystemParameters.addNativeMethodsSemantics(SystemParameters.compiler.getNativeMethodsSemantics())

    SystemParameters.analysisOutput = new TouchOutput()
    SystemParameters.progressOutput = new TouchOutput()

    ch.ethz.inf.pm.sample.Main.compile(files)

    //EntryState
    val domain = new Octagon()
    val numerical = new TouchDomain(new ApronInterface(new Abstract1(domain, new Environment()), domain))
    val heapID = new SimpleProgramPointHeapIdentifier(null,null)
    heapID.typ = SystemParameters.typ

    val heapDomain: NonRelationalHeapDomain[HeapId] =
      new NonRelationalHeapDomain[HeapId](heapID.getType(), new MaybeHeapIdSetDomain(), heapID)
    heapDomain.setParameter("UnsoundEntryState",false)

    val entryDomain =
      new HeapAndAnotherDomain[TouchDomain[ApronInterface], NonRelationalHeapDomain[HeapId], HeapId](numerical, heapDomain)

    val entryValue = new ExpressionSet(SystemParameters.typ.top())

    val entryState = new AbstractState[TouchDomain[ApronInterface], NonRelationalHeapDomain[HeapId], HeapId](entryDomain, entryValue)

    val analysis = new TouchAnalysisWithApron[TouchDomain[ApronInterface]]
    analysis.fixpointComputation(entryState, new OutputCollector)

  }

}

class TouchOutput extends ScreenOutput {
  def getString(): String = {
    ""
  }

  def appendString(s: String) = {
    println(s);
  }
}
