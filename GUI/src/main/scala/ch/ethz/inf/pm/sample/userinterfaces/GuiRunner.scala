package ch.ethz.inf.pm.sample.userinterfaces

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.property.OutputCollector

import scala.collection.immutable.List

/**
 * This is used to bridge the type-checking between Analyses and ShowGraph (both written in Scala)
 * to be used from inside Java code
 */
object GuiRunner {

  def run[S <: State[S]](a: Analysis, methods: List[String], state: S, o: OutputCollector) {
    ShowGraph.Show[S](a.analyze(methods, state, o))
  }

  def createEmptyValue(): ExpressionSet = new ExpressionSet(SystemParameters.getType.top, SetDomain.Default())

  def createNonRelationalMayHeapDomain[I <: NonRelationalHeapIdentifier[I]](id: I): NonRelationalHeapDomain[I] = {
    val ids: MayHeapSetDomain[I] = new MayHeapSetDomain[I]
    val env: VariableEnv[I] = new VariableEnv[I](ids)
    val heap: HeapEnv[I] = new HeapEnv[I](ids)
    new NonRelationalHeapDomain[I](env, heap, ids, id)
  }

  def createNonRelationalMayAndMustHeapDomain[I <: NonRelationalHeapIdentifier[I]](id: I): NonRelationalMayAndMustHeapDomain[I] = {
    val mayHeap: NonRelationalHeapDomain[I] = createNonRelationalMayHeapDomain(id)
    val mustIds: TupleIdSetDomain[I] = new TupleIdSetDomain[I]
    val mustEnv: VariableEnv[I] = new VariableEnv[I](mustIds)
    val mustHeapEnv: HeapEnv[I] = new HeapEnv[I](mustIds)
    val mustHeap: NonRelationalMustHeapDomain[I] = new NonRelationalMustHeapDomain[I](mustEnv, mustHeapEnv, mustIds, id)
    new NonRelationalMayAndMustHeapDomain[I](mayHeap, mustHeap)
  }

  def createNonRelationalSummaryCollectionHeapDomain[I <: NonRelationalHeapIdentifier[I]](id: I): NonRelationalSummaryCollectionHeapDomain[I] = {
    val ids: MayHeapSetDomain[I] = new MayHeapSetDomain[I]
    val env: VariableEnv[I] = new VariableEnv[I](ids)
    val heap: HeapEnv[I] = new HeapEnv[I](ids)
    new NonRelationalSummaryCollectionHeapDomain[I](env, heap, ids, id)
  }

}