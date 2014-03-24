package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, Identifier, SemanticDomain}
import ch.ethz.inf.pm.sample.execution.EntryStateBuilder
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import apron.Polka

trait ValueDrivenHeapEntryStateBuilder[
    E <: SemanticDomain[E], // Edge state domain
    T <: ValueDrivenHeapState[E, T]]
  extends EntryStateBuilder[T] {

  protected def topApronInterface: ApronInterface.Default =
    ApronInterface.Default(None, new Polka(false), env = Set.empty[Identifier]).top()

  protected def topHeapGraph: HeapGraph[E] =
    HeapGraph[E]()
}

object DefaultHeapEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
    ApronInterface.Default,
    ValueDrivenHeapState.Default[ApronInterface.Default]] {

  def topState = {
    ValueDrivenHeapState.Default[ApronInterface.Default](topHeapGraph, topApronInterface, ExpressionSet())
  }
}

object PreciseHeapEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  PreciseValueDrivenHeapState.EdgeStateDomain[ApronInterface.Default],
  PreciseValueDrivenHeapState.Default[ApronInterface.Default]] {

  def topState = {
    val generalValState = PreciseValueDrivenHeapState.makeTopEdgeState(topApronInterface)
    PreciseValueDrivenHeapState.Default(topHeapGraph, generalValState, ExpressionSet())
  }
}