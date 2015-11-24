package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, Identifier, SemanticDomain}
import ch.ethz.inf.pm.sample.execution.EntryStateBuilder

trait ValueDrivenHeapEntryStateBuilder[
E <: SemanticDomain[E], // Edge state domain
T <: ValueDrivenHeapState[E, T]]
  extends EntryStateBuilder[T] {

  protected def topApronInterface: Apron.Polyhedra = Apron.Polyhedra.Top.factory()

  protected def topHeapGraph: HeapGraph[E] =
    HeapGraph[E]()
}

object DefaultHeapEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  Apron.Polyhedra,
  ValueDrivenHeapState.Default[Apron.Polyhedra]] {

  def topState = {
    ValueDrivenHeapState.Default[Apron.Polyhedra](topHeapGraph, topApronInterface, ExpressionSet())
  }
}

object PreciseHeapEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  PreciseValueDrivenHeapState.EdgeStateDomain[Apron.Polyhedra],
  PreciseValueDrivenHeapState.Default[Apron.Polyhedra]] {

  def topState = {
    val generalValState = PreciseValueDrivenHeapState.makeTopEdgeState(topApronInterface)
    PreciseValueDrivenHeapState.Default(topHeapGraph, generalValState, ExpressionSet())
  }
}