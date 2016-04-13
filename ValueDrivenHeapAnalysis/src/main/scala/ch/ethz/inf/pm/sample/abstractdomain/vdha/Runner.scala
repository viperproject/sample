/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, Identifier, SemanticDomain}
import ch.ethz.inf.pm.sample.execution.EntryStateBuilder

trait ValueDrivenHeapEntryStateBuilder[
E <: SemanticDomain[E], // Edge state domain
T <: ValueDrivenHeapState[E, T]]
  extends EntryStateBuilder[T] {

  protected def emptyApronState: Apron.Polyhedra =
    Apron.Polyhedra.Bottom.factory()

  protected def emptyHeapGraph: HeapGraph[E] =
    HeapGraph[E]()
}

object DefaultHeapEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  Apron.Polyhedra,
  ValueDrivenHeapState.Default[Apron.Polyhedra]] {

  def topState = {
    ValueDrivenHeapState.Default[Apron.Polyhedra](emptyHeapGraph, emptyApronState, ExpressionSet())
  }
}

object PreciseHeapEntryStateBuilder extends ValueDrivenHeapEntryStateBuilder[
  PreciseValueDrivenHeapState.EdgeStateDomain[Apron.Polyhedra],
  PreciseValueDrivenHeapState.Default[Apron.Polyhedra]] {

  def topState = {
    val generalValState = PreciseValueDrivenHeapState.makeTopEdgeState(emptyApronState)
    PreciseValueDrivenHeapState.Default(emptyHeapGraph, generalValState, ExpressionSet())
  }
}