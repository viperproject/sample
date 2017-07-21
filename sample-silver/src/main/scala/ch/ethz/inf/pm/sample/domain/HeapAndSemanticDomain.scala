/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.domain

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, Lattice, SemanticDomain}

/**
  * A domain that combines a heap domain and a semantic domain.
  *
  * @param heap     The element of the heap domain.
  * @param semantic The element of the semantic domain.
  * @tparam H The type of the elements of the heap domain.
  * @tparam S The type of the elements of the semantic domain.
  * @tparam I The type of the identifiers used by the heap domain.
  * @author Jerome Dohrau
  */
case class HeapAndSemanticDomain[H <: HeapDomain[H, I], S <: SemanticDomain[S], I <: Identifier](heap: H, semantic: S)
  extends Lattice[HeapAndSemanticDomain[H, S, I]] {

  /**
    * The type oef the elements of the domain.
    */
  type T = HeapAndSemanticDomain[H, S, I]

  /**
    * A factory method that creates a domain element with the given underlying
    * elements of the heap domain and semantic domain.
    *
    * @param heap     The element of the heap domain.
    * @param semantic The element of the semantic domain.
    * @return The element of the domain.
    */
  def factory(heap: H, semantic: S): T =
    if (heap.isBottom || semantic.isBottom) HeapAndSemanticDomain(heap.bottom(), semantic.bottom())
    else HeapAndSemanticDomain(heap, semantic)

  def copy(heap: H = heap, semantic: S = semantic): T = factory(heap, semantic)

  /* ------------------------------------------------------------------------- *
   * LATTICE METHODS
   */

  override def factory(): HeapAndSemanticDomain[H, S, I] = top()

  override def top(): HeapAndSemanticDomain[H, S, I] = factory(heap.top(), semantic.top())

  override def bottom(): HeapAndSemanticDomain[H, S, I] = factory(heap.bottom(), semantic.bottom())

  override def isTop: Boolean = heap.isTop && semantic.isTop

  override def isBottom: Boolean = heap.isBottom || semantic.isBottom

  override def lub(other: T): T = {
    val newHeap = heap lub other.heap
    val newSemantic = semantic lub other.semantic
    copy(heap = newHeap, semantic = newSemantic)
  }

  override def glb(other: T): T = {
    val newHeap = heap glb other.heap
    val newSemantic = semantic glb other.semantic
    copy(heap = newHeap, semantic = newSemantic)
  }

  override def widening(other: T): T = {
    val newHeap = heap widening other.heap
    val newSemantic = semantic widening other.semantic
    copy(heap = newHeap, semantic = newSemantic)
  }

  override def lessEqual(other: T): Boolean =
    if (isBottom) true
    else if (other.isBottom) false
    else (heap lessEqual other.heap) && (semantic lessEqual other.semantic)

}
