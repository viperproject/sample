/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.domain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type

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
    * The type of the elements of the domain.
    */
  type T = HeapAndSemanticDomain[H, S, I]

  /**
    * A factory method that creates a domain element with the given underlying
    * element of the heap domain and element of the semantic domain.
    *
    * @param heap     The element of the heap domain.
    * @param semantic The element of the semantic domain.
    * @return An element of the domain.
    */
  def factory(heap: H, semantic: S): T =
    if (heap.isBottom || semantic.isBottom) HeapAndSemanticDomain(heap.bottom(), semantic.bottom())
    else HeapAndSemanticDomain(heap, semantic)

  /* ------------------------------------------------------------------------- *
   * LATTICE METHODS
   */

  override def factory(): T = top()

  override def top(): T = factory(heap.top(), semantic.top())

  override def bottom(): T = factory(heap.bottom(), semantic.bottom())

  override def lub(other: T): T = {
    val newHeap = heap lub other.heap
    val newSemantic = semantic lub other.semantic
    factory(newHeap, newSemantic)
  }

  override def glb(other: T): T = {
    val newHeap = heap glb other.heap
    val newSemantic = semantic glb other.semantic
    factory(newHeap, newSemantic)
  }

  override def widening(other: T): T = {
    val newHeap = heap widening other.heap
    val newSemantic = semantic widening other.semantic
    factory(newHeap, newSemantic)
  }

  override def lessEqual(other: T): Boolean =
    if (isBottom) true
    else if (other.isBottom) false
    else {
      val heapCompare = heap lessEqual other.heap
      val semanticCompare = semantic lessEqual other.semantic
      heapCompare && semanticCompare
    }

  override def isTop: Boolean = heap.isTop && semantic.isTop

  override def isBottom: Boolean = heap.isBottom || semantic.isBottom

  /* ------------------------------------------------------------------------- *
   * DOMAIN METHODS
   */

  def createVariable(variable: Identifier, typ: Type): T = {
    val (heap1, replacement1) = heap.createVariable(variable, typ)
    val semantic1 = semantic.merge(replacement1).createVariable(variable, typ)
    factory(heap1, semantic1)
  }

  def removeVariable(variable: Identifier): T = {
    val (heap1, replacement1) = heap.removeVariable(variable)
    val semantic1 = semantic.merge(replacement1).removeVariable(variable)
    factory(heap1, semantic1)
  }

  def assign(target: Identifier, value: Expression): T = {
    val (heap1, replacement1) = heap.assign(target, value)
    val semantic1 = semantic.merge(replacement1).assign(target, value)
    factory(heap1, semantic1)
  }

  def assignField(receiver: Identifier, field: String, value: Expression): T = {
    // update heap and get heap identifiers
    val (heap1, replacement1) = heap.assignField(receiver, field, value)
    val (idSet, heap2, replacement2) = heap1.getHeapIdentifiers(receiver, field)

    if (idSet.isTop) factory(heap2, semantic.top())
    else {
      val semantic1 = semantic.merge(replacement1).merge(replacement2)
      val idList = idSet.toSet.toList
      if (idList.isEmpty) factory(heap2, semantic1)
      else {
        val semantic2 = idList
          .map(semantic1.assign(_, value))
          .reduce(combinator)
        factory(heap2, semantic2)
      }
    }
  }

  def assume(condition: Expression): (T, Replacement) = condition match {
    case BinaryBooleanExpression(left, right, BooleanOperator.&&) =>
      val (domain1, replacement1) = assume(left)
      val (domain2, replacement2) = domain1.assume(right)
      (domain2, replacement1 ++ replacement2)
    case _ =>
      val (heap1, replacement1) = heap.assume(condition)
      val semantic1 = semantic.merge(replacement1).assume(condition)
      val domain1 = factory(heap1, semantic1)
      (domain1, replacement1)
  }

  /**
    * Combines two elements of the semantic domain.
    *
    * For now two elements are combined using the least upper bound.
    *
    * @param s1 The first element of the semantic domain.
    * @param s2 The second element of the semantic domain.
    * @return The combined element.
    */
  def combinator(s1: S, s2: S): S = s1 lub s2
}