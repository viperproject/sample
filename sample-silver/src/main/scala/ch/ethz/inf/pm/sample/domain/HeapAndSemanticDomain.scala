/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.domain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}

/**
  * An identifier representing a field.
  *
  * @param receiver The receiver of the field.
  * @param field    The field.
  * @author Jerome Dohrau
  */
case class FieldIdentifier(receiver: Identifier, field: Identifier)
  extends Identifier {

  override def getName: String = s"$receiver.$field"

  override def getField: Option[String] = Some(field.getName)

  override def representsSingleVariable: Boolean = receiver.representsSingleVariable

  override def pp: ProgramPoint = field.pp

  override def typ: Type = field.typ
}

/**
  * A domain that combines a heap domain and a semantic domain.
  *
  * @param heap     The element of the heap domain.
  * @param semantic The element of the semantic domain.
  * @param fields   The fields.
  * @tparam H The type of the elements of the heap domain.
  * @tparam S The type of the elements of the semantic domain.
  * @tparam I The type of the identifiers used by the heap domain.
  * @author Jerome Dohrau
  */
case class HeapAndSemanticDomain[H <: HeapDomain[H, I], S <: SemanticDomain[S], I <: Identifier](heap: H, semantic: S, fields: Seq[Identifier])
  extends Lattice[HeapAndSemanticDomain[H, S, I]] {

  /**
    * The type oef the elements of the domain.
    */
  type T = HeapAndSemanticDomain[H, S, I]

  /**
    * Creates an element of the heap and semantic domain with the given fields.
    *
    * @param fields The fields.
    * @return
    */
  def factory(fields: Seq[Identifier]): T = {
    // initialize heap domain
    val newHeap = heap.factory(fields)
    // initialize semantic domain
    val locations = newHeap.locations
    val variables = locations.flatMap { receiver => fields.map { field => FieldIdentifier(receiver, field): Identifier } }
    val newSemantic = variables.foldLeft(semantic.factory()) {
      case (semantic, variable) => semantic.createVariable(variable)
    }

    copy(heap = newHeap, semantic = newSemantic, fields = fields)
  }

  /**
    * A factory method that creates a domain element with the given underlying
    * elements of the heap domain and semantic domain.
    *
    * @param heap     The element of the heap domain.
    * @param semantic The element of the semantic domain.
    * @return The element of the domain.
    */
  def factory(heap: H, semantic: S, fields: Seq[Identifier]): T =
    if (heap.isBottom || semantic.isBottom) HeapAndSemanticDomain(heap.bottom(), semantic.bottom(), Seq.empty)
    else if (heap.isTop && semantic.isTop) HeapAndSemanticDomain(heap, semantic, Seq.empty)
    else HeapAndSemanticDomain(heap, semantic, fields)

  def copy(heap: H = heap,
           semantic: S = semantic,
           fields: Seq[Identifier] = fields): T =
    factory(heap, semantic, fields)

  /* ------------------------------------------------------------------------- *
   * LATTICE METHODS
   */

  override def factory(): HeapAndSemanticDomain[H, S, I] = copy(
    heap = heap.factory(),
    semantic = semantic.factory(),
    fields = Seq.empty
  )

  override def top(): HeapAndSemanticDomain[H, S, I] = copy(
    heap = heap.top(),
    semantic = semantic.top()
  )

  override def bottom(): HeapAndSemanticDomain[H, S, I] = copy(
    heap = heap.bottom(),
    semantic = semantic.bottom()
  )

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

  /* ------------------------------------------------------------------------- *
   * HEAP AND SEMANTIC DOMAIN METHODS
   */

  /**
    * Performs an abstract garbage collection by pruning all unreachable heap
    * locations and then removes identifiers from the semantic domain if
    * necessary.
    *
    * @return The resulting domain.
    */
  def garbageCollect(): T = {
    // TODO: Substitute
    val (newHeap, substitution) = heap.garbageCollect()
    copy(heap = newHeap)
  }
}
