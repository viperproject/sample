/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.util

import ch.ethz.inf.pm.sample.abstractdomain._

object HeapIdSetFunctionalLifting {


  def applyGetFieldId[N <: SemanticDomain[N], I <: HeapIdentifier[I], H <: HeapDomain[H, I]](createdLocation: HeapIdSetDomain[I], result : HeapAndAnotherDomain[N, H, I], f : Assignable => (HeapIdSetDomain[I], H, Replacement)): (HeapIdSetDomain[I], H, Replacement) = {
    var ids: Option[HeapIdSetDomain[I]] = None
    var state: Option[H] = None
    var rep2: Replacement = new Replacement

    for (id <- createdLocation.value) {
      val (address, newHeap2, rep1) = f(id)
      //result2._2.getFieldIdentifier(id, field.getName(), field.getType(), field.getProgramPoint());
      ids match {
        case None => ids = Some(address);
        case Some(s) => ids = Some(createdLocation.combinator(s, address));
      }
      rep2 = rep2.lub(rep1)
      state match {
        case None => state = Some(newHeap2);
        case Some(s) =>
          val (s1, rep3) = createdLocation.heapCombinator(s, newHeap2, result._1, result._1)
          state = Some(s1)
          rep2 = rep2.lub(rep3);
      }
    }
    if(state.isEmpty || ids.isEmpty) throw new SemanticException("We should have at least one abstract id in the HeapIdSetDomain")
    (ids.get, state.get, rep2)
  }

  /**
   * Applies a function to a set of heap identifiers, combining (reducing) the result with the HeapIdSetDomain's
   * combinator, from left to right. An empty heap id set yields bottom.
   *
   * @param latticeFactory a dummy lattice element used to retrieve bottom
   * @param ids the set of heap ids to be transformed
   * @param f transformer taking a heap id, yielding a lattice element
   * @tparam T resulting lattice element type
   * @tparam I heap identifier type of ids in set
   * @return lattice element resulting from reduction
   */
  def applyToSetHeapId[T <: Lattice[T], I <: HeapIdentifier[I]](latticeFactory: T, ids : HeapIdSetDomain[I], f: Assignable => T) : T = {
    val bottom = latticeFactory.bottom()
    val idSet = ids.value
    idSet.map(f)
      .reduceLeftOption(ids.combinator)
      .getOrElse(bottom)
  }

}