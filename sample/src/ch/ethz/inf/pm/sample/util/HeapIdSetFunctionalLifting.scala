package ch.ethz.inf.pm.sample.util

import ch.ethz.inf.pm.sample.abstractdomain._

/**
 * Created by IntelliJ IDEA.
 * User: Pietro
 * Date: 25/11/11
 * Time: 12.32
 * To change this template use File | Settings | File Templates.
 */

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
      rep2 = rep2.lub(rep2, rep1)
      state match {
        case None => state = Some(newHeap2);
        case Some(s) =>
          val (s1, rep3) = createdLocation.heapCombinator(s, newHeap2, result._1, result._1);
          state = Some(s1);
          rep2 = rep2.lub(rep2, rep3);
      }
    }
    if(state==None || ids == None) throw new SemanticException("We should have at least one abstract id in the HeapIdSetDomain");
    (ids.get, state.get, rep2)
  }

  def applyToSetHeapId[T <: Lattice[T], I <: HeapIdentifier[I]](fact: T, ids : HeapIdSetDomain[I], f: Assignable => T) : T = {
      var result : Option[T] = None;
      for(id <- ids.value)
        result match {
          case None => result=Some(f(id));
          case Some(s) => result=Some(ids.combinator(s, f(id)))
        }
      result match {
        case None => fact.bottom()
        case Some(s) => s
      }
    }

}