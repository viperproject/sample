/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain


import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._
import util.HeapIdSetFunctionalLifting

/**
 * A domain that combines a heap and another semantic domain.
 * The intuition is that the heap domain takes care of approximating the heap structure, while the
 * semantic domain has to manage the information of its interest without taking care of field accesses
 * and object creation, but dealing only with identifiers (of variables or of heap nodes).
 *
 * TODO:
 * Rewrite most methods. Contains lots of (cosmetically cleaned up) legacy code. Especially,
 * remove the timing code as it is a separate concern (use adapter to wrap domains?)
 */
case class HeapAndAnotherDomain[
N <: SemanticDomain[N],
H <: HeapDomain[H, I],
I <: HeapIdentifier[I]](
                         semantic: N,
                         heap: H)
  extends Lattice[HeapAndAnotherDomain[N, H, I]]
  with LatticeWithReplacement[HeapAndAnotherDomain[N, H, I]] {


  type T = HeapAndAnotherDomain[N, H, I]

  override def toString: String = "Heap state:\n" +
    ToStringUtilities.indent(heap.toString) +
    "\nSemantic state:\n" +
    ToStringUtilities.indent(semantic.toString)

  def _1 = semantic

  def _2 = heap

  def getStringOfId(id: Identifier): String = semantic.getStringOfId(id)

  def isBottom = _1.isBottom || _2.isBottom

  def isTop = _1.isTop && _2.isTop

  def ids = _1.ids ++ _2.ids

  def factory(semantic: N, heap: H): T = HeapAndAnotherDomain[N, H, I](semantic, heap)

  def factory(): T = top()

  def createVariableForArgument(variable: Assignable, typ: Type, path: List[String]) = {
    val (newHeap, ids, r) = heap.createVariableForArgument(variable, typ, path)
    var newSemantic = semantic
    newSemantic = applyToAssignable[N](variable, newSemantic, _.createVariableForArgument(_, typ, path)._1)
    variable match {
      case x: VariableIdentifier =>
        newSemantic = newSemantic.createVariableForArgument(x, typ, path)._1
      case x: HeapIdSetDomain[I] =>
        var first: Boolean = true
        for (singleid <- x.value)
          if (first) {
            first = false
            newSemantic = newSemantic.createVariableForArgument(singleid, typ, path)._1
          }
          else
            newSemantic = x.combinator(newSemantic, newSemantic.createVariableForArgument(singleid, typ, path)._1)
    }
    //We recursively create the entry state for all the entry abstract nodes.
    newSemantic = newSemantic.merge(r)
    for (id <- ids.keys)
      if (!id.equals(variable))
        newSemantic = newSemantic.createVariableForArgument(id, typ, ids.apply(id))._1
    (factory(newSemantic, newHeap), ids)
  }

  def setToTop(variable: Assignable): T = {
    val (newHeap, r) = heap.setToTop(variable)
    var newSemantic = semantic.merge(r)
    newSemantic = applyToAssignable[N](variable, newSemantic, _.setToTop(_))
    factory(newSemantic, newHeap)
  }

  def assign(variable: Assignable, expr: Expression): T = {
    val (newHeap, r) = heap.assign(variable, expr, semantic)
    val (newHeap2, r1) = newHeap.endOfAssignment()
    var newSemantic = semantic.merge(r).merge(r1)
    newSemantic = applyToAssignable[N](variable, newSemantic, _.assign(_, expr))
    factory(newSemantic, newHeap2)
  }

  def assignField(variable: Assignable, field: String, expr: Expression, typ: Type, pp: ProgramPoint): T = {
    val (newHeap, r2) = heap.assignField(variable, field, expr)
    val (id, newHeap2, r1) = newHeap.getFieldIdentifier(variable, field, typ, pp)
    val (newHeap3, r3) = newHeap2.endOfAssignment()
    val newSemantic = semantic.merge(r2).merge(r1).merge(r3)
    var newSemanticOpt: Option[N] = None
    if (id.isTop)
      newSemanticOpt = Some(newSemantic.top())
    else
      for (singleheapid <- id.value) {
        if (newSemanticOpt.isEmpty)
          newSemanticOpt = Some(newSemantic.assign(singleheapid, expr))
        else newSemanticOpt = Some(id.combinator(newSemanticOpt.get, newSemantic.assign(singleheapid, expr)))
      }
    val newSemanticResult =
      if (newSemanticOpt.isDefined)
        newSemanticOpt.get //throw new SemanticException("You should assign to something")
      else newSemantic
    factory(newSemanticResult, newHeap3)
  }

  def backwardAssignField(oldPreState: T, variable: Assignable, field: String, expr: Expression, typ: Type, pp: ProgramPoint): T = {
    val (newHeap, r2) = heap.backwardAssignField(oldPreState.heap, variable, field, expr)
    val (id, newHeap2, r1) = newHeap.getFieldIdentifier(variable, field, typ, pp)
    val (newHeap3, r3) = newHeap2.endOfAssignment()
    val newSemantic = semantic.merge(r2).merge(r1).merge(r3)
    var newSemanticOpt: Option[N] = None
    if (id.isTop)
      newSemanticOpt = Some(newSemantic.top())
    else
      for (singleheapid <- id.value) {
        if (newSemanticOpt.isEmpty)
          newSemanticOpt = Some(newSemantic.backwardAssign(oldPreState.semantic, singleheapid, expr))
        else newSemanticOpt = Some(id.combinator(newSemanticOpt.get, newSemantic.backwardAssign(oldPreState.semantic, singleheapid, expr)))
      }
    val newSemanticResult = newSemanticOpt.getOrElse(newSemantic)
    factory(newSemanticResult, newHeap3)
  }


  private def assignSemanticValue(ids: HeapIdSetDomain[I], value: Expression, initial: N): N = {
    def assignValueTo(initialState: N, value: Expression)(a: Assignable) = {
      applyToAssignable[N](a, initialState, _.assign(_, value))
    }

    if (ids.isTop) {
      initial.top()
    } else {
      HeapIdSetFunctionalLifting.applyToSetHeapId(initial.factory(), ids, assignValueTo(initial, value))
    }
  }

  def setArgument(variable: Assignable, expr: Expression): T = {
    val (newHeap, r) = heap.setArgument(variable, expr)
    var newSemantic = semantic.merge(r)
    newSemantic = applyToAssignable[N](variable, newSemantic, _.setArgument(_, expr))
    factory(newSemantic, newHeap)
  }

  def assume(expr: Expression): (T, Replacement) = expr match {
    case BinaryBooleanExpression(_, _, BooleanOperator.&&) =>
      val binaryBoolExpr = expr.asInstanceOf[BinaryBooleanExpression]
      val (result, rep1) = this.assume(binaryBoolExpr.left)
      val (result2, rep2) = result.assume(binaryBoolExpr.right)
      (result2, rep1 ++ rep2)
    case _ =>
      val (newHeap, r) = heap.assume(expr)
      val newSemantic = semantic.merge(r).assume(expr)
      (factory(newSemantic, newHeap), r)
  }

  def createVariable(variable: Assignable, typ: Type): T = {
    val (newHeap, r) = heap.createVariable(variable, typ)
    val newSemantic = applyToAssignable[N](variable, semantic.merge(r), _.createVariable(_, typ))
    factory(newSemantic, newHeap)
  }

  def removeVariable(variable: Assignable): T = {
    val (newHeap, r) = heap.removeVariable(variable)
    val newSemantic = applyToAssignable[N](variable, semantic.merge(r), _.removeVariable(_))
    factory(newSemantic, newHeap)
  }
  
  def backwardAssign(oldPreState: T, variable: Assignable, expr: Expression): T = {
    val (newHeap, r) = heap.backwardAssign(oldPreState.heap, variable, expr)
    val (newHeap2, _) = oldPreState.heap.glbWithReplacement(newHeap)
    var newSemantic = semantic.merge(r)
    newSemantic = applyToAssignable[N](variable, newSemantic, _.backwardAssign(oldPreState.semantic, _, expr))
    factory(newSemantic, newHeap2)
  }

  override def top(): T = factory(semantic.top(), heap.bottom())

  override def bottom(): T = factory(semantic.bottom(), heap.bottom())

  override def lubWithReplacement(other: T): (T, Replacement) = {
    val (newHeap, rep) = heap.lubWithReplacement(other.heap)
    val newSemantic = semantic.merge(rep).lub(other.semantic.merge(rep))
    (factory(newSemantic, newHeap), rep)
  }

  override def lub(other: T): T = lubWithReplacement(other)._1

  override def glbWithReplacement(other: T): (T, Replacement) = {
    val (newHeap, rep) = heap.glbWithReplacement(other.heap)
    val newSemantic = semantic.merge(rep).glb(other.semantic.merge(rep))
    (factory(newSemantic, newHeap), rep)
  }

  override def glb(other: T): T = glbWithReplacement(other)._1

  override def wideningWithReplacement(other: T): (T, Replacement) = {
    val (newHeap, rep) = heap.wideningWithReplacement(other.heap)
    val newSemantic = semantic.merge(rep).widening(other.semantic.merge(rep))
    (factory(newSemantic, newHeap), rep)
  }

  override def widening(other: T): T = wideningWithReplacement(other)._1

  override def lessEqual(other: T): Boolean = {
    if (semantic.lessEqual(semantic.bottom())) return true
    if (other.semantic.lessEqual(other.semantic.bottom())) return false
    var b = heap.lessEqual(other.heap)
    if (!b) return false
    b = semantic.lessEqual(other.semantic)
    b
  }

  def explainError(expr: Expression): Set[(String, ProgramPoint)] = _1.explainError(expr) ++ _2.explainError(expr)

  private def applyToAssignable[L <: Lattice[L]](variable: Assignable, state: L, functor: (L, Identifier) => L): L = {
    variable match {
      case x: VariableIdentifier => functor(state, x)
      case x: I @unchecked => functor(state, x): L
    }
  }


}