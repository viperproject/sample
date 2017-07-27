/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.domain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.domain.util.Substitution
import ch.ethz.inf.pm.sample.oorepresentation.silver.RefType
import ch.ethz.inf.pm.sample.oorepresentation._

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
    val identifiers = locations.flatMap { location => fields.map { field => FieldIdentifier(location, field): Identifier } }
    val newSemantic = identifiers.foldLeft(semantic.factory()) { case (domain, variable) => domain.createVariable(variable) }
    // return domain element
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

  override def lub(other: T): T =
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else {
      val newHeap = heap lub other.heap
      val newSemantic = semantic lub other.semantic
      copy(heap = newHeap, semantic = newSemantic)
    }

  override def glb(other: T): T =
    if (isBottom || other.isTop) this
    else if (isTop || other.isBottom) other
    else {
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
    * Adds the given variable to the domain.
    *
    * @param variable The variable to add.
    * @return The resulting domain.
    */
  def createVariable(variable: VariableIdentifier): T = {
    val (newHeap, substitution) = heap.createVariable(variable)
    val newSemantic = substitutedSemantic(substitution).createVariable(variable)
    copy(heap = newHeap, semantic = newSemantic)
  }

  /**
    * Removes the given variable from the domain.
    *
    * @param variable The variable to remove.
    * @return The resulting domain.
    */
  def removeVariable(variable: VariableIdentifier): T = {
    val (newHeap, substitution) = heap.removeVariable(variable)
    val newSemantic = substitutedSemantic(substitution).removeVariable(variable)
    copy(heap = newHeap, semantic = newSemantic)
  }

  /**
    * Performs a variable assignment of the given expression to the given
    * variable.
    *
    * @param variable   The variable.
    * @param expression The expression.
    * @return The resulting domain.
    */
  def assignVariable(variable: VariableIdentifier, expression: Expression): T = {
    // perform assignment in heap domain
    val (newHeap, substitution) = heap.assignVariable(variable, expression)
    val newSemantic = substitutedSemantic(substitution)
    val domain = copy(heap = newHeap, semantic = newSemantic)
    // perform assignment in semantic domain
    val left = Set(variable: Identifier)
    val right = domain.withFieldIdentifiers(expression)
    domain.assignSemantic(left, right)
  }

  /**
    * Performs a field assignment of the given expression to the given target.
    *
    * @param target     The target.
    * @param expression The expression.
    * @return The resulting domain.
    */
  def assignField(target: AccessPathIdentifier, expression: Expression): T = {
    // get receiver and field
    val receiver = AccessPathIdentifier(target.path.init)
    val field = target.path.last
    // perform assignment in heap domain
    val (newHeap, substitution) = heap.assignField(target, expression)
    val newSemantic = substitutedSemantic(substitution)
    val domain = copy(heap = newHeap, semantic = newSemantic)
    // perform assignment in semantic domain
    val receivers = newHeap.getValue(receiver)
    val left = receivers.map { receiver => FieldIdentifier(receiver, field): Identifier }
    val right = domain.withFieldIdentifiers(expression)
    domain.assignSemantic(left, right)
  }

  /**
    * Inhales the given condition.
    *
    * @param condition The condition to inhaled.
    * @return The resulting domain.
    */
  def inhale(condition: Expression): T = {
    val (newHeap, substitution) = heap.inhale(condition)
    val newSemantic = substitutedSemantic(substitution).assume(condition)
    copy(heap = newHeap, semantic = newSemantic)
  }

  /**
    * Exhales the given condition.
    *
    * @param condition The condition to exhale.
    * @return The resulting domain.
    */
  def exhale(condition: Expression): T = {
    val (newHeap, substitution) = heap.exhale(condition)
    val newSemantic = substitutedSemantic(substitution)
    copy(heap = newHeap, semantic = newSemantic)
  }

  /**
    * Performs an abstract garbage collection by pruning all unreachable heap
    * locations and then removes identifiers from the semantic domain if
    * necessary.
    *
    * @return The resulting domain.
    */
  def garbageCollect(): T = {
    val (newHeap, substitution) = heap.garbageCollect()
    val newSemantic = substitutedSemantic(substitution)
    copy(heap = newHeap, semantic = newSemantic)
  }

  /**
    * Returns the set of identifiers contained in the domain.
    *
    * @return The set of identifiers.
    */
  def ids = semantic.ids

  /**
    * Returns a set of possible constraints.
    *
    * @param ids The identifiers that should be mentioned in the constraints.
    * @return The set of constraints.
    */
  def getConstraints(ids: Set[Identifier]): Set[Expression] =
    semantic.getConstraints(ids).map(withoutFieldIdentifiers)

  /**
    * Applies the given substitution to the semantic domain and returns the
    * result.
    *
    * @param substitution The substitution to apply.
    * @return The substituted semantic domain.
    */
  private def substitutedSemantic(substitution: Substitution): S =
    substitution.extend(fields)(semantic)

  /**
    * A helper method that performs an assignment in the semantic domain.
    *
    * @param left  The set representing all possible targets.
    * @param right The set representing all possible values.
    * @return The resulting domain.
    */
  private def assignSemantic(left: Set[Identifier], right: Set[Expression]): T =
    if (left.isEmpty && right.isEmpty) bottom()
    else {
      val results = for (variable <- left; value <- right) yield semantic.assign(variable, value)
      val newSemantic = results.reduce(_ lub _)
      copy(semantic = newSemantic)
    }

  /**
    * Replaces all access path identifiers in the given right with their
    * corresponding field identifiers.
    *
    * TODO: Implement properly.
    */
  private def withFieldIdentifiers(expression: Expression): Set[Expression] = expression match {
    case _: Constant |
         _: VariableIdentifier =>
      Set(expression)
    case BinaryArithmeticExpression(left, right, operator) =>
      val leftSet = withFieldIdentifiers(left)
      val rightSet = withFieldIdentifiers(right)
      for (newLeft <- leftSet; newRight <- rightSet) yield BinaryArithmeticExpression(newLeft, newRight, operator)
    case AccessPathIdentifier(path) =>
      if (path.length == 1) Set(path.head)
      else {
        val receiver = AccessPathIdentifier(path.init)
        val field = path.last
        val receivers = heap.getValue(receiver)
        receivers.map { receiver => FieldIdentifier(receiver, field) }
      }
    case _ => ???
  }

  /**
    * TODO: Implement properly.
    */
  private def withoutFieldIdentifiers(expression: Expression): Expression = expression match {
    case _: Constant |
         _: VariableIdentifier |
         _: AccessPathIdentifier =>
      expression
    case BinaryArithmeticExpression(left, right, operator) =>
      val newLeft = withoutFieldIdentifiers(left)
      val newRight = withoutFieldIdentifiers(right)
      BinaryArithmeticExpression(newLeft, newRight, operator)
    case FieldIdentifier(receiver, field) =>
      val init = receiver.getName
        .split("\\.").toList
        .map(x => VariableIdentifier(x)(RefType()))
      val path = init :+ field
      AccessPathIdentifier(path)
    case _ => ???
  }

}
