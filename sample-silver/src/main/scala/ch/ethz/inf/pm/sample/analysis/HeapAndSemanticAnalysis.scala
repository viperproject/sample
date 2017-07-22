/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.analysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.domain.{HeapAndSemanticDomain, HeapDomain}
import ch.ethz.inf.pm.sample.execution.SilverState
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.Expression
import com.typesafe.scalalogging.LazyLogging

/**
  * A state of an analysis that combines a heap domain with a semantic domain.
  *
  * This state essentially wraps an element of the heap and numerical domain
  * and adds all the state methods.
  *
  * @tparam T The type of the heap and semantic analysis state.
  * @tparam H The type of the elements of the heap domain.
  * @tparam S The type of the elements of the semantic domain.
  * @tparam I THe type of the identifiers used by the heap domain.
  * @author Jerome Dohrau
  */
trait HeapAndSemanticAnalysisState[T <: HeapAndSemanticAnalysisState[T, H, S, I], H <: HeapDomain[H, I], S <: SemanticDomain[S], I <: Identifier]
  extends SilverState[T]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {

  this: T =>

  type D = HeapAndSemanticDomain[H, S, I]

  /**
    * Returns the element of the heap and semantic domain.
    *
    * @return The element of the hap and semantic domain.
    */
  def domain: D

  /**
    * Returns the current program point.
    *
    * @return The current program point.
    */
  def pp: ProgramPoint

  /* ------------------------------------------------------------------------- *
   * LATTICE METHODS
   */

  override def factory(): T = top()

  override def top(): T = copy(domain = domain.top())

  override def bottom(): T = copy(domain = domain.bottom())

  override def lub(other: T): T = {
    logger.trace(s"lub($this, $other)")

    val newDomain = domain lub other.domain
    copy(domain = newDomain)
  }

  override def glb(other: T): T = {
    logger.trace(s"glb($this, other)")

    val newDomain = domain glb other.domain
    copy(domain = newDomain)
  }

  override def widening(other: T): T = {
    logger.trace(s"widening($this, other)")

    val newDomain = domain widening other.domain
    copy(domain = newDomain)
  }

  override def lessEqual(other: T): Boolean = {
    logger.trace(s"lessEqual($this, other)")

    domain lessEqual other.domain
  }

  override def isBottom: Boolean = domain.isBottom

  override def isTop: Boolean = domain.isTop

  /* ------------------------------------------------------------------------- *
   * SILVER STATE METHODS
   */

  override def inhale(expression: Expression): T = ???

  override def exhale(expression: Expression): T = ???

  /* ------------------------------------------------------------------------- *
   * SIMPLE STATE METHODS
   */

  override def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): T = ???

  override def createVariableForArgument(variable: VariableIdentifier, typ: Type): T = ???

  override def assignVariable(target: Expression, right: Expression): T = ???

  override def assignField(target: Expression, field: String, right: Expression): T = ???

  override def setVariableToTop(variable: Expression): T = ???

  override def removeVariable(variable: VariableIdentifier): T = ???

  override def getFieldValue(target: Expression, field: String, typ: Type): T = ???

  override def assume(condition: Expression): T = ???

  /* ------------------------------------------------------------------------- *
   * STATE METHODS
   */

  override def before(pp: ProgramPoint): T = ???

  override def createObject(typ: Type, pp: ProgramPoint): T = ???

  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = ???

  override def expr: ExpressionSet = ???

  override def getVariableValue(id: Identifier): T = ???

  override def pruneUnreachableHeap(): T = ???

  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = ???

  override def removeExpression(): T = ???

  override def setArgument(x: ExpressionSet, right: ExpressionSet): T = ???

  override def setExpression(expr: ExpressionSet): T = ???

  override def throws(t: ExpressionSet): T = ???

  override def ids: IdentifierSet = ???

  /* ------------------------------------------------------------------------- *
   * HEAP AND SEMANTIC ANALYSIS STATE METHODS
   */

  /**
    * Copies the state and updates the domain, expression set, and program point
    * if the corresponding arguments are defined.
    *
    * @param domain The heap and semantic domain.
    * @param expr   The expression set.
    * @param pp     The program point.
    * @return The updated state.
    */
  def copy(domain: D = domain,
           expr: ExpressionSet = expr,
           pp: ProgramPoint = pp): T
}