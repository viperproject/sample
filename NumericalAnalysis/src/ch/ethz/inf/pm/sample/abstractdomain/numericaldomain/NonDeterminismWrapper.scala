/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._

/**
 *
 * Gives Non-Deterministic expression support to a relational domain.
 *
 * A non-deterministic expression is something like assume( x * (1 to 3) == 5 or 10 or 15 ) )
 *
 * @author Lucas Brutschy
 */
case class NonDeterminismWrapper[X <: NumericalDomain.Relational[X]](wrapped:X)
  extends NumericalDomain.Relational[NonDeterminismWrapper[X]]
  with NumericalDomain.Relational.Wrapper[X,NonDeterminismWrapper[X]] {

  override def assign(variable: Identifier, expr: Expression): NonDeterminismWrapper[X] = {
    wrapperFactory(nondeterminismWrapper(expr, wrapped, (someExpr, someState) => {
      someState.assign(variable,someExpr)
    }))
  }

  override def assume(expr: Expression): NonDeterminismWrapper[X] = {
    wrapperFactory(nondeterminismWrapper(expr, wrapped, (someExpr, someState) => {
      someState.assume(someExpr)
    }))
  }

  override def backwardAssign(oldPre: NonDeterminismWrapper[X], variable: Identifier, expr: Expression): NonDeterminismWrapper[X] = {

    // for non-deterministic expressions, we fall back to simply forgetting the variable
    if (!isDeterministicExpr(expr)) {
      return wrapperFactory(wrapped.setToTop(variable))
    }

    wrapperFactory(wrapped.backwardAssign(oldPre.wrapped,variable,expr))

  }

  protected def nondeterminismWrapper(expr: Expression, state: X, someFunc: (Expression, X) => X): X = {

    // Extract all non-deterministic expressions and store them in temporary variables
    var newState = state
    val (newExpr, tempAssigns) = removeNondeterminism("tmp", expr)

    // Add all temporary variables
    for ((id, _) <- tempAssigns) {
      newState = newState.createVariable(id)
    }

    for ((id, ndExpr) <- tempAssigns) {
      ndExpr.op match {
        case NondeterministicOperator.or =>
          val newStateLeft = newState.assign(id, ndExpr.left)
          val newStateRight = newState.assign(id, ndExpr.right)
          newState = newStateLeft.lub(newStateRight)
        case NondeterministicOperator.toExcl =>
          ndExpr.left match {
            case Constant("neginfty", _) => ()
            case _ => newState = newState.assume(BinaryArithmeticExpression(id, ndExpr.left, ArithmeticOperator.>=))
          }
          ndExpr.right match {
            case Constant("posinfty", _) => ()
            case _ => newState = newState.assume(BinaryArithmeticExpression(id, ndExpr.right, ArithmeticOperator.<))
          }
        case NondeterministicOperator.toIncl =>
          ndExpr.left match {
            case Constant("neginfty", _) => ()
            case _ => newState = newState.assume(BinaryArithmeticExpression(id, ndExpr.left, ArithmeticOperator.>=))
          }
          ndExpr.right match {
            case Constant("posinfty",_) => ()
            case _ => newState = newState.assume(BinaryArithmeticExpression(id, ndExpr.right, ArithmeticOperator.<=))
          }
      }
    }

    newState = someFunc(newExpr, newState)

    // Remove all temporary variables
    for ((id, _) <- tempAssigns) {
      newState = newState.removeVariable(id)
    }

    newState
  }

  private def removeNondeterminism(label: String, expr: Expression): (Expression, List[(Identifier, BinaryNondeterministicExpression)]) = {
    expr match {
      case BinaryArithmeticExpression(left, right, op) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        (BinaryArithmeticExpression(expL, expR, op), varL ::: varR)
      case BinaryBooleanExpression(left, right, op) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        (BinaryBooleanExpression(expL, expR, op), varL ::: varR)
      case ReferenceComparisonExpression(left, right, op) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        (ReferenceComparisonExpression(expL, expR, op), varL ::: varR)
      case NegatedBooleanExpression(left) =>
        val (expL, varL) = removeNondeterminism(label, left)
        (NegatedBooleanExpression(expL), varL)
      case UnaryArithmeticExpression(left, op, ret) =>
        val (expL, varL) = removeNondeterminism(label, left)
        (UnaryArithmeticExpression(expL, op, ret), varL)
      case BinaryNondeterministicExpression(left, right, op) =>
        val (expL, varL) = removeNondeterminism(label + "L", left)
        val (expR, varR) = removeNondeterminism(label + "R", right)
        val identifier = VariableIdentifier(label)(expr.typ, expr.pp)
        (identifier, varL ::: varR ::: List((identifier, BinaryNondeterministicExpression(expL, expR, op))))
      case x: Expression => (x, Nil)
    }
  }

  private def isDeterministicExpr(expr: Expression): Boolean = {
    expr match {
      case BinaryArithmeticExpression(left, right, op) =>
        isDeterministicExpr(left) && isDeterministicExpr(right)
      case BinaryBooleanExpression(left, right, op) =>
        isDeterministicExpr(left) && isDeterministicExpr(right)
      case ReferenceComparisonExpression(left, right, op) =>
        isDeterministicExpr(left) && isDeterministicExpr(right)
      case NegatedBooleanExpression(left) =>
        isDeterministicExpr(left)
      case UnaryArithmeticExpression(left, op, ret) =>
        isDeterministicExpr(left)
      case BinaryNondeterministicExpression(left, right, op) =>
        false
      case x: Expression => true
    }
  }

  override def wrapperFactory(wrapped: X): NonDeterminismWrapper[X] = NonDeterminismWrapper(wrapped)
}
