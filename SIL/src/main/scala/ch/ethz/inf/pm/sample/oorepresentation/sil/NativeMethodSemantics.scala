/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import scala.Some
import ch.ethz.inf.pm.sample.reporting.Reporter

/** Native method semantic for arithmetic and boolean operators */
object ArithmeticAndBooleanNativeMethodSemantics extends ForwardNativeMethodSemantics {

  import ExpressionFactory._

  def applyForwardNativeSemantics[S <: State[S]](
      leftExp: ExpressionSet,
      op: String,
      parameters: List[ExpressionSet],
      typeParameters: List[Type],
      returnType: Type,
      programPoint: ProgramPoint,
      state: S): Option[S] = {
    val exprSet = (leftExp.getType(), op, parameters) match {
      case (IntType, "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "\\" | "%", _ :: Nil) |
           (BoolType, "==" | "!=", _ :: Nil) =>
        val arithmeticOp = ArithmeticOperator.withName(op.replace('\\', '/'))
        Some(createBinaryExpression(leftExp, parameters(0), arithmeticOp, returnType))
      case (BoolType, "||" | "&&", rightExp :: Nil) =>
        Some(createBooleanBinaryExpression(leftExp, rightExp, BooleanOperator.withName(op), returnType))
      case (BoolType, "==>", rightExp :: Nil) =>
        Some(createBooleanBinaryExpression(createNegatedBooleanExpression(leftExp), rightExp, BooleanOperator.||, returnType))
      case (RefType(_), "==" | "!=", rightExp :: Nil) =>
        Some(createReferenceComparisonExpression(leftExp, rightExp, ArithmeticOperator.withName(op), returnType))
      case (IntType, "+" | "-", Nil) =>
        Some(createUnaryExpression(leftExp, ArithmeticOperator.withName(op), returnType))
      case (BoolType, "!", Nil) =>
        Some(createNegatedBooleanExpression(leftExp))
      case _ => None
    }
    if (exprSet.isDefined) Some(state.setExpression(exprSet.get)) else None
  }
}

/** Provides the semantics of the custom 'assert', 'assume' and 'cond' methods.
  * The latter is used to support conditional expressions.
  */
object RichNativeMethodSemantics extends ForwardNativeMethodSemantics {
  def applyForwardNativeSemantics[S <: State[S]](
      thisExpr: ExpressionSet,
      operator: String,
      parameters: List[ExpressionSet],
      typeParameters: List[Type],
      returnType: Type,
      programPoint: ProgramPoint,
      state: S): Option[S] = {
    val nativeMethod = NativeMethods.values.find(_.toString == operator)
    nativeMethod match {
      // Semantics of conditional expressions like '(cond) ? a : b'
      case Some(NativeMethods.cond_exp) =>
        val thenState = state.assume(thisExpr)
        val elseState = state.assume(thisExpr.not())
        val thenExpr :: elseExpr :: Nil = parameters
        if (thenState.lessEqual(state.bottom()))
          Some(elseState.setExpression(elseExpr))
        else if (elseState.lessEqual(state.bottom()))
          Some(thenState.setExpression(thenExpr))
        else
          Some(thenState.setExpression(thenExpr).lub(elseState.setExpression(elseExpr)))
      // Speeds up the analysis if there is in fact an assertion violation
      case Some(NativeMethods.assert) =>
        val assertedState = state.setExpression(thisExpr)
        val assertedStateFalse = assertedState.testFalse()
        if (!assertedStateFalse.lessEqual(state.bottom())) {
          Reporter.reportError("Possible assertion violation", programPoint)
        }
        Some(assertedState.testTrue())
      case Some(NativeMethods.assume) =>
        Some(state.assume(thisExpr))
      case None => None
    }
  }
}

object NativeMethods extends Enumeration {
  val cond_exp = Value(Constants.GhostSymbolPrefix + "cond_exp")
  val assert = Value(Constants.GhostSymbolPrefix + "assert")
  val assume = Value(Constants.GhostSymbolPrefix + "assume")
}