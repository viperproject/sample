/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.ExpressionSetFactory._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{PermType, SilverMethodDeclaration, SilverMethods}

/** Super trait for all silver commands.
  */
sealed trait SilverCommand extends Command

/** A command that inhales the given expression.
  *
  * @param expression The inhaled expression.
  * @author Jerome Dohrau
  */
case class InhaleCommand(expression: ExpressionSet) extends SilverCommand

/** A command that exhales the given expression.
  *
  * @param expression The exhaled expression.
  * @author Jerome Dohrau
  */
case class ExhaleCommand(expression: ExpressionSet) extends SilverCommand

/** A command that handles a precondition.
  *
  * @param condition The precondition.
  * @author Jerome Dohrau
  */
case class PreconditionCommand(condition: ExpressionSet) extends SilverCommand

/** A command that handles the given postcondition.
  *
  * @param condition The postcondition.
  * @author Jerome Dohrau
  */
case class PostconditionCommand(condition: ExpressionSet) extends SilverCommand

/** A command that handles the given invariant.
  *
  * @param condition The invariant.
  * @author Jerome Dohrau
  */
case class InvariantCommand(condition: ExpressionSet) extends SilverCommand

/**
  * A command issued whenever a loop is entered.
  *
  * @author Jerome Dohrau
  */
case class EnterLoopCommand() extends SilverCommand

/**
  * A command issued whenever a loop is left.
  *
  * @author Jerome Dohrau
  */
case class LeaveLoopCommand() extends SilverCommand

/**
  * A command issued when a method is left and we return into the caller.
  *
  * @param methodDeclaration the method declaration of the called method
  * @param methodCall        the statement that called the method
  * @param targetExpressions The target expressions that will receive the callee's returns
  * @param exitState         the exit state of the called method. E.g CfgResult.exitState()
  * @author Flurin Rindisbacher
  */
case class ReturnFromMethodCommand[S](methodDeclaration: SilverMethodDeclaration, methodCall: MethodCall, targetExpressions: Seq[ExpressionSet], exitState: S) extends SilverCommand

/**
  * A command issued when a method is called in the BACKWARD analysis. This command is issued when a callee
  * has been fully analyzed and the entry state should be merged back into the caller. This is the dual to ReturnFromMethodCommand
  * in the forward analysis.
  *
  * @param methodDeclaration    the method declaration of the called method
  * @param methodCall           the statement that called the method
  * @param parameterExpressions The parameters expressions that will receive the states of the callee's parameters.
  * @param entryState           the exit state of the called method. E.g CfgResult.exitState()
  * @author Flurin Rindisbacher
  */
case class CallMethodBackwardsCommand[S](methodDeclaration: SilverMethodDeclaration, methodCall: MethodCall, parameterExpressions: Seq[ExpressionSet], entryState: S) extends SilverCommand

/** Object adding Inhale/Exhale semantics.
  *
  * @author Caterina Urban, Jerome Dohrau
  */
object SilverSemantics extends NativeMethodSemantics {

  /**
    * It defines the backward semantics of native method calls
    *
    * @param thisExpr       the expression representing the object on whom the method is called
    * @param operator       the string of the called method
    * @param parameters     the parameters of the called method
    * @param typeParameters the list of type generics
    * @param returnType     the type of the returned value
    * @param programPoint   the program point of the method call
    * @param state          the abstract state in which the method call is evaluated
    * @return the abstract state obtained after the backward evaluation of the native method call,
    *         None if the semantics of the method call is not defined
    */
  override def applyBackwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet,
                                                           operator: String,
                                                           parameters: List[ExpressionSet],
                                                           typeParameters: List[Type],
                                                           returnType: Type,
                                                           programPoint: ProgramPoint,
                                                           state: S): Option[S] =
    applyForwardNativeSemantics[S](thisExpr, operator, parameters, typeParameters, returnType, programPoint, state)

  /**
    * It defines the forward semantics of native method calls
    *
    * @param expression     the expression representing the object on whom the method is called
    * @param operator       the string of the called method
    * @param parameters     the parameters of the called method
    * @param typeParameters the list of type generics
    * @param returnType     the type of the returned value
    * @param state          the abstract state in which the method call is evaluated
    * @return the abstract state obtained after the forward evaluation of the native method call,
    *         None if the semantics of the method call is not defined
    */
  override def applyForwardNativeSemantics[S <: State[S]](expression: ExpressionSet,
                                                          operator: String,
                                                          parameters: List[ExpressionSet],
                                                          typeParameters: List[Type],
                                                          returnType: Type,
                                                          programPoint: ProgramPoint,
                                                          state: S): Option[S] = {
    (operator, parameters) match {
      case ("/", right :: Nil) if returnType == PermType =>
        Some(state.setExpression(createFractionalPermission(expression, right, returnType)))
      case ("&&", right :: Nil) =>
        Some(state.setExpression(createBooleanBinaryExpression(expression, right, BooleanOperator.&&)))
      case ("==" | "!=" | "<" | "<!" | ">" | ">=", right :: Nil) =>
        Some(state.setExpression(createBinaryArithmeticExpression(expression, right, ArithmeticOperator.withName(operator))))
      case _ =>
        SilverMethods.values.find(_.toString == operator) match {
          case Some(SilverMethods.access) =>
            Some(state.setExpression(createFieldAccessPredicate(expression, parameters.head, returnType)))
          case Some(SilverMethods.permission) => Some(state.setExpression(createCurrentPermission(expression, returnType)))
          case Some(SilverMethods.inhale) => Some(state.command(InhaleCommand(expression)))
          case Some(SilverMethods.exhale) => Some(state.command(ExhaleCommand(expression)))
          case Some(method) => throw new UnsupportedOperationException(s"Unexpected method: $method")
          case None => None
        }
    }
  }
}
