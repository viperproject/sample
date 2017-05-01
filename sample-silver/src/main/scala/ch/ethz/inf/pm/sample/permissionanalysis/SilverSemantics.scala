/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.ExpressionSetFactory._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverMethodDeclaration, SilverMethods, sample}
import ch.ethz.inf.pm.sample.oorepresentation._

import scala.collection.mutable

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
  * A command issued when a method is left. exitState contains the return values of the analyzed methodCall
  * The command should apply the effect of the method
  * @param methodDeclaration
  * @param methodCall
  * @param stateBeforeMethodCall the callee's state before the method call
  * @param methodExitStates the effect of the method. contains the exit states of all analyzed methods
  * @author Flurin Rindisbacher
  */
case class LeaveMethodCommand[S](methodDeclaration: SilverMethodDeclaration, methodCall: MethodCall, stateBeforeMethodCall: S, methodExitStates: mutable.Map[String, S]) extends SilverCommand

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
      case ("&&", right :: Nil) =>
        Some(state.setExpression(createBooleanBinaryExpression(expression, right, BooleanOperator.&&)))
      case ("==" | "!=" | "<" | "<!" | ">" | ">=", right :: Nil) =>
        Some(state.setExpression(createBinaryArithmeticExpression(expression, right, ArithmeticOperator.withName(operator))))
      case _ =>
        SilverMethods.values.find(_.toString == operator) match {
          case Some(SilverMethods.access) => Some(state.setExpression(createFieldAccessPredicate(expression, parameters.head, parameters(1), returnType)))
          case Some(SilverMethods.permission) => Some(state.setExpression(createCurrentPermission(expression, returnType)))
          case Some(SilverMethods.inhale) => Some(state.command(InhaleCommand(expression)))
          case Some(SilverMethods.exhale) => Some(state.command(ExhaleCommand(expression)))
          case Some(method) => throw new UnsupportedOperationException(s"Unexpected method: $method")
          case None => None
        }
    }
  }
}
