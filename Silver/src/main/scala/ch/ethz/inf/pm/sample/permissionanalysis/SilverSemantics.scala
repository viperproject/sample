/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.ExpressionFactory._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverMethods
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}

/** An inhale command.
  *
  * @param expression The inhaled expression.
  * @author Jerome Dohrau
  */
case class InhaleCommand(expression: ExpressionSet) extends Command

/** An exhale command.
  *
  * @param expression The exhaled expression.
  * @author Jerome Dohrau
  */
case class ExhaleCommand(expression: ExpressionSet) extends Command

case class PreconditionCommand(condition: ExpressionSet) extends Command

case class PostconditionCommand(condition: ExpressionSet) extends Command

/** A command
  *
  * @param condition The loop invariant.
  * @author Jerome Dohrau
  */
case class InvariantCommand(condition: ExpressionSet) extends Command

/** Object adding Inhale/Exhale semantics.
  *
  * @author Caterina Urban, Jerome Dohrau
  */
object SilverMethodSemantics extends NativeMethodSemantics {

  /**
    * It defines the forward semantics of native method calls
    *
    * @param thisExpr the expression representing the object on whom the method is called
    * @param operator the string of the called method
    * @param parameters the parameters of the called method
    * @param typeParameters the list of type generics
    * @param returnType the type of the returned value
    * @param state the abstract state in which the method call is evaluated
    * @return the abstract state obtained after the forward evaluation of the native method call,
    *         None if the semantics of the method call is not defined
    */
  override def applyForwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet,
                                                          operator: String,
                                                          parameters: List[ExpressionSet],
                                                          typeParameters: List[Type],
                                                          returnType: Type,
                                                          programPoint: ProgramPoint,
                                                          state: S): Option[S] = {
    val nativeMethod = SilverMethods.values.find(_.toString == operator)
    nativeMethod match {
      case Some(SilverMethods.permission) =>
        val permission = if (parameters.length <= 1)
          createPermissionExpression(thisExpr, parameters(0), parameters(0), returnType)
        else
          createPermissionExpression(thisExpr, parameters(0), parameters(1), returnType)
        Some(state.setExpression(permission))
      case Some(SilverMethods.inhale) => Some(state.command(InhaleCommand(thisExpr)))
      case Some(SilverMethods.exhale) => Some(state.command(ExhaleCommand(thisExpr)))
      case Some(SilverMethods.precondition) => Some(state.command(PreconditionCommand(thisExpr)))
      case Some(SilverMethods.postcondition) => Some(state.command(PostconditionCommand(thisExpr)))
      case Some(SilverMethods.invariant) => Some(state.command(InvariantCommand(thisExpr)))
      case Some(method) => throw new UnsupportedOperationException(s"Unexpected method: $method")
      case None => None
    }
  }

  /**
    * It defines the backward semantics of native method calls
    *
    * @param thisExpr the expression representing the object on whom the method is called
    * @param operator the string of the called method
    * @param parameters the parameters of the called method
    * @param typeParameters the list of type generics
    * @param returnType the type of the returned value
    * @param programPoint the program point of the method call
    * @param state the abstract state in which the method call is evaluated
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
}
