/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.ExpressionFactory._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.Constants
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}

/** Object enumerating methods to handle permissions.
  *
  * @author Caterina Urban
  */
object PermissionMethods extends Enumeration {
  val permission = Value(Constants.GhostSymbolPrefix + "permission")
  val inhale = Value(Constants.GhostSymbolPrefix + "inhale")
  val exhale = Value(Constants.GhostSymbolPrefix + "exhale")
}

/** An inhale command.
  *
  * @param expression The inhaled expression.
  * @author Jerome Dohrau
  */
case class Inhale(expression: ExpressionSet) extends Command

/** An exhale command.
  *
  * @param expression The exhaled expression.
  * @author Jerome Dohrau
  */
case class Exhale(expression: ExpressionSet) extends Command

/** Object adding Inhale/Exhale semantics.
  *
  * @author Caterina Urban
  */
object PermissionMethodSemantics extends NativeMethodSemantics {

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
    val nativeMethod = PermissionMethods.values.find(_.toString == operator)
    nativeMethod match {
      case Some(PermissionMethods.permission) =>
        val permission = if (parameters.length <= 1)
          createPermissionExpression(thisExpr, parameters(0), parameters(0), returnType)
        else
          createPermissionExpression(thisExpr, parameters(0), parameters(1), returnType)
        Some(state.setExpression(permission))
      case Some(PermissionMethods.inhale) => Some(state.command(Inhale(thisExpr)))
      case Some(PermissionMethods.exhale) => Some(state.command(Exhale(thisExpr)))
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
