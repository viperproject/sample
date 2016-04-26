/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.ExpressionFactory._
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, PermissionExpression, SimplePermissionState, State}
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
    * @param typeparameters the list of type generics
    * @param returnedtype the type of the returned value
    * @param state the abstract state in which the method call is evaluated
    * @return the abstract state obtained after the forward evaluation of the native method call,
    *         None if the semantics of the method call is not defined
    */
  override def applyForwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet,
                                                          operator: String,
                                                          parameters: List[ExpressionSet],
                                                          typeparameters: List[Type],
                                                          returnedtype: Type,
                                                          programpoint: ProgramPoint,
                                                          state: S): Option[S] = state match {
    case state: SimplePermissionState[S] =>
      val nativeMethod = PermissionMethods.values.find(_.toString == operator)
      nativeMethod match {
        case Some(PermissionMethods.permission) =>
          val thenExpr = createPermissionExpression(thisExpr, parameters(0), returnedtype)
          Some(state.setExpression(thenExpr).asInstanceOf[S])
        case Some(PermissionMethods.inhale) => Some(state.inhale(thisExpr).asInstanceOf[S])
        case Some(PermissionMethods.exhale) => Some(state.exhale(thisExpr).asInstanceOf[S])
        case None => None
      }
    case _ => None
  }

  /**
    * It defines the backward semantics of native method calls
    *
    * @param thisExpr the expression representing the object on whom the method is called
    * @param operator the string of the called method
    * @param parameters the parameters of the called method
    * @param typeparameters the list of type generics
    * @param returnedtype the type of the returned value
    * @param programpoint the program point of the method call
    * @param state the abstract state in which the method call is evaluated
    * @return the abstract state obtained after the backward evaluation of the native method call,
    *         None if the semantics of the method call is not defined
    */
  override def applyBackwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet,
                                                           operator: String,
                                                           parameters: List[ExpressionSet],
                                                           typeparameters: List[Type],
                                                           returnedtype: Type,
                                                           programpoint: ProgramPoint,
                                                           state: S,
                                                           oldPreState: S): Option[S] = ???
}
