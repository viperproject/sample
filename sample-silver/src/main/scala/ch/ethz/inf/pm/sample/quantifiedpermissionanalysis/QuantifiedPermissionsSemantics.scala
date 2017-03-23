/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.ExpressionSetFactory._
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.oorepresentation.silver.{Constants, PermType}
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.permissionanalysis.{ExhaleCommand, InhaleCommand}
import com.typesafe.scalalogging.LazyLogging

/**
  * Native method semantics for quantified permissions.
  * Defines the behaviour for quantified permissions-related constructs, like forall,
  *
  * @author Severin Münger
  *         Added on 05/11/16.
  */
object QuantifiedPermissionMethods extends Enumeration {
  val access = Value(Constants.GhostSymbolPrefix + "access")
  val permission = Value(Constants.GhostSymbolPrefix + "permission")
  val forall = Value(Constants.GhostSymbolPrefix + "forall")
  val inhale = Value(Constants.GhostSymbolPrefix + "inhale")
  val exhale = Value(Constants.GhostSymbolPrefix + "exhale")
}

/** Object adding Inhale/Exhale semantics.
  *
  * @author Caterina Urban
  */
object QuantifiedPermissionMethodSemantics extends NativeMethodSemantics with LazyLogging {

  /**
    * It defines the forward semantics of native method calls
    *
    * @param thisExpr       the expression representing the object on whom the method is called
    * @param operator       the string of the called method
    * @param parameters     the parameters of the called method
    * @param typeparameters the list of type generics
    * @param returnedtype   the type of the returned value
    * @param state          the abstract state in which the method call is evaluated
    * @return the abstract state obtained after the forward evaluation of the native method call,
    *         None if the semantics of the method call is not defined
    */
  override def applyForwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet,
                                                          operator: String,
                                                          parameters: List[ExpressionSet],
                                                          typeparameters: List[Type],
                                                          returnedtype: Type,
                                                          programpoint: ProgramPoint,
                                                          state: S): Option[S] = applyBackwardNativeSemantics(
    thisExpr,
    operator,
    parameters,
    typeparameters,
    returnedtype,
    programpoint,
    state
  )

  override def applyBackwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet,
                                                           operator: String,
                                                           parameters: List[ExpressionSet],
                                                           typeparameters: List[Type],
                                                           returnedtype: Type,
                                                           programpoint: ProgramPoint,
                                                           state: S): Option[S] = {
    val nativeMethod = QuantifiedPermissionMethods.values.find(_.toString == operator)
    nativeMethod match {
      case Some(QuantifiedPermissionMethods.access) =>
        val permissionExpr =
          if (parameters.size <= 1) createFieldAccessPredicate(thisExpr, parameters.head, ExpressionSet(Constant("1", PermType)), returnedtype)
          else createFieldAccessPredicate(thisExpr, parameters.head, parameters(1), returnedtype)
        Some(state.setExpression(permissionExpr))
      case Some(QuantifiedPermissionMethods.inhale) => Some(state.command(InhaleCommand(thisExpr)))
      case Some(QuantifiedPermissionMethods.exhale) => Some(state.command(ExhaleCommand(thisExpr)))
      case Some(QuantifiedPermissionMethods.forall) =>
        val implies = thisExpr.getSingle.get.asInstanceOf[BinaryBooleanExpression]
        val left = implies.left.asInstanceOf[NegatedBooleanExpression].exp
        assert(implies.op == BooleanOperator.||)
        val right = implies.right
        Some(state.setExpression(ExpressionSet(ForallExpression(left, right, parameters.head.getSingle.get.asInstanceOf[VariableIdentifier]))))
      case Some(QuantifiedPermissionMethods.permission) => Some(state.setExpression(createCurrentPermission(thisExpr, returnedtype)))
      case None => (operator, parameters) match {
        case ("&&", right :: Nil) => Some(state.setExpression(createBooleanBinaryExpression(thisExpr, right, BooleanOperator.&&)))
        case ("==" | "!=" | "<" | "<!" | ">" | ">=", right :: Nil) => Some(state.setExpression(createBinaryArithmeticExpression(thisExpr, right, ArithmeticOperator.withName(operator))))
        case _ => Some(state.setExpression(ExpressionSetFactory.createFunctionCallExpression(operator, parameters, returnedtype, programpoint)))
      }
    }
  }


}
