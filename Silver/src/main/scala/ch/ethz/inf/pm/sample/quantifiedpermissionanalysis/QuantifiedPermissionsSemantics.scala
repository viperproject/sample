package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.ExpressionFactory._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.Constants
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}

/**
  * Native method semantics for quantified permissions.
  * Defines the behaviour for quantified permissions-related constructs, like forall,
  *
  * @author Severin Münger
  *         Added on 05/11/16.
  */
object QuantifiedPermissionMethods extends Enumeration {
  val forall = Value(Constants.GhostSymbolPrefix + "forall")
  val inhale = Value(Constants.GhostSymbolPrefix + "inhale")
  val exhale = Value(Constants.GhostSymbolPrefix + "exhale")
}

/** Object adding Inhale/Exhale semantics.
  *
  * @author Caterina Urban
  */
object QuantifiedPermissionMethodSemantics extends NativeMethodSemantics {

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
                                                          state: S): Option[S] = None

  override def applyBackwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet,
                                                           operator: String,
                                                           parameters: List[ExpressionSet],
                                                           typeparameters: List[Type],
                                                           returnedtype: Type,
                                                           programpoint: ProgramPoint,
                                                           state: S): Option[QuantifiedPermissionsState] = {
    state match {
      case state: QuantifiedPermissionsState =>
        val nativeMethod = QuantifiedPermissionMethods.values.find(_.toString == operator)
        nativeMethod match {
          case Some(QuantifiedPermissionMethods.inhale) => Some(state.inhale(thisExpr))
          case Some(QuantifiedPermissionMethods.exhale) => Some(state.exhale(thisExpr))
          case Some(QuantifiedPermissionMethods.forall) =>
            val implies = thisExpr.getSingle.get.asInstanceOf[BinaryBooleanExpression]
            val left = implies.left.asInstanceOf[NegatedBooleanExpression].exp
            val right = implies.right
            val forallExpr = ForallExpression(left, right, parameters.head.getSingle.get.asInstanceOf[VariableIdentifier])
            Some(state.setExpression(ExpressionSet(forallExpr)))
          case None => if (thisExpr.getSingle.isDefined) {
            Some(state.array_acc(thisExpr.getSingle.get, parameters(0).getSingle.get, returnedtype).asInstanceOf[S])
          } else {
            None
          }
          case _ => None
        }
      case _ => None
    }
  }
}
