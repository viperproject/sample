package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}

/**
 * User: Lucas Brutschy
 * Date: 11/8/12
 * Time: 5:36 PM
 */
abstract class Any extends RichNativeSemantics {

  /**
   * Backward semantics are empty for all native function for now
   */
  def applyBackwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                  parameters : List[ExpressionSet], typeparameters : List[Type],
                                                  returnedtype : Type, pp : ProgramPoint, state : S) : Option[S] = None

  /**
   * Delegates forward semantics to concrete classes.
   *
   * Checks if the object or any other
   */
  def applyForwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                 parameters : List[ExpressionSet], typeparameters : List[Type],
                                                 returnedtype : Type, pp : ProgramPoint, state : S) : Option[S] = {

    // Check if the object or an argument can be invalid - in this case, we must produce an error
    if (!thisExpr.getType().isStatic()) {
      Error(thisExpr equal invalid(thisExpr.getType()), operator+": Object ("+thisExpr+") might be invalid")(state,pp)
    }
    for (param <- parameters) {
      Error(param equal invalid(param.getType()), operator+": Parameter ("+param+") might be invalid")(state,pp)
    }

    // Implementations of standard methods that can be invoked on any kind of object, even invalids.

    operator match {

      case "post_to_wall" => Some(state) // TODO: create reference from wall to this?
      case "∥" => Some(state) // TODO: put a valid string on the stack
      case "is_invalid" => Some(Expr[S](thisExpr equal invalid(thisExpr.getType()))(state,pp))

      // Delegate to concrete implementations
      case _ =>
        if (thisExpr.getType().toString() == getTypeName)
          Some(forwardSemantics(thisExpr,operator,parameters)(pp,state))
        else None

    }

  }

  /**
   * The string name of the current type
   */
  def getTypeName:String

  /**
   * Implements forward semantics
   *
   * @param method Name of the operator/method (underscored)
   * @param parameters An expression for each parameter
   * @param pp // program point of the invocation
   * @param state // state after evaluation of the parameters
   * @return // state after evaluation of the method / operator
   */
  def forwardSemantics[S <: State[S]](thisExpr:ExpressionSet,method:String,parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S

}
