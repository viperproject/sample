package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * User: Lucas Brutschy
 * Date: 11/8/12
 * Time: 5:36 PM
 */
abstract class AAny extends RichNativeSemantics {

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

      if (thisExpr.getType().toString() == getTypeName) {

        // Check if the object or an argument can be invalid - in this case, we must produce an error
        if(operator != "is_invalid") {
          if (!thisExpr.getType().isStatic()) {
            if (thisExpr.getType() != TBoolean.typ) { // FIXME: Invalid boolean types. Do they exist?
              Error(thisExpr equal Invalid(thisExpr.getType())(pp), operator+": Object ("+thisExpr+") might be invalid")(state,pp)
            }
          }
          for (param <- parameters) {
            if (param.getType() != TBoolean.typ) { // FIXME: Invalid boolean types. Do they exist?
              Error(param equal Invalid(param.getType())(pp), operator+": Parameter ("+param+") might be invalid")(state,pp)
            }
          }
        }

        Some(forwardSemantics(thisExpr,operator,parameters)(pp,state))

      } else None

  }

  /**
   * The string name of the current type
   */
  def getTypeName = getTyp.getName

  /**
   * The current type
   */
  def getTyp:TouchType

  /**
   * Implements forward semantics
   *
   * @param method Name of the operator/method (underscored)
   * @param parameters An expression for each parameter
   * @param pp // program point of the invocation
   * @param state // state after evaluation of the parameters
   * @return // state after evaluation of the method / operator
   */
  def forwardSemantics[S <: State[S]](this0:ExpressionSet,method:String,parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case "post_to_wall" =>
      Skip // TODO: create reference from wall to this?

    case "∥" =>
      Return[S](Valid(TString.typ)(pp))(state,pp)

    case "is_invalid" =>
      Return[S](this0 equal Invalid(this0.getType())(pp))(state,pp)

    case _ =>
      MatchFields[S](this0,parameters,getTyp,method)

  }

}
