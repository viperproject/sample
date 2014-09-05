package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.td.analysis.{RichNativeSemantics, RichExpressionImplicits, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.domain.MultiValExpression
import RichNativeSemantics._

/**
 * User: Lucas Brutschy
 * Date: 11/8/12
 * Time: 5:36 PM
 */
trait AAny extends NativeMethodSemantics with RichExpressionImplicits with TouchType {

  def isSingleton = false
  def isImmutable = true

  /**
   * Backward semantics are empty for all native function for now
   */
  def applyBackwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet, operator: String,
                                                  parameters: List[ExpressionSet], typeparameters: List[Type],
                                                  returnedtype: Type, pp: ProgramPoint, state: S, oldPreState: S): Option[S] = None

  /**
   * Delegates forward semantics to concrete classes.
   *
   * Checks if the object or any other
   */
  def applyForwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet, operator: String,
                                                 parameters: List[ExpressionSet], typeparameters: List[Type],
                                                 returnedtype: Type, pp: ProgramPoint, state: S): Option[S] = {


    if (thisExpr.getType().asInstanceOf[TouchType].typeName == typeName) {

      if (state.lessEqual(state.bottom())) {
        return Some(state.bottom())
      }

      var curState = state

      // Check if the object or an argument can be invalid - in this case, we must produce an error
      if (operator != "is invalid" && operator != ":=" && operator != "," && thisExpr.getType().name != "code") {
        if (!thisExpr.getType().isStatic) {
          if (TouchAnalysisParameters.printValuesInWarnings)
            curState = Error(thisExpr equal Invalid(thisExpr.getType(), "")(pp), operator, "Object (" + thisExpr + ") whose field/method is accessed might be invalid")(curState, pp)
          else
            curState = Error(thisExpr equal Invalid(thisExpr.getType(), "")(pp), operator, "Object whose field/method is accessed might be invalid")(curState, pp)
        }
        for (param <- parameters) {
          if (TouchAnalysisParameters.printValuesInWarnings)
            curState = Error(param equal Invalid(param.getType(), "")(pp), operator, "Parameter (" + param + ") might be invalid")(curState, pp)
          else
            curState = Error(param equal Invalid(param.getType(), "")(pp), operator, "Parameter might be invalid")(curState, pp)
        }
      }

      Some(forwardSemantics(thisExpr, operator, parameters, returnedtype.asInstanceOf[TouchType])(pp, curState))

    } else None

  }

  /**
   * Implements forward semantics
   *
   * @param method Name of the operator/method (underscored)
   * @param parameters An expression for each parameter
   * @param pp // program point of the invocation
   * @param state // state after evaluation of the parameters
   * @return // state after evaluation of the method / operator
   */
  def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                     (implicit pp: ProgramPoint, state: S): S = method match {

    /** Updates any display of this map */
    case "update on wall" =>
      Skip // TODO: Update environment

    case "post to wall" =>
      Skip // TODO: create reference from wall to this?

    case "∥" =>
      val List(other) = parameters
      Return[S](this0 concat other)

    case "to string" =>
      Top[S](TString)

    case "is invalid" =>
      Return[S](this0 equal Invalid(this0.getType(), "")(pp))(state, pp)

    case "equals" =>
      Dummy[S](this0, method)
      Top[S](TBoolean)

    case ":=" =>
      val List(right) = parameters
      val res = Assign[S](this0, right)
      // Dirty old PhD students hacking dirty
      if (TouchAnalysisParameters.prematureAbortion && this0.toString.contains("__data_")) {
        Exit[S](res, pp)
      }
      res

    case "," =>
      val List(right) = parameters // Unknown,Unknown
    var multiValExpressionSet = new ExpressionSet(TUnknown)
      for (l <- this0.getSetOfExpressions; r <- right.getSetOfExpressions) {
        multiValExpressionSet = multiValExpressionSet.add(new MultiValExpression(l, r, TUnknown))
      }
      state.setExpression(multiValExpressionSet)

    case _ =>
      MatchFields[S](this0, parameters, this, method)

  }

}
