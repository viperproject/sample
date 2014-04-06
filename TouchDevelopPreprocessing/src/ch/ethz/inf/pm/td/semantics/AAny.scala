package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.domain.MultiValExpression
import scala.Some
import ch.ethz.inf.pm.td.analysis.interpreter._
import ch.ethz.inf.pm.sample.backwardanalysis.ExecutionHistoryState

/**
 * User: Lucas Brutschy
 * Date: 11/8/12
 * Time: 5:36 PM
 */
abstract class AAny extends NativeMethodSemantics with RichExpressionImplicits {

  def applyBackwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                  parameters : List[ExpressionSet], typeparameters : List[Type],
                                                  returnedtype : Type, pp : ProgramPoint, state : S, oldPreState: S) : Option[S] = {
    val thisType = thisExpr.getType()
    if (thisType.toString() == getTypeName) {

      if (thisType != SCode.typ && state.lessEqual(state.bottom())) {
        return Some(state.bottom())
      }

      val curState = state

      val backwardResult = backwardSemantics[S](thisExpr, operator, parameters, returnedtype.asInstanceOf[TouchType])(pp, curState, oldPreState)
      Some(backwardResult)

    } else None
  }

  /**
   * Delegates forward semantics to concrete classes.
   *
   * Checks if the object or any other
   */
  def applyForwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                 parameters : List[ExpressionSet], typeparameters : List[Type],
                                                 returnedtype : Type, pp : ProgramPoint, state : S) : Option[S] = {


      if (thisExpr.getType().toString == getTypeName) {

        if (state.lessEqual(state.bottom())) {
          return Some(state.bottom())
        }

        // TODO: This belongs somewhere else I guess
        if (TouchAnalysisParameters.prematureAbortion) {
          Exit[S](state,pp)
        }

        var curState = state

        // Check if the object or an argument can be invalid - in this case, we must produce an error
        if(operator != "is invalid" && operator != ":=" && operator != "," && thisExpr.getType().name != "code") {
          if (!thisExpr.getType().isStatic) {
            if (thisExpr.getType() != TBoolean.typ) { // FIXME: Invalid boolean types. Do they exist?
              if (TouchAnalysisParameters.printValuesInWarnings)
                curState = Error(thisExpr equal Invalid(thisExpr.getType())(pp), operator,  "Object ("+thisExpr+") whose field/method is accessed might be invalid")(curState,pp)
              else
                curState = Error(thisExpr equal Invalid(thisExpr.getType())(pp), operator,  "Object whose field/method is accessed might be invalid")(curState,pp)
            }
          }
          for (param <- parameters) {
            if (param.getType() != TBoolean.typ) { // FIXME: Invalid boolean types. Do they exist?
              if (TouchAnalysisParameters.printValuesInWarnings)
                curState = Error(param equal Invalid(param.getType())(pp), operator, "Parameter ("+param+") might be invalid")(curState,pp)
              else
                curState = Error(param equal Invalid(param.getType())(pp), operator, "Parameter might be invalid")(curState,pp)
            }
          }
        }

        val forwardResult = forwardSemantics(thisExpr, operator, parameters, returnedtype.asInstanceOf[TouchType])(pp, curState)
        Some(forwardResult)

      } else None

  }

  /**
   * The string name of the current type
   */
  def getTypeName = getTyp.name

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
  def forwardSemantics[S <: State[S]](this0:ExpressionSet,method:String,parameters:List[ExpressionSet],returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state: S): S = method match {

    /** Updates any display of this map */
    case "update on wall" =>
      Skip // TODO: Update environment

    case "post to wall" =>
      Skip // TODO: create reference from wall to this?

    case "∥" =>
      val List(other) = parameters
      Return[S]( this0 concat other )

    case "to string" =>
      Top[S](TString.typ)

    case "is invalid" =>
      Return[S](this0 equal Invalid(this0.getType())(pp))(state,pp)

    case "equals" =>
      Dummy[S](this0,method)
      Top[S](TBoolean.typ)

    case ":=" =>
      val List(right) = parameters
      val res = Assign[S](this0,right)
      res.optimizeSummaryNodes()

    case "," =>
      val List(right) = parameters // Unknown,Unknown
      var multiValExpressionSet = new ExpressionSet(TUnknown.typ)
      for (l <- this0.getSetOfExpressions; r <- right.getSetOfExpressions) {
        multiValExpressionSet = multiValExpressionSet.add(new MultiValExpression(l,r,TUnknown.typ))
      }
      state.setExpression(multiValExpressionSet)

    case _ =>
      MatchFields[S](this0,parameters,getTyp,method)
  }

  def backwardSemantics[S <: State[S]](this0: ExpressionSet, method:String, parameters: List[ExpressionSet], returnedType: TouchType)
                                     (implicit pp: ProgramPoint, post: S, oldPreState: S): S = {
    val historyState = ExecutionHistoryState(oldPreState)
    val forwardHistory = forwardSemantics[ExecutionHistoryState[S]](this0, method, parameters, returnedType)(pp, historyState)
    val refinedPre = forwardHistory.executeHistoryBackward(post)
    refinedPre
  }


  def concreteSemantics(this0: TouchValue, method: String, params: List[TouchValue], interpreter: ConcreteInterpreter,
                        pp: ProgramPoint): TouchValue = {
    val state = interpreter.state

    method match {
      case "is invalid" => this0 match {
        case iv: InvalidV => BooleanV(v = true)
        case _ => BooleanV(v= false)
      }
      case "post to wall" => UnitV
      case _ =>

        // If this0 is a reference to a heap object, this may be a generic field get/set call
        val result: Option[TouchValue] =
          this0 match {
            case ref@RefV(typ, heapId) =>
              if(params.length == 0) {
                // Getters
                typ.possibleFields.find(_.getName == method ) match {
                  case Some(tf: TouchField) =>
                    val fieldVal = state.getField(ref, method)
                    Some(fieldVal)
                  case None => None
                }
              } else if (params.length == 1) {
                // Setter
                typ.possibleFields.find("set "+_.getName == method ) match {
                  case Some(tf: TouchField) =>
                    state.setField(ref, method, params.head)
                    Some(UnitV)
                  case None => None
                }
              } else None
            case _ => None
          }

        result match {
          case Some(v) => v
          case None => sys.error(s"Method $method on $this0 not implemented")
        }
    }
  }
}
