package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.domain.MultiValExpression
import RichNativeSemantics._
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.typecheck.Member

/**
 * User: Lucas Brutschy
 * Date: 11/8/12
 * Time: 5:36 PM
 */
trait AAny extends NativeMethodSemantics with RichExpressionImplicits with TouchType {

  def member_∥ = ApiMember(
    name = "∥",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  def member_is_invalid = ApiMember(
    name = "is invalid",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  def member_comma = new ApiMember(
    name = ",",
    paramTypes = List(ApiParam(TUnknown), ApiParam(TUnknown)),
    thisType = ApiParam(this),
    returnType = TUnknown,
    semantics = DefaultSemantics
  )

  def member_:= = new ApiMember(
    name = ":=",
    paramTypes = List(ApiParam(TUnknown), ApiParam(TUnknown)),
    thisType = ApiParam(this),
    returnType = TUnknown,
    semantics = DefaultSemantics
  )

  def member_async = new ApiMember(
    name = "async",
    paramTypes = List(ApiParam(TUnknown)),
    thisType = ApiParam(this),
    returnType = TUnknown,
    semantics = DefaultSemantics
  )

  def member_post_to_wall = ApiMember(
    name = "post to wall",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  def declarations:Map[String,ApiMember] = Map(
    "," -> member_comma,
    ":=" -> member_:=,
    "∥" -> member_∥,
    "async" -> member_async,
    "is invalid" -> member_is_invalid,
    "post to wall" -> member_post_to_wall
  )

  def isSingleton = false
  def isImmutable = true

  def possibleFields = Set.empty

  override def representedFields =
    if (TouchAnalysisParameters.libraryFieldPruning) {
      val relFields = SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields
      val typFields = possibleFields -- mutedFields
      typFields.filter({ f: Identifier => relFields.contains(this.name + "." + f.getName)}).toSet[Identifier]
    } else {
      possibleFields -- mutedFields
    }
  def representedTouchFields = representedFields.map(_.asInstanceOf[ApiField])

  def mutedFields:Set[ApiField] = Set.empty

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

      if (state.isBottom) {
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

      if (curState.isBottom) {
        return Some(state.bottom())
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

      matchFields[S](this0, parameters, method)

  }


  def matchFields[S <: State[S]](this0: RichExpression, parameters: List[ExpressionSet], method: String)(implicit state: S, pp: ProgramPoint): S = {

    val fieldResult =
      if (parameters.length == 0)
      // Getters
        possibleFields.find(_.getName == method) match {
          case Some(field) =>
            val stateWithExpr = state.getFieldValue(this0,method,field.typ)
            if (stateWithExpr.isBottom) Some(state.setExpression(new ExpressionSet(field.typ).top()))
            else Some(stateWithExpr)
          case None => None
        }
      else if (parameters.length == 1)
        // Setters
        possibleFields.find("set " + _.getName == method) match {
          case Some(field) =>
            Some(AssignField[S](this0, field, parameters.head))
          case None => None
        }
      else None


    fieldResult match {
      case Some(res) => res
      case None =>

        val mutedFieldResult =
          if (parameters.length == 0)
          // Getters
            mutedFields.find(_.getName == method) match {
              case Some(field) => Some(Top[S](field.typ)(state,pp))
              case None => None
            }
          else if (parameters.length == 1)
          // Setters
            mutedFields.find("set " + _.getName == method) match {
              case Some(field) => Some(state)
              case None => None
            }
          else None

        mutedFieldResult match {
          case Some(res) => res
          case None =>
            declarations.get(method) match {
              case Some(res) =>
                res.semantics.forwardSemantics(this0,res,parameters)
              case None =>
                Unimplemented[S](this.toString + "." + method)
            }

        }
    }

  }

}
