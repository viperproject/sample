/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, Identifier, State}
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.domain.MultiValExpression
import RichNativeSemantics._
import ch.ethz.inf.pm.td.cloud.CloudQueryWrapper

/**
 * User: Lucas Brutschy
 * Date: 11/8/12
 * Time: 5:36 PM
 */
trait AAny extends NativeMethodSemantics with RichExpressionImplicits with TouchType {

  def member__confirmed = ApiMember(
    name = "◈confirmed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = CloudQueryWrapper(ValidPureSemantics,Set(CloudEnabledModifier))
  )

  def member__ref = ApiMember(
    name = "◈ref",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GRef(this),
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        // TODO: References should be encoded as a string in the string domain
        var curState = state
        val typ = GRef(this0.getType().asInstanceOf[AAny])
        curState = New[S](typ)(curState,pp)
        val ref = curState.expr
        curState = AssignField[S](ref,typ.field__get,this0)
        curState
      }
    }
  )

  def member_∥ = ApiMember(
    name = "∥",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(other) = parameters
        Return[S](this0 concat other)
      }
    }
  )

  def member_is_invalid = ApiMember(
    name = "is invalid",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Return[S](this0 equal Invalid(this0.getType(), "")(pp))(state, pp)
      }
    }
  )

  def member_comma = ApiMember(
    name = ",",
    paramTypes = List(ApiParam(TUnknown), ApiParam(TUnknown)),
    thisType = ApiParam(this),
    returnType = TUnknown,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(right) = parameters // Unknown,Unknown
        var multiValExpressionSet = new ExpressionSet(TUnknown)
        for (l <- this0.getNonTop; r <- right.getNonTop) {
          multiValExpressionSet = multiValExpressionSet.add(MultiValExpression(l, r, TUnknown))
        }
        state.setExpression(multiValExpressionSet)
      }
    }
  )

  def member_:= = ApiMember(
    name = ":=",
    paramTypes = List(ApiParam(TUnknown), ApiParam(TUnknown)),
    thisType = ApiParam(this),
    returnType = TUnknown,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(right) = parameters
        val res = Assign[S](this0, right)
        // Dirty old PhD students hacking dirty
        if (TouchAnalysisParameters.get.prematureAbort) {
          Exit[S](res, pp)
        }
        res
      }
    }
  )

  def member_async = ApiMember(
    name = "async",
    paramTypes = List(ApiParam(TUnknown)),
    thisType = ApiParam(this),
    returnType = GTask(TUnknown),
    semantics = DefaultSemantics
  )

  def member_equals = ApiMember(
    name = "equals",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = ValidPureSemantics
  )

  def member_post_to_wall = ApiMember(
    name = "post to wall",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = SkipSemantics
  )

  def getDeclaration(s: String) = declarations.get(s)

  def declarations:Map[String,ApiMember] =
    Map(
      "," -> member_comma,
      ":=" -> member_:=,
      "∥" -> member_∥,
      "async" -> member_async,
      "is invalid" -> member_is_invalid,
      "post to wall" -> member_post_to_wall,
      "equals" -> member_equals,
      "◈ref" -> member__ref,
      member__confirmed.name -> member__confirmed
    )

  def isSingleton = false
  def isImmutable = true

  def possibleFields = Set.empty

  override def representedFields =
    if (TouchAnalysisParameters.get.libraryFieldPruning && SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.nonEmpty) {
      val relFields = SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields
      val typFields = possibleFields -- mutedFields
      typFields.filter({ f: Identifier => relFields.contains(this.name + "." + f.getName) })
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
                                                  returnedtype: Type, pp: ProgramPoint, state: S): Option[S] = None

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
          if (TouchAnalysisParameters.get.printValuesInWarnings)
            curState = Error(thisExpr equal Invalid(thisExpr.getType(), "")(pp), operator, "Object (" + thisExpr + ") whose field/method is accessed might be invalid")(curState, pp)
          else
            curState = Error(thisExpr equal Invalid(thisExpr.getType(), "")(pp), operator, "Object whose field/method is accessed might be invalid")(curState, pp)
        }
        for (param <- parameters) {
          if (TouchAnalysisParameters.get.printValuesInWarnings)
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

    case _ =>

      matchRecordCalls[S](this0, parameters, method, returnedType)

  }

  private def matchRecordCalls[S <: State[S]](this0: RichExpression, parameters: List[ExpressionSet], method: String, returnedType:TouchType)(implicit state: S, pp: ProgramPoint): S = {

    // Sometimes, x.bla(y) is rewritten to code->bla(x,y) for records
    val context = SystemParameters.analysisUnitContext
    val arguments = this0.getType() :: (parameters map (_.getType()))
    SystemParameters.compiler.asInstanceOf[TouchCompiler].getMethod(method, arguments) match {
      case Some(mdecl) =>
        MethodSummaries.collect(pp, mdecl, state, parameters)
      case _ =>
        matchFields[S](this0,parameters,method,returnedType)
    }

  }

  private def matchFields[S <: State[S]](this0: RichExpression, parameters: List[ExpressionSet], method: String, returnedType:TouchType)(implicit state: S, pp: ProgramPoint): S = {

    val fieldResult =
      if (parameters.isEmpty)
      // Getters
        representedFields.find(_.getName == method) match {
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
            if (!representedFields.contains(field))
              Some(state)
            else
              Some(AssignField[S](this0, field, parameters.head))
          case None => None
        }
      else None


    fieldResult match {
      case Some(res) => res
      case None =>

        val mutedFieldResult =
          if (parameters.isEmpty)
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
            getDeclaration(method) match {
              case Some(res) =>
                res.semantics.forwardSemantics(this0,res,parameters)
              case None =>

//                // Try implicit conversion to Ref
//                if (!this.isInstanceOf[GRef]) {
//                  val refType = GRef(this)
//                  refType.getDeclaration(method) match {
//                    case Some(x) =>
//                      x.semantics.forwardSemantics[S](this0,x,parameters)
//                    case None =>
//                      Unimplemented[S](this.toString + "." + method, returnedType)
//                  }
//
//                } else {
                  Unimplemented[S](this.toString + "." + method, returnedType)
//                }

            }

        }
    }

  }

  def mkGetterSetters(fields: Set[ApiField]):Map[String,ApiMember] = fields.flatMap { x: ApiField => Map(
    x.getName -> ApiMember(x.getName, List(), ApiParam(this), x.typ, DefaultSemantics),
    "set " + x.getName -> ApiMember("set " + x.getName, List(), ApiParam(this, isMutated = true), TNothing, DefaultSemantics))
  }.toMap

  def mkGetters(fields: Set[ApiField]) = fields.map { x: ApiField => (
    x.getName, ApiMember(x.getName, List(), ApiParam(this), x.typ, DefaultSemantics))
  }.toMap

}
