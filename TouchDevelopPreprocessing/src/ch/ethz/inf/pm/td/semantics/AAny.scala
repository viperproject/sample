/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.cloud.{AbstractEventGraph, CloudQueryWrapper, CloudUpdateWrapper}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.domain.{FieldIdentifier, MultiValExpression}

/**
  * The super type of all other TouchDevelop types
  *
  * @author Lucas Brutschy
  */
trait AAny extends NativeMethodSemantics with RichExpressionImplicits with TouchType {

  // =================
  //
  // Implicit conversion to reference
  //
  //
  case class RefConversionSemantics() extends ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet,
                                                 method: ApiMember,
                                                 parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      val (objs,strs) = this0.ids.getNonTopUnsafe.collect { case f:FieldIdentifier => (f.obj,f.field)}.unzip
      val typ = GRef(this0.typ.asInstanceOf[AAny])
      if (objs.nonEmpty && strs.nonEmpty) {
        val objExpr = ExpressionSet(TString, SetDomain.Default.Inner(objs.toSet))
        val strExpr = ExpressionSet(TString, SetDomain.Default.Inner(strs.map(Constant(_, TString))))
        New[S](typ, Map(typ.field__receiver -> objExpr, typ.field__field -> strExpr))
      } else {
        Reporter.reportImpreciseSemantics("We could not determine the contents of a reference, this could be unsound" ,pp)
        Top[S](typ)
      }
    }
  }

  case class ReferenceConversionAndThenSemantics(thenApi: ApiMember) extends ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet,
                                                 method: ApiMember,
                                                 parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      val refState = member__ref.semantics.forwardSemantics[S](this0,member__ref,Nil)(pp,state)
      val newThis = refState.expr
      thenApi.semantics.forwardSemantics[S](newThis,method,parameters)(pp,refState)
    }
  }

  def member__add: ApiMember = ApiMember(
    name = "◈add",
    paramTypes = List(ApiParam(TNumber,isMutated = false)),
    thisType = ApiParam(this, isMutated = true),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Assign[S](this0,this0 + 1)
      }
    },Set(CloudEnabledModifier))
  )

  def member__test_and_set = ApiMember(
    name = "◈test and set",
    paramTypes = List(ApiParam(TString,isMutated = false)),
    thisType = ApiParam(this, isMutated = true),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        If[S](this0 equal String(""), { x: S =>
          Assign[S](this0,parameters.head)
        }, { x: S =>
          Skip[S]
        })
      }
    },Set(CloudEnabledModifier))
  )

  /** Never used: Checks if value is confirmed */
  def member__confirmed = ApiMember(
    name = "◈confirmed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = CloudQueryWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Top[S](TBoolean)
      }
    },Set(CloudEnabledModifier))
  )

  /** Never used: Set the value of the reference */
  def member__set = ApiMember(
    name = "◈set",
    paramTypes = List(ApiParam(this)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Assign[S](this0,parameters.head)
      }
    },Set(CloudEnabledModifier))
  )

  def member__clear = ApiMember(
    name = "◈clear",
    paramTypes = Nil,
    thisType = ApiParam(this, isMutated = true),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Clear[S](this0)
      }
    },Set(CloudEnabledModifier))
  )

  def member__get = ApiMember(
    name = "◈get",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = this,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Return[S](this0)
      }
    }
  )
  def member__ref:ApiMember = ApiMember(
    name = "◈ref",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GRef(this),
    semantics = RefConversionSemantics()
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
        val res = Return[S](this0 equal Invalid(this0.typ, "")(pp))(state, pp)
        res
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
        for (l <- this0.toSetOrFail; r <- right.toSetOrFail) {
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
      member__ref.name -> member__ref,
      member__confirmed.name -> member__confirmed,
      member__add.name -> member__add,
      member__clear.name -> member__clear,
      member__get.name -> member__get,
      member__set.name -> member__set,
      member__test_and_set.name -> member__test_and_set
    )

  def isSingleton = false
  def isImmutable = true

  def possibleFields = Set.empty

  override def representedFields =
    if (TouchAnalysisParameters.get.libraryFieldPruning &&
      SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.nonEmpty) {
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
                                                 parameters: List[ExpressionSet], typeParameters: List[Type],
                                                 returnedType: Type, pp: ProgramPoint, state: S): Option[S] = {


    if (thisExpr.typ.asInstanceOf[TouchType].typeName == typeName) {

      if (state.isBottom) {
        return Some(state.bottom())
      }

      var curState = state

      // Check if the object or an argument can be invalid - in this case, we must produce an error
      if (operator != "is invalid" && operator != ":=" && operator != "," && thisExpr.typ.name != "code") {
        if (!thisExpr.typ.isStatic) {
          if (TouchAnalysisParameters.get.printValuesInWarnings)
            curState = Error(thisExpr equal Invalid(thisExpr.typ, "")(pp),
              operator, "Object (" + thisExpr + ") whose field/method is accessed might be invalid")(curState, pp)
          else
            curState = Error(thisExpr equal Invalid(thisExpr.typ, "")(pp),
              operator, "Object whose field/method is accessed might be invalid")(curState, pp)
        }
        for (param <- parameters) {
          if (TouchAnalysisParameters.get.printValuesInWarnings)
            curState = Error(param equal Invalid(param.typ, "")(pp),
              operator, "Parameter (" + param + ") might be invalid")(curState, pp)
          else
            curState = Error(param equal Invalid(param.typ, "")(pp),
              operator, "Parameter might be invalid") (curState, pp)
        }
      }

      if (curState.isBottom) {
        return Some(state.bottom())
      }

      AbstractEventGraph.record(operator,thisExpr,parameters,state,pp)

      val res = forwardSemantics(thisExpr, operator, parameters, returnedType.asInstanceOf[TouchType])(pp, curState)

      if (SystemParameters.DEBUG) assert(returnedType == TNothing || res.expr.typ == returnedType)

      Some(res)

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
  def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet],
                                      returnedType: TouchType)
                                     (implicit pp: ProgramPoint, state: S): S = method match {

    case _ =>

      matchRecordCalls[S](this0, parameters, method, returnedType)

  }

  private def matchRecordCalls[S <: State[S]](this0: RichExpression, parameters: List[ExpressionSet], method: String,
                                              returnedType:TouchType)(implicit state: S, pp: ProgramPoint): S = {

    val arguments = this0.typ :: (parameters map (_.typ))
    SystemParameters.compiler.asInstanceOf[TouchCompiler].getMethod(method, arguments) match {
      case Some(mdecl) =>
        MethodSummaries.collect(pp, mdecl, state, parameters)
      case _ =>
        matchFields[S](this0,parameters,method,returnedType)
    }

  }

  private def matchFields[S <: State[S]](this0: RichExpression, parameters: List[ExpressionSet], method: String,
                                         returnedType:TouchType)(implicit state: S, pp: ProgramPoint): S = {

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

                if (SystemParameters.DEBUG) {
                  if ((this0.typ.possibleFields -- representedFields).exists(_.getName == method)) {
                    println("Looks like library fragment analysis missed "+this0.typ+"->"+method)
                  }
                }
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

  def Clear[S <: State[S]](this0:RichExpression)(implicit state:S,pp:ProgramPoint) = {
    Assign[S](this0,Default(this0.typ,"Value got cleared"))
  }

}
