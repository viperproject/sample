/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.{ApiField, RichExpressionSet}
import ch.ethz.inf.pm.td.cloud.{CloudQueryWrapper, CloudUpdateWrapper}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.defsemantics.Default_GRef

/**
 * Customizes the abstract semantics of Ref
 *
 * A reference to a value
 *
 * @author Lucas Brutschy
 */
case class GRef (TT:AAny) extends Default_GRef {

  lazy val field__receiver = ApiField("*receiver",TNothing)
  lazy val field__field = ApiField("*field",TString)
  override lazy val possibleFields = super.possibleFields ++ Set(
    field__receiver,
    field__field
  )

  override def member__ref:ApiMember = ApiMember(
    name = "◈ref",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = this,
    semantics = IdentitySemantics
  )

  override def member__add: ApiMember = ApiMember(
    name = "◈add",
    paramTypes = List(ApiParam(TNumber,isMutated = false)),
    thisType = ApiParam(this, isMutated = true),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Assign[S](DeRef[S](this0),DeRef[S](this0) + 1)
      }
    },Set(CloudEnabledModifier))
  )

  override def member__test_and_set = ApiMember(
    name = "◈test and set",
    paramTypes = List(ApiParam(TString,isMutated = false)),
    thisType = ApiParam(this, isMutated = true),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        If[S](DeRef[S](this0) equal String(""), { x: S =>
          Assign[S](DeRef[S](this0),parameters.head)
        }, { x: S =>
          Skip[S]
        })
      }
    },Set(CloudEnabledModifier))
  )

  /** Never used: Checks if value is confirmed */
  override def member__confirmed = ApiMember(
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
  override def member__set = ApiMember(
    name = "◈set",
    paramTypes = List(ApiParam(TT)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        Assign[S](DeRef[S](this0),parameters.head)
      }
    },Set(CloudEnabledModifier))
  )

  override def member__clear = ApiMember(
    name = "◈clear",
    paramTypes = Nil,
    thisType = ApiParam(this, isMutated = true),
    returnType = TNothing,
    semantics = CloudUpdateWrapper(new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        TT.Clear[S](DeRef[S](this0))
      }
    },Set(CloudEnabledModifier))
  )

  override def member__get = ApiMember(
    name = "◈get",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TT,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val res = Return[S](DeRef[S](this0))
        res
      }
    }
  )

  /**
    * Converts a stored string back into a reference
    */
  def DeRef[S <: State[S]](this0: RichExpressionSet)(implicit state: S, pp: ProgramPoint): ExpressionSet = {

    val botRes = ExpressionSet(TT,SetDomain.Default.Bottom[Expression]())
    val topRes = ExpressionSet(TT,SetDomain.Default.Top[Expression]())

    val receiver = Field[S](this0,field__receiver)

    val fields =
      EvalConstant[S](Field[S](this0,field__field)) match {
        case SetDomain.Default.Bottom() =>
          return botRes
        case SetDomain.Default.Top() =>
          return topRes
        case SetDomain.Default.Inner(xs) =>
          xs.map(_.constant)
      }

    val exprs =
      for (f <- fields) yield {
        state.getFieldValue(receiver.thisExpr,f,TT).expr
      }

    Lattice.bigLub(exprs)
  }

}
          
