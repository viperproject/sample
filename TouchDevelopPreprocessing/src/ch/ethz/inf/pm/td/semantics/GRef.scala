
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichExpression}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.defsemantics.Default_GRef
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.cloud.CloudUpdateWrapper

/**
 * Customizes the abstract semantics of Ref
 *
 * A reference to a value
 *
 * @author Lucas Brutschy
 */
case class GRef (TT:AAny) extends Default_GRef {

  lazy val field__identifier = ApiField("*identifier",TString)

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
        Assign[S](DeRef[S](this0),this0 + 1)
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
        If[S](this0 equal String(""), { x: S =>
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
    semantics = DefaultSemantics
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
    paramTypes = List(ApiParam(TNumber,isMutated = false)),
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

  override lazy val possibleFields = super.possibleFields ++ Set(
    field__identifier
  )

  /**
    * Converts a stored string back into a reference
    */
  def DeRef[S <: State[S]](this0:RichExpression)(implicit state:S, pp:ProgramPoint):ExpressionSet = {

    val exprs =
      EvalConstant[S](Field[S](this0,field__identifier)) match {
        case s:SetDomain.Default.Bottom[Constant] =>
          SetDomain.Default.Bottom[Expression]()
        case s:SetDomain.Default.Top[Constant] =>
          state.ids match {
            case IdentifierSet.Top =>
              SetDomain.Default.Top[Expression]()
            case IdentifierSet.Bottom =>
              SetDomain.Default.Bottom[Expression]()
            case IdentifierSet.Inner(x) =>
              val ids = for (id <- x if id.typ == TT) yield id
              if (ids.nonEmpty)
                SetDomain.Default.Inner[Expression](ids.toSet)
              else
                SetDomain.Default.Bottom[Expression]()
          }
        case s:SetDomain.Default.Inner[Constant] =>
          val identifiers =
            for (c <- s.value) yield {
              SRecords.lookupRef(c.constant) match {
                case Some(x) => x
                case None => throw TouchException("Tried to dereference reference, but got some invalid constant")
              }
            }
          SetDomain.Default.Inner[Expression](identifiers.toSet)
      }
    new ExpressionSet(TT,exprs)
  }

}
          
