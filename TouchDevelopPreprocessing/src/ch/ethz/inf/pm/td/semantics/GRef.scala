
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.{ApiMember, ApiMemberSemantics, ApiParam, TouchException}
import ch.ethz.inf.pm.td.defsemantics.Default_GRef
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.domain.TouchStateInterface

/**
 * Customizes the abstract semantics of Ref
 *
 * A reference to a value
 *
 * @author Lucas Brutschy
 */
case class GRef (TT:AAny) extends Default_GRef {

  lazy val field__identifier = ApiField("*identifier",TString)

  override def member__get = ApiMember(
    name = "◈get",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TT,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {

        EvalConstant[S](Field[S](this0,field__identifier)) match {
          case s:SetDomain.Default.Bottom[Constant] =>
            state.bottom()
          case s:SetDomain.Default.Top[Constant] =>
            val tS = state.asInstanceOf[TouchStateInterface[_]]
            tS.ids match {
              case IdentifierSet.Top => Top[S](method.returnType)
              case IdentifierSet.Bottom => state.bottom()
              case IdentifierSet.Inner(x) =>
                val ids = for (id <- x if id.typ == method.returnType) yield id
                if (ids.nonEmpty)
                  Return[S](ExpressionSet(ids.toSeq))
                else
                  state.bottom()
            }
          case s:SetDomain.Default.Inner[Constant] =>
            val identifiers =
              for (c <- s.value) yield {
                SRecords.lookupRef(c.constant) match {
                  case Some(x) => x
                  case None => throw TouchException("Tried to dereference reference, but got some invalid constant")
                }
              }
            Return[S](ExpressionSet(identifiers.toSeq))
        }

      }
    }
  )

  override lazy val possibleFields = super.possibleFields ++ Set(
    field__identifier
  )

}
          
