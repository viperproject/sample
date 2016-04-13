
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiMember, ApiMemberSemantics}
import ch.ethz.inf.pm.td.defsemantics.Default_GRef

/**
 * Customizes the abstract semantics of Ref
 *
 * A reference to a value
 *
 * @author Lucas Brutschy
 */
case class GRef (TT:AAny) extends Default_GRef {

  override def member__add = super.member__add.copy(
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        DefaultSemantics.forwardSemantics(this0,method,parameters)
      }
    }
  )

  lazy val field__get = ApiField("◈get",TT)
  lazy val field__confirmed = ApiField("◈confirmed",TBoolean)

  override lazy val possibleFields = super.possibleFields ++ Set(
    field__get,
    field__confirmed
  )

}
          
