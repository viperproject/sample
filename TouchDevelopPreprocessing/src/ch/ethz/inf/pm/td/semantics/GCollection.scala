
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.defsemantics.Default_GCollection
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._

/**
 * Customizes the abstract semantics of Collection
 *
 * A collection of objects
 *
 * @author Lucas Brutschy
 */

case class GCollection (TT:AAny) extends Default_GCollection {

  override lazy val member_join = super.member_join.copy(semantics = ValidPureSemantics)

  override lazy val member_first = super.member_first.copy(semantics = new ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S) = {
      Return[S](At[S](this0,0))
    }
  })

  override lazy val member_last = super.member_last.copy(semantics = new ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S) = {
      Return[S](At[S](this0,Count[S](this0) - 1))
    }
  })

}
