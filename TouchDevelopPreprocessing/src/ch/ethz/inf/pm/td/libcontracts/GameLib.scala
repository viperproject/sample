/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.libcontracts

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichExpressionImplicits
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._

/**
 * @author Lucas Brutschy
 */
case object GameLib extends LibraryContract with RichExpressionImplicits {

  override val name = "game"

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet],
                                               returnedType: TouchType)(implicit pp: ProgramPoint, state: S) = {
    method match {

      case _ =>
        Top[S](returnedType)
//        super.forwardSemantics(this0,method,parameters,returnedType)

    }
  }
}