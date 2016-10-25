/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.libcontracts

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichExpressionImplicits
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.semantics.{AAction, TNumber}

/**
 * @author Lucas Brutschy
 */
case object LayoutLib extends LibraryContract with RichExpressionImplicits {

  override val name = "layout"

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet],
                                               returnedType: TouchType)(implicit pp: ProgramPoint, state: S) = {
    method match {

      case _ =>

        var curState = state
        for (p  <- parameters) {
           p.typ match {
             case a:AAction =>
               curState = a.Run[S](p,List(Valid(TNumber)))(curState,pp)
             case _ =>
               ()
           }
        }
        curState = Top[S](returnedType)(curState,pp)
        curState

    }
  }
}