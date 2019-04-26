/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.libcontracts

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichExpressionSetImplicits
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.semantics.{AAction, GRef}

/**
 * @author Lucas Brutschy
 */
case object FormsLib extends LibraryContract with RichExpressionSetImplicits {

  override val name = "forms"

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet],
                                               returnedType: TouchType)(implicit pp: ProgramPoint, state: S) = {
    method match {

      case _ =>

        var curState = state
        for (p  <- parameters) {
           p.typ match {
             case a:AAction =>
               curState = a.Enable[S](p)(curState,pp)
             case r@GRef(t) =>
               curState = SetToTop[S](r.DeRef[S](p))(curState,pp)
             case _ =>
               ()
           }
        }
        curState = Top[S](returnedType)(curState,pp)
        curState

    }
  }
}