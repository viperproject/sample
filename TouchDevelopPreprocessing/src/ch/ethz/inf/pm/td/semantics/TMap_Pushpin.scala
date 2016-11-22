/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TMap_Pushpin
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Map Pushpin
 *
 * A map pushpin
 *
 * @author Lucas Brutschy
 */

object TMap_Pushpin extends Default_TMap_Pushpin {

  /** Gets the pushpin geo location */
  lazy val field_location = ApiField("location", TLocation)

  /** Shows or hides the pushpin */
  lazy val field_visible = ApiField("visible", TBoolean)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_tap_handler = ApiField("tap handler", TPosition_Action)

  override def member_on_tap =
    super.member_on_tap.copy(semantics = AAction.EnableSemantics(TMap_Pushpin.field_tap_handler))

  override def possibleFields = super.possibleFields ++ List(field_location, field_visible, field_tap_handler)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
