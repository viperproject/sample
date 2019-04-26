/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TObstacle
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Obstacle
 *
 * An obstacle on a board
 *
 * @author Lucas Brutschy
 */

object TObstacle extends Default_TObstacle {

  /** Color */
  lazy val field_color = ApiField("color", TColor)

  /** Sets the obstacle thickness */
  lazy val field_thickness = ApiField("thickness", TNumber)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_collision_handler = ApiField("collision handler", TSprite_Action)

  override def member_on_collision =
    super.member_on_collision.copy(semantics = AAction.EnableSemantics(TObstacle.field_collision_handler))

  override def possibleFields = super.possibleFields ++ List(field_color, field_thickness, field_collision_handler)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Delete the obstacle */
    case "delete" =>
      val List() = parameters // ignore
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
