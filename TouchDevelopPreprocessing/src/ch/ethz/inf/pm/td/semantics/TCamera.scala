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
import ch.ethz.inf.pm.td.defsemantics.Default_TCamera
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Camera
 *
 * The front or back camera
 *
 * @author Lucas Brutschy
 */ 

object TCamera extends Default_TCamera {

  /** Gets the height of the camera image in pixels. */
  lazy val field_height = ApiField("height", TNumber)

  /** Gets the width of the camera image in pixels. */
  lazy val field_width = ApiField("width", TNumber)

  /** Indicates if this camera is in front of the phone; false if this is the primary (back) camera. */
  lazy val field_is_front = ApiField("is front", TBoolean)

  override def possibleFields = super.possibleFields ++ List(field_height, field_is_front, field_width)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Takes a low quality picture from the camera. */
    case "preview" =>
      Top[S](TPicture)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
