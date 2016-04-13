
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Camera
 *
 * The front or back camera
 *
 * @author Lucas Brutschy
 */

trait Default_TCamera extends AAny {

  lazy val typeName = TypeName("Camera")
          
  /** Rarely used: [**not implemented**] Gets the height of the camera image in pixels. */
  def member_height = ApiMember(
    name = "height",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if this camera is in front of the phone; false if this is the primary (back) camera. */
  def member_is_front = ApiMember(
    name = "is front",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Takes a low quality picture from the camera. */
  def member_preview = ApiMember(
    name = "preview",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Gets the width of the camera image in pixels. */
  def member_width = ApiMember(
    name = "width",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "height" -> member_height,
    "is front" -> member_is_front,
    "preview" -> member_preview,
    "width" -> member_width
  )
            

}
          
