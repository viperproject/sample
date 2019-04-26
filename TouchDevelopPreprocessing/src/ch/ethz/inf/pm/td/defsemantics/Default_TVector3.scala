/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Vector3
 *
 * A 3D vector
 *
 * @author Lucas Brutschy
 */

trait Default_TVector3 extends AAny {

  lazy val typeName = TypeName("Vector3")
          
  /** Rarely used: Adds a vector */
  def member_add = ApiMember(
    name = "add",
    paramTypes = List(ApiParam(TVector3)),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Rarely used: Restricts the vector in the specified range */
  def member_clamp = ApiMember(
    name = "clamp",
    paramTypes = List(ApiParam(TVector3), ApiParam(TVector3)),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Never used: Calculates the cross product with the other vector */
  def member_cross = ApiMember(
    name = "cross",
    paramTypes = List(ApiParam(TVector3)),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the distance between the two vectors */
  def member_distance = ApiMember(
    name = "distance",
    paramTypes = List(ApiParam(TVector3)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the length of the vector */
  def member_length = ApiMember(
    name = "length",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Linear interpolation between two vectors */
  def member_linear_interpolation = ApiMember(
    name = "linear interpolation",
    paramTypes = List(ApiParam(TVector3), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Never used: Multiplies component-wise with a vector */
  def member_multiply = ApiMember(
    name = "multiply",
    paramTypes = List(ApiParam(TVector3)),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Rarely used: Returns a vector pointing in the opposite direction */
  def member_negate = ApiMember(
    name = "negate",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Never used: Returns a vector of one unit pointing in the same direction as the original vector */
  def member_normalize = ApiMember(
    name = "normalize",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Multiplies with a scaling factor */
  def member_scale = ApiMember(
    name = "scale",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Rarely used: Subtracts another vector */
  def member_subtract = ApiMember(
    name = "subtract",
    paramTypes = List(ApiParam(TVector3)),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Rarely used: Turns the vector into a string */
  def member_to_string = ApiMember(
    name = "to string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the x-component */
  def member_x = ApiMember(
    name = "x",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the y-component */
  def member_y = ApiMember(
    name = "y",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the z-component */
  def member_z = ApiMember(
    name = "z",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add" -> member_add,
    "clamp" -> member_clamp,
    "cross" -> member_cross,
    "distance" -> member_distance,
    "length" -> member_length,
    "linear interpolation" -> member_linear_interpolation,
    "multiply" -> member_multiply,
    "negate" -> member_negate,
    "normalize" -> member_normalize,
    "scale" -> member_scale,
    "subtract" -> member_subtract,
    "to string" -> member_to_string,
    "x" -> member_x,
    "y" -> member_y,
    "z" -> member_z
  )
            

}
          
