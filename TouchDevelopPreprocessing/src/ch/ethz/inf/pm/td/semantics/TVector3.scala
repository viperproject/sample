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
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_TVector3
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TVector3 extends Default_TVector3 {

  lazy val field_x = ApiField("x", TNumber)
  lazy val field_y = ApiField("y", TNumber)
  lazy val field_z = ApiField("z", TNumber)

  override def mutedFields = super.mutedFields ++ List(field_x,field_y,field_z)

  override def possibleFields = super.possibleFields ++ List(field_x,field_y,field_z)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    /** Adds a vector */
    case "add" =>
      val List(other) = parameters // Vector3
      val x = Field[S](this0,TVector3.field_x) + Field[S](other,TVector3.field_x)
      val y = Field[S](this0,TVector3.field_y) + Field[S](other,TVector3.field_y)
      val z = Field[S](this0,TVector3.field_z) + Field[S](other,TVector3.field_z)
      New[S](TVector3,Map(
        TVector3.field_x -> x,
        TVector3.field_y -> y,
        TVector3.field_z -> z
      ))

    /** Restricts the vector in the specified range */
    case "clamp" =>
      val List(min,max) = parameters // Vector3,Vector3
      val x = Field[S](min,TVector3.field_x) ndToIncl Field[S](max,TVector3.field_x)
      val y = Field[S](min,TVector3.field_y) ndToIncl Field[S](max,TVector3.field_y)
      val z = Field[S](min,TVector3.field_z) ndToIncl Field[S](max,TVector3.field_z)
      // PRECISION: Here we are not using the original value of the vector.
      New[S](TVector3,Map(
        TVector3.field_x -> x,
        TVector3.field_y -> y,
        TVector3.field_z -> z
      ))

    /** Calculates the cross product with the other vector */
    case "cross" =>
      val List(other) = parameters // Vector3
      val thisX = Field[S](this0,TVector3.field_x)
      val thisY = Field[S](this0,TVector3.field_y)
      val thisZ = Field[S](this0,TVector3.field_z)
      val otherX = Field[S](other,TVector3.field_x)
      val otherY = Field[S](other,TVector3.field_y)
      val otherZ = Field[S](other,TVector3.field_z)
      val x = thisY * otherZ - thisZ * otherY
      val y = thisZ * otherX - thisX * otherZ
      val z = thisX * otherY - thisY * otherX
      New[S](TVector3,Map(
        TVector3.field_x -> x,
        TVector3.field_y -> y,
        TVector3.field_z -> z
      ))

    /** Gets the distance between the two vectors */
    case "distance" =>
      val List(other) = parameters // Vector3
      val x = Field[S](this0,TVector3.field_x) - Field[S](other,TVector3.field_x)
      val y = Field[S](this0,TVector3.field_y) - Field[S](other,TVector3.field_y)
      val z = Field[S](this0,TVector3.field_z) - Field[S](other,TVector3.field_z)
      val disSquare = x * x + y * y + z * z
      // PRECISION: Imprecise square root
      Return[S](0 ndToIncl disSquare, 0 ndToIncl 1)

    /** Gets the length of the vector */
    case "length" =>
      val thisX = Field[S](this0,TVector3.field_x)
      val thisY = Field[S](this0,TVector3.field_y)
      val thisZ = Field[S](this0,TVector3.field_z)
      val lenSquare = thisX * thisX + thisY * thisY + thisZ * thisZ
      // PRECISION: Imprecise square root
      Return[S](0 ndToIncl lenSquare,0 ndToIncl 1)

    /** Linear interpolation between two vectors */
    case "linear interpolation" =>
      val List(other,amount) = parameters // Vector3,Number
      val x = Field[S](this0,TVector3.field_x) + amount * Field[S](other,TVector3.field_x)
      val y = Field[S](this0,TVector3.field_y) + amount * Field[S](other,TVector3.field_y)
      val z = Field[S](this0,TVector3.field_z) + amount * Field[S](other,TVector3.field_z)
      New[S](TVector3,Map(
        TVector3.field_x -> x,
        TVector3.field_y -> y,
        TVector3.field_z -> z
      ))

    /** Multiplies component-wise with a vector */
    case "multiply" =>
      val List(other) = parameters // Vector3
      val x = Field[S](this0,TVector3.field_x) * Field[S](other,TVector3.field_x)
      val y = Field[S](this0,TVector3.field_y) * Field[S](other,TVector3.field_y)
      val z = Field[S](this0,TVector3.field_z) * Field[S](other,TVector3.field_z)
      New[S](TVector3,Map(
        TVector3.field_x -> x,
        TVector3.field_y -> y,
        TVector3.field_z -> z
      ))
    /** Returns a vector pointing in the opposite direction */
    case "negate" =>
      val x = 0 - Field[S](this0,TVector3.field_x)
      val y = 0 - Field[S](this0,TVector3.field_y)
      val z = 0 - Field[S](this0,TVector3.field_z)
      New[S](TVector3,Map(
        TVector3.field_x -> x,
        TVector3.field_y -> y,
        TVector3.field_z -> z
      ))

    /** Returns a vector of one unit pointing in the same direction as the original vector */
    case "normalize" =>
      // PRECISION: There is a more precise way to do this
      New[S](TVector3,Map(
        TVector3.field_x -> (-1 ndToIncl 1),
        TVector3.field_y -> (-1 ndToIncl 1),
        TVector3.field_z -> (-1 ndToIncl 1)
      ))

    /** Multiplies with a scaling factor */
    case "scale" =>
      val List(scalar) = parameters // Number
      val x = scalar * Field[S](this0,TVector3.field_x)
      val y = scalar * Field[S](this0,TVector3.field_y)
      val z = scalar * Field[S](this0,TVector3.field_z)
      New[S](TVector3,Map(
        TVector3.field_x -> x,
        TVector3.field_y -> y,
        TVector3.field_z -> z
      ))

    /** Subtracts another vector */
    case "subtract" =>
      val List(other) = parameters // Vector3
      val x = Field[S](this0,TVector3.field_x) - Field[S](other,TVector3.field_x)
      val y = Field[S](this0,TVector3.field_y) - Field[S](other,TVector3.field_y)
      val z = Field[S](this0,TVector3.field_z) - Field[S](other,TVector3.field_z)
      New[S](TVector3,Map(
        TVector3.field_x -> x,
        TVector3.field_y -> y,
        TVector3.field_z -> z
      ))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)
  }
}