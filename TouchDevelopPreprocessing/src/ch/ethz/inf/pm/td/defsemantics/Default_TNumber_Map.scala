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
 * Specifies the abstract semantics of Number Map
 *
 * A map of numbers to numbers
 *
 * @author Lucas Brutschy
 */

trait Default_TNumber_Map extends AMap {

  lazy val typeName = TypeName("Number Map")
          
  def keyType = TNumber

  def valueType = TNumber

  /** Rarely used: Computes the average of the values */
  def member_avg = ApiMember(
    name = "avg",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Computes the maximum of the values */
  def member_max = ApiMember(
    name = "max",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Computes the minimum of the values */
  def member_min = ApiMember(
    name = "min",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Extracts the elements at indices between start (inclusive) and end (non-inclusive). */
  def member_slice = ApiMember(
    name = "slice",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber_Map,
    semantics = DefaultSemantics
  )

  /** Rarely used: Computes the sum of the values */
  def member_sum = ApiMember(
    name = "sum",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Updates any display of this map */
  def member_update_on_wall = ApiMember(
    name = "update on wall",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "avg" -> member_avg,
    "max" -> member_max,
    "min" -> member_min,
    "slice" -> member_slice,
    "sum" -> member_sum,
    "update on wall" -> member_update_on_wall
  )
            

}
          
