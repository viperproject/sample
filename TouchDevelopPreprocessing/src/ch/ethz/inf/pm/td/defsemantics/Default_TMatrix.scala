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
 * Specifies the abstract semantics of Matrix
 *
 * A 2D matrix of numbers
 *
 * @author Lucas Brutschy
 */

trait Default_TMatrix extends AMutableLinearCollection {

  lazy val typeName = TypeName("Matrix")
          
  def keyType = TNumber

  def valueType = TNumber

  /** Never used: Creates a deep copy of the matrix. */
  def member_clone = ApiMember(
    name = "clone",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMatrix,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the number of columns */
  def member_column_count = ApiMember(
    name = "column count",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Copies the content from ``other`` starting at position ``row`` and ``column`` */
  def member_copy_from = ApiMember(
    name = "copy from",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TMatrix)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the value at a given location. Returns invalid if outside of the array dimensions */
  def member_item = ApiMember(
    name = "item",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Computes the maximum of the values */
  def member_max = ApiMember(
    name = "max",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Computes the minimum of the values */
  def member_min = ApiMember(
    name = "min",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns a matrix resulting from multiply each element in the matrices. The size of both matrices must match. */
  def member_multiply = ApiMember(
    name = "multiply",
    paramTypes = List(ApiParam(TMatrix)),
    thisType = ApiParam(this),
    returnType = TMatrix,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the matrix negated. */
  def member_negate = ApiMember(
    name = "negate",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMatrix,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the number of rows */
  def member_row_count = ApiMember(
    name = "row count",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns a copy of the matrix scaled by factor. */
  def member_scale = ApiMember(
    name = "scale",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TMatrix,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the value at a particular position. The matrix  will be expanded if the position falls outside the boundaries. */
  def member_set_item = ApiMember(
    name = "set item",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Returns a matrix resulting from subtracting b from this matrix. The size of both matrices must match. */
  def member_subtract = ApiMember(
    name = "subtract",
    paramTypes = List(ApiParam(TMatrix)),
    thisType = ApiParam(this),
    returnType = TMatrix,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the string representation of the matrix */
  def member_to_string = ApiMember(
    name = "to string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the transposed matrix. */
  def member_transpose = ApiMember(
    name = "transpose",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMatrix,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "clone" -> member_clone,
    "column count" -> member_column_count,
    "copy from" -> member_copy_from,
    "item" -> member_item,
    "max" -> member_max,
    "min" -> member_min,
    "multiply" -> member_multiply,
    "negate" -> member_negate,
    "row count" -> member_row_count,
    "scale" -> member_scale,
    "set item" -> member_set_item,
    "subtract" -> member_subtract,
    "to string" -> member_to_string,
    "transpose" -> member_transpose
  )
            

}
          
