
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
 * Specifies the abstract semantics of Collection
 *
 * A collection of objects
 *
 * @author Lucas Brutschy
 */

trait Default_GCollection extends AMutableLinearCollection {

  def TT:AAny
           

  lazy val typeName = TypeName("Collection", List(TT.typeName))
          
  def keyType = TNumber

  def valueType = TT

  /** Never used: Computes the average of the key of the elements in the collection */
  def member_avg_of = ApiMember(
    name = "avg of",
    paramTypes = List(ApiParam(GNumber_Converter(TT))),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Computes the average of the values */
  def member_avg = ApiMember(
    name = "avg",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the identifier of the next set of items (if any) */
  def member_continuation = ApiMember(
    name = "continuation",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the first element if any */
  def member_first = ApiMember(
    name = "first",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TT,
    semantics = DefaultSemantics
  )

  /** Never used: Concatenates the separator and items into a string */
  def member_join = ApiMember(
    name = "join",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the last element if any */
  def member_last = ApiMember(
    name = "last",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TT,
    semantics = DefaultSemantics
  )

  /** Never used: Applies `converter` on all elements of the input collection and returns a collection of results */
  def member_map_to = ApiMember(
    name = "map to",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TUnfinished_Type,
    semantics = DefaultSemantics
  )

  /** Never used: Computes the maximum of the key of the elements in the collection */
  def member_max_of = ApiMember(
    name = "max of",
    paramTypes = List(ApiParam(GNumber_Converter(TT))),
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

  /** Never used: Computes the minimum of the key of the elements in the collection */
  def member_min_of = ApiMember(
    name = "min of",
    paramTypes = List(ApiParam(GNumber_Converter(TT))),
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

  /** Never used: Returns a collection sorted using specified comparison key */
  def member_ordered_by_string = ApiMember(
    name = "ordered by string",
    paramTypes = List(ApiParam(GString_Converter(TT))),
    thisType = ApiParam(this),
    returnType = GCollection(TT),
    semantics = DefaultSemantics
  )

  /** Never used: Returns a collection sorted using specified comparison key */
  def member_ordered_by = ApiMember(
    name = "ordered by",
    paramTypes = List(ApiParam(GNumber_Converter(TT))),
    thisType = ApiParam(this),
    returnType = GCollection(TT),
    semantics = DefaultSemantics
  )

  /** Never used: Ask user to pick an entry from this collection */
  def member_pick_entry = ApiMember(
    name = "pick entry",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TT,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the identifier of the next set of items */
  def member_set_continuation = ApiMember(
    name = "set continuation",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Returns a slice of the collection starting at `start`, and ends at, but does not include, the `end`. */
  def member_slice = ApiMember(
    name = "slice",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = GCollection(TT),
    semantics = DefaultSemantics
  )

  /** Never used: Sorts from the newest to oldest */
  def member_sort_by_date = ApiMember(
    name = "sort by date",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sorts the places by distance to the location */
  def member_sort_by_distance = ApiMember(
    name = "sort by distance",
    paramTypes = List(ApiParam(TLocation)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Returns a collection sorted using specified `comparison` function */
  def member_sorted = ApiMember(
    name = "sorted",
    paramTypes = List(ApiParam(GComparison(TT))),
    thisType = ApiParam(this),
    returnType = GCollection(TT),
    semantics = DefaultSemantics
  )

  /** Never used: Computes the sum of the key of the elements in the collection */
  def member_sum_of = ApiMember(
    name = "sum of",
    paramTypes = List(ApiParam(GNumber_Converter(TT))),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Computes the sum of the values */
  def member_sum = ApiMember(
    name = "sum",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns a collection with the `count` first elements if any. */
  def member_take = ApiMember(
    name = "take",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = GCollection(TT),
    semantics = DefaultSemantics
  )

  /** Never used: Returns a collections of elements that satisfy the filter `condition` */
  def member_where = ApiMember(
    name = "where",
    paramTypes = List(ApiParam(GPredicate(TT))),
    thisType = ApiParam(this),
    returnType = GCollection(TT),
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "avg of" -> member_avg_of,
    "avg" -> member_avg,
    "continuation" -> member_continuation,
    "first" -> member_first,
    "join" -> member_join,
    "last" -> member_last,
    "map to" -> member_map_to,
    "max of" -> member_max_of,
    "max" -> member_max,
    "min of" -> member_min_of,
    "min" -> member_min,
    "ordered by string" -> member_ordered_by_string,
    "ordered by" -> member_ordered_by,
    "pick entry" -> member_pick_entry,
    "set continuation" -> member_set_continuation,
    "slice" -> member_slice,
    "sort by date" -> member_sort_by_date,
    "sort by distance" -> member_sort_by_distance,
    "sorted" -> member_sorted,
    "sum of" -> member_sum_of,
    "sum" -> member_sum,
    "take" -> member_take,
    "where" -> member_where
  )
            

}
          
