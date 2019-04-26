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
 * Specifies the abstract semantics of Json Object
 *
 * A json data structure.
 *
 * @author Lucas Brutschy
 */

trait Default_TJson_Object extends AMap {

  lazy val typeName = TypeName("Json Object")
          
  def keyType = TNumber

  def valueType = TJson_Object

  /** Rarely used: Gets a field value as a boolean */
  def member_boolean = ApiMember(
    name = "boolean",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a deep copy clone of the object */
  def member_clone = ApiMember(
    name = "clone",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Rarely used: Indicates if the key exists */
  def member_contains_key = ApiMember(
    name = "contains key",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets a value by name */
  def member_field = ApiMember(
    name = "field",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: Create a string formatted for easy readability */
  def member_format = ApiMember(
    name = "format",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets a json kind (string, number, object, array, boolean, null) */
  def member_kind = ApiMember(
    name = "kind",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets a field value as a number */
  def member_number = ApiMember(
    name = "number",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Stringify the current JSON object */
  def member_serialize = ApiMember(
    name = "serialize",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets a field value as a string */
  def member_string = ApiMember(
    name = "string",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the field value as a time */
  def member_time = ApiMember(
    name = "time",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Rarely used: Converts to a boolean (type must be boolean) */
  def member_to_boolean = ApiMember(
    name = "to boolean",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Converts to a collection of JsonObjects (type must be array) */
  def member_to_collection = ApiMember(
    name = "to collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TJson_Object),
    semantics = DefaultSemantics
  )

  /** Never used: Copy current JSON object into a Json Builder so it can be modified */
  def member_to_json_builder = ApiMember(
    name = "to json builder",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Builder,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Converts to a number (type must be number) */
  def member_to_number = ApiMember(
    name = "to number",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Converts to a string (type must be string) */
  def member_to_string = ApiMember(
    name = "to string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Converts and parses to a date time (type must be string) */
  def member_to_time = ApiMember(
    name = "to time",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "boolean" -> member_boolean,
    "clone" -> member_clone,
    "contains key" -> member_contains_key,
    "field" -> member_field,
    "format" -> member_format,
    "kind" -> member_kind,
    "number" -> member_number,
    "serialize" -> member_serialize,
    "string" -> member_string,
    "time" -> member_time,
    "to boolean" -> member_to_boolean,
    "to collection" -> member_to_collection,
    "to json builder" -> member_to_json_builder,
    "to number" -> member_to_number,
    "to string" -> member_to_string,
    "to time" -> member_to_time
  )
            

}
          
