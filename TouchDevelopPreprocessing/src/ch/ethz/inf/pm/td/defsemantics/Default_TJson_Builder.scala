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
 * Specifies the abstract semantics of Json Builder
 *
 * A json data structure builder
 *
 * @author Lucas Brutschy
 */

trait Default_TJson_Builder extends AMap {

  lazy val typeName = TypeName("Json Builder")
          
  def keyType = TString

  def valueType = TJson_Builder

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "add boolean" -> member_add_boolean,
    "add builder" -> member_add_builder,
    "add null" -> member_add_null,
    "add number" -> member_add_number,
    "add string" -> member_add_string,
    "add" -> member_add,
    "boolean" -> member_boolean,
    "clone" -> member_clone,
    "contains key" -> member_contains_key,
    "copy from" -> member_copy_from,
    "field" -> member_field,
    "kind" -> member_kind,
    "number" -> member_number,
    "remove at" -> member_remove_at,
    "remove field" -> member_remove_field,
    "serialize" -> member_serialize,
    "set boolean" -> member_set_boolean,
    "set builder" -> member_set_builder,
    "set field null" -> member_set_field_null,
    "set field" -> member_set_field,
    "set number" -> member_set_number,
    "set picture" -> member_set_picture,
    "set sound" -> member_set_sound,
    "set string" -> member_set_string,
    "string" -> member_string,
    "time" -> member_time,
    "to boolean" -> member_to_boolean,
    "to collection" -> member_to_collection,
    "to number" -> member_to_number,
    "to string" -> member_to_string,
    "to time" -> member_to_time
  )

  /** Never used: Adds a boolean to the array. */
  def member_add_boolean = ApiMember(
    name = "add boolean",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Add a reference to JsonBuilder to the array. */
  def member_add_builder = ApiMember(
    name = "add builder",
    paramTypes = List(ApiParam(TJson_Builder)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a null value to the array. */
  def member_add_null = ApiMember(
    name = "add null",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a number to the array. */
  def member_add_number = ApiMember(
    name = "add number",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a string to the array. */
  def member_add_string = ApiMember(
    name = "add string",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a value to the array. */
  def member_add = ApiMember(
    name = "add",
    paramTypes = List(ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a field value as a boolean */
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
    returnType = TJson_Builder,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if the key exists */
  def member_contains_key = ApiMember(
    name = "contains key",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Copy all fields from given JSON object */
  def member_copy_from = ApiMember(
    name = "copy from",
    paramTypes = List(ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a value by name */
  def member_field = ApiMember(
    name = "field",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TJson_Builder,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a json kind (string, number, object, array, boolean, null) */
  def member_kind = ApiMember(
    name = "kind",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a field value as a number */
  def member_number = ApiMember(
    name = "number",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Removes the i-th json value */
  def member_remove_at = ApiMember(
    name = "remove at",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Deletes named field */
  def member_remove_field = ApiMember(
    name = "remove field",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
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

  /** Never used: Sets the boolean value. */
  def member_set_boolean = ApiMember(
    name = "set boolean",
    paramTypes = List(ApiParam(TString), ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the field the the reference to JsonBuilder. */
  def member_set_builder = ApiMember(
    name = "set builder",
    paramTypes = List(ApiParam(TString), ApiParam(TJson_Builder)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the field value as null. */
  def member_set_field_null = ApiMember(
    name = "set field null",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the field value. */
  def member_set_field = ApiMember(
    name = "set field",
    paramTypes = List(ApiParam(TString), ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the number value. */
  def member_set_number = ApiMember(
    name = "set number",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the Picture value as a data uri. */
  def member_set_picture = ApiMember(
    name = "set picture",
    paramTypes = List(ApiParam(TString), ApiParam(TPicture), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the Sound value as a data uri. */
  def member_set_sound = ApiMember(
    name = "set sound",
    paramTypes = List(ApiParam(TString), ApiParam(TSound)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the string value. */
  def member_set_string = ApiMember(
    name = "set string",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a field value as a string */
  def member_string = ApiMember(
    name = "string",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the field value as a time */
  def member_time = ApiMember(
    name = "time",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Never used: Converts to a boolean (type must be boolean) */
  def member_to_boolean = ApiMember(
    name = "to boolean",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Converts to a collection of JsonBuilders (type must be array) */
  def member_to_collection = ApiMember(
    name = "to collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TJson_Builder),
    semantics = DefaultSemantics
  )

  /** Never used: Converts to a number (type must be number) */
  def member_to_number = ApiMember(
    name = "to number",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Converts to a string (type must be string) */
  def member_to_string = ApiMember(
    name = "to string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Converts and parses to a date time (type must be string) */
  def member_to_time = ApiMember(
    name = "to time",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )
            

}
          
