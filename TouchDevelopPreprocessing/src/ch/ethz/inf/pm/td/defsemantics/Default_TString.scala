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
 * Specifies the abstract semantics of String
 *
 * A piece of text
 *
 * @author Lucas Brutschy
 */

trait Default_TString extends AAny {

  lazy val typeName = TypeName("String")
          
  /** Sometimes used: Gets the character at a specified index. Returns invalid if out of bounds. */
  def member_at = ApiMember(
    name = "at",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the charecter unicode value at a given index. Returns NaN if out of bounds */
  def member_code_at = ApiMember(
    name = "code at",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Compares two pieces of text */
  def member_compare = ApiMember(
    name = "compare",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Concatenates two pieces of text */
  def member_concat = ApiMember(
    name = "concat",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns a value indicating if the second string is contained */
  def member_contains = ApiMember(
    name = "contains",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Stores text in the clipboard */
  def member_copy_to_clipboard = ApiMember(
    name = "copy to clipboard",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns the number of characters */
  def member_count = ApiMember(
    name = "count",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Determines whether the ending matches the specified string */
  def member_ends_with = ApiMember(
    name = "ends with",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns the index of the first occurence if found starting at a given position */
  def member_index_of = ApiMember(
    name = "index of",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Inserts a string at a given position */
  def member_insert = ApiMember(
    name = "insert",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Frequently used: Indicates if the string is empty */
  def member_is_empty = ApiMember(
    name = "is empty",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates if the string matches a regular expression */
  def member_is_match_regex = ApiMember(
    name = "is match regex",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: Returns the index of the last occurence if found starting at a given position */
  def member_last_index_of = ApiMember(
    name = "last index of",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the groups from the matching the regex expression (pattern). Returns an empty collection if no matches. */
  def member_match = ApiMember(
    name = "match",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the strings matching the regex expression (pattern) */
  def member_matches = ApiMember(
    name = "matches",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns the string with characters removed starting at a given index */
  def member_remove = ApiMember(
    name = "remove",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Run `replacer` on every match of the regex */
  def member_replace_regex_with_converter = ApiMember(
    name = "replace regex with converter",
    paramTypes = List(ApiParam(TString), ApiParam(GString_Converter(GCollection(TString)))),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Replace every match of the regex according to the replacement string */
  def member_replace_regex = ApiMember(
    name = "replace regex",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns a given string with a replacement */
  def member_replace = ApiMember(
    name = "replace",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Shares the string (email, sms, facebook, social or '' to pick from a list) */
  def member_share = ApiMember(
    name = "share",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns a string collection that contains the substrings in this string that are delimited by elements of a specified string. */
  def member_split = ApiMember(
    name = "split",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Frequently used: Determines whether the beginning matches the specified string */
  def member_starts_with = ApiMember(
    name = "starts with",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns a substring given a start index and a length */
  def member_substring = ApiMember(
    name = "substring",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Parses the string as a boolean */
  def member_to_boolean = ApiMember(
    name = "to boolean",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Converts the first character into the character code number (unicode) */
  def member_to_character_code = ApiMember(
    name = "to character code",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Parses the string as a color. */
  def member_to_color = ApiMember(
    name = "to color",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Rarely used: Parses the string as a date and time. */
  def member_to_datetime = ApiMember(
    name = "to datetime",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Never used: Converts the value into a json data structure. */
  def member_to_json = ApiMember(
    name = "to json",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Rarely used: Parses the string as a geo coordinate. */
  def member_to_location = ApiMember(
    name = "to location",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLocation,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns a copy of this string converted to lowercase, using the casing rules of the current culture. */
  def member_to_lower_case = ApiMember(
    name = "to lower case",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Frequently used: Parses the string as a number */
  def member_to_number = ApiMember(
    name = "to number",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Parses the string as a time (12:30:12) and returns the number of seconds. */
  def member_to_time = ApiMember(
    name = "to time",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Use ``to_character_code`` instead */
  def member_to_unicode = ApiMember(
    name = "to unicode",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns a copy of this string converted to uppercase, using the casing rules of the current culture. */
  def member_to_upper_case = ApiMember(
    name = "to upper case",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Removes all trailing occurrences of a set of characters specified in a string from the current string. */
  def member_trim_end = ApiMember(
    name = "trim end",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Trims the string at the given length and adds ``...`` if necessary */
  def member_trim_overflow = ApiMember(
    name = "trim overflow",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Removes all leading occurrences of a set of characters specified in a string from the current string. */
  def member_trim_start = ApiMember(
    name = "trim start",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Removes all leading and trailing occurrences of a set of characters specified in a string from the current string. */
  def member_trim = ApiMember(
    name = "trim",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "at" -> member_at,
    "code at" -> member_code_at,
    "compare" -> member_compare,
    "concat" -> member_concat,
    "contains" -> member_contains,
    "copy to clipboard" -> member_copy_to_clipboard,
    "count" -> member_count,
    "ends with" -> member_ends_with,
    "index of" -> member_index_of,
    "insert" -> member_insert,
    "is empty" -> member_is_empty,
    "is match regex" -> member_is_match_regex,
    "last index of" -> member_last_index_of,
    "match" -> member_match,
    "matches" -> member_matches,
    "remove" -> member_remove,
    "replace regex with converter" -> member_replace_regex_with_converter,
    "replace regex" -> member_replace_regex,
    "replace" -> member_replace,
    "share" -> member_share,
    "split" -> member_split,
    "starts with" -> member_starts_with,
    "substring" -> member_substring,
    "to boolean" -> member_to_boolean,
    "to character code" -> member_to_character_code,
    "to color" -> member_to_color,
    "to datetime" -> member_to_datetime,
    "to json" -> member_to_json,
    "to location" -> member_to_location,
    "to lower case" -> member_to_lower_case,
    "to number" -> member_to_number,
    "to time" -> member_to_time,
    "to unicode" -> member_to_unicode,
    "to upper case" -> member_to_upper_case,
    "trim end" -> member_trim_end,
    "trim overflow" -> member_trim_overflow,
    "trim start" -> member_trim_start,
    "trim" -> member_trim
  )
            

}
          
