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
 * Specifies the abstract semantics of DateTime
 *
 * A combination of date and time
 *
 * @author Lucas Brutschy
 */

trait Default_TDateTime extends AAny {

  lazy val typeName = TypeName("DateTime")
          
  /** Frequently used: Returns a date that adds the specified number of days to the value of this instance. */
  def member_add_days = ApiMember(
    name = "add days",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Rarely used: Returns a date that adds the specified number of hours to the value of this instance. */
  def member_add_hours = ApiMember(
    name = "add hours",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns a date that adds the specified number of milliseconds to the value of this instance. */
  def member_add_milliseconds = ApiMember(
    name = "add milliseconds",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns a date that adds the specified number of minutes to the value of this instance. */
  def member_add_minutes = ApiMember(
    name = "add minutes",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] Returns a date that adds the specified number of months to the value of this instance. */
  def member_add_months = ApiMember(
    name = "add months",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns a date that adds the specified number of seconds to the value of this instance. */
  def member_add_seconds = ApiMember(
    name = "add seconds",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] Returns a date that adds the specified number of years to the value of this instance. */
  def member_add_years = ApiMember(
    name = "add years",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the date */
  def member_date = ApiMember(
    name = "date",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the day of the month */
  def member_day = ApiMember(
    name = "day",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Converts into text that describes the elapsed time in a friendly way. */
  def member_from_now = ApiMember(
    name = "from now",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Compares dates for greater or equal */
  def member_greater_or_equal = ApiMember(
    name = "greater or equal",
    paramTypes = List(ApiParam(TDateTime)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Compares dates for greater */
  def member_greater = ApiMember(
    name = "greater",
    paramTypes = List(ApiParam(TDateTime)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the hour */
  def member_hour = ApiMember(
    name = "hour",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Compares dates for less or equal */
  def member_less_or_equals = ApiMember(
    name = "less or equals",
    paramTypes = List(ApiParam(TDateTime)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: Compares dates for less */
  def member_less = ApiMember(
    name = "less",
    paramTypes = List(ApiParam(TDateTime)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the millisecond */
  def member_millisecond = ApiMember(
    name = "millisecond",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the milliseconds elapsed since January 1, 1970 UTC; same as getTime() in JavaScript. */
  def member_milliseconds_since_epoch = ApiMember(
    name = "milliseconds since epoch",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the minute */
  def member_minute = ApiMember(
    name = "minute",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the month */
  def member_month = ApiMember(
    name = "month",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Compares dates for disequality */
  def member_not_equals = ApiMember(
    name = "not equals",
    paramTypes = List(ApiParam(TDateTime)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the second */
  def member_second = ApiMember(
    name = "second",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Computes the difference between date-times in seconds */
  def member_subtract = ApiMember(
    name = "subtract",
    paramTypes = List(ApiParam(TDateTime)),
    thisType = ApiParam(this),
    returnType = TNumber,
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

  /** Rarely used: [**not implemented**] Converts to the local time */
  def member_to_local_time = ApiMember(
    name = "to local time",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Converts a dates to a string */
  def member_to_string = ApiMember(
    name = "to string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Converts coordinated universal time */
  def member_to_universal_time = ApiMember(
    name = "to universal time",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the day of the week (sunday = 0, monday = 1, ... saturday = 6) */
  def member_week_day = ApiMember(
    name = "week day",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the day of the year between 1 and 366 */
  def member_year_day = ApiMember(
    name = "year day",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the year */
  def member_year = ApiMember(
    name = "year",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add days" -> member_add_days,
    "add hours" -> member_add_hours,
    "add milliseconds" -> member_add_milliseconds,
    "add minutes" -> member_add_minutes,
    "add months" -> member_add_months,
    "add seconds" -> member_add_seconds,
    "add years" -> member_add_years,
    "date" -> member_date,
    "day" -> member_day,
    "from now" -> member_from_now,
    "greater or equal" -> member_greater_or_equal,
    "greater" -> member_greater,
    "hour" -> member_hour,
    "less or equals" -> member_less_or_equals,
    "less" -> member_less,
    "millisecond" -> member_millisecond,
    "milliseconds since epoch" -> member_milliseconds_since_epoch,
    "minute" -> member_minute,
    "month" -> member_month,
    "not equals" -> member_not_equals,
    "second" -> member_second,
    "subtract" -> member_subtract,
    "to json" -> member_to_json,
    "to local time" -> member_to_local_time,
    "to string" -> member_to_string,
    "to universal time" -> member_to_universal_time,
    "week day" -> member_week_day,
    "year day" -> member_year_day,
    "year" -> member_year
  )
            

}
          
