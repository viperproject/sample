/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{TopSemantics, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_TDateTime
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of DateTime
 *
 * A combination of date and time
 *
 * @author Lucas Brutschy
 */ 

object TDateTime extends Default_TDateTime {

  /** Gets the day of the month */
  lazy val field_day = ApiField("day", TNumber)

  /** Gets the hour */
  lazy val field_hour = ApiField("hour", TNumber)

  /** Gets the millisecond */
  lazy val field_millisecond = ApiField("millisecond", TNumber)

  /** Gets the minute */
  lazy val field_minute = ApiField("minute", TNumber)

  /** Gets the month */
  lazy val field_month = ApiField("month", TNumber)

  /** Gets the second */
  lazy val field_second = ApiField("second", TNumber)

  /** Gets the year */
  lazy val field_year = ApiField("year", TNumber)

  override lazy val member_greater_or_equal = super.member_greater_or_equal.copy(semantics = TopSemantics)
  override lazy val member_subtract = super.member_subtract.copy(semantics = TopSemantics)
  override lazy val member_greater = super.member_greater.copy(semantics = TopSemantics)

  override def possibleFields = super.possibleFields ++ List(
    field_day,
    field_hour,
    field_millisecond,
    field_minute,
    field_month,
    field_second,
    field_year
  )

  override def mutedFields = super.mutedFields ++ List(
    field_day,
    field_hour,
    field_millisecond,
    field_minute,
    field_month,
    field_second,
    field_year
  )

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Returns a date that adds the specified number of days to the value of this instance. */
    case "add days" =>
      val List(days) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_day -> (Field[S](this0,TDateTime.field_day) + days)
      ))

    /** Returns a date that adds the specified number of hours to the value of this instance. */
    case "add hours" =>
      val List(hours) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_hour -> (Field[S](this0,TDateTime.field_hour) + hours)
      ))

    /** Returns a date that adds the specified number of milliseconds to the value of this instance. */
    case "add milliseconds" =>
      val List(milliseconds) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_millisecond -> (Field[S](this0,TDateTime.field_millisecond) + milliseconds)
      ))

    /** Returns a date that adds the specified number of minutes to the value of this instance. */
    case "add minutes" =>
      val List(minutes) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_minute -> (Field[S](this0,TDateTime.field_minute) + minutes)
      ))

    /** Returns a date that adds the specified number of months to the value of this instance. */
    case "add months" =>
      val List(months) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_month -> (Field[S](this0,TDateTime.field_month) + months)
      ))

    /** Returns a date that adds the specified number of seconds to the value of this instance. */
    case "add seconds" =>
      val List(seconds) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_second -> (Field[S](this0,TDateTime.field_second) + seconds)
      ))

    /** Returns a date that adds the specified number of years to the value of this instance. */
    case "add years" =>
      val List(years) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_year -> (Field[S](this0,TDateTime.field_year) + years)
      ))

    /** Gets the date */
    case "date" =>
      New[S](TDateTime,Map(
        TDateTime.field_day -> Field[S](this0,TDateTime.field_day),
        TDateTime.field_month -> Field[S](this0,TDateTime.field_month),
        TDateTime.field_year -> Field[S](this0,TDateTime.field_year)
      ))

    /** Compares dates for equality */
    case "equals" =>
      val List(other) = parameters // DateTime
      Return[S](
        (Field[S](this0,TDateTime.field_day) equal Field[S](other,TDateTime.field_day)) &&
          (Field[S](this0,TDateTime.field_hour) equal Field[S](other,TDateTime.field_hour)) &&
          (Field[S](this0,TDateTime.field_millisecond) equal Field[S](other,TDateTime.field_millisecond)) &&
          (Field[S](this0,TDateTime.field_minute) equal Field[S](other,TDateTime.field_minute)) &&
          (Field[S](this0,TDateTime.field_month) equal Field[S](other,TDateTime.field_month)) &&
          (Field[S](this0,TDateTime.field_second) equal Field[S](other,TDateTime.field_second)) &&
          (Field[S](this0,TDateTime.field_year) equal Field[S](other,TDateTime.field_year))
      )

    /** Gets the day of the week (sunday = 0, monday = 1, ... saturday = 6) */
    case "week day" =>
      Return[S](0 ndToIncl 6)
    // DECLARATION AS FIELD:
    //   /** Gets the day of the week (sunday = 0, monday = 1, ... saturday = 6) */
    //   lazy val field_week_day = new TouchField("week day",TNumber.typeName)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
