package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TDateTime {

  val typName = "DateTime"
  val typ = TouchType(typName,isSingleton = false)

}

class TDateTime extends Any {

  def getTypeName = TDateTime.typName

  def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Returns a date that adds the specified number of days to the value of this instance. */
    case "add_days" =>
      val List(days) = parameters // Number
      New[S](TDateTime.typ) // TODO

    /** Returns a date that adds the specified number of hours to the value of this instance. */
    case "add_hours" =>
      val List(hours) = parameters // Number
      New[S](TDateTime.typ) // TODO

    /** Returns a date that adds the specified number of milliseconds to the value of this instance. */
    case "add_milliseconds" =>
      val List(milliseconds) = parameters // Number
      New[S](TDateTime.typ) // TODO

    /** Returns a date that adds the specified number of minutes to the value of this instance. */
    case "add_minutes" =>
      val List(minutes) = parameters // Number
      New[S](TDateTime.typ) // TODO

    /** Returns a date that adds the specified number of months to the value of this instance. */
    case "add_months" =>
      val List(months) = parameters // Number
      New[S](TDateTime.typ) // TODO

    /** Returns a date that adds the specified number of seconds to the value of this instance. */
    case "add_seconds" =>
      val List(seconds) = parameters // Number
      New[S](TDateTime.typ) // TODO

    /** Returns a date that adds the specified number of years to the value of this instance. */
    case "add_years" =>
      val List(years) = parameters // Number
      New[S](TDateTime.typ) // TODO

    /** Gets the date */
    case "date" =>
      New[S](TDateTime.typ) // TODO

    /** Gets the day of the month */
    case "day" =>
      New[S](TNumber.typ) // TODO

    /** Compares dates for equality */
    case "equals" =>
      val List(other) = parameters // DateTime
      New[S](TBoolean.typ) // TODO

    /** Compares dates for greater */
    case "greater" =>
      val List(other) = parameters // DateTime
      New[S](TBoolean.typ) // TODO

    /** Compares dates for greater or equal */
    case "greater_or_equal" =>
      val List(other) = parameters // DateTime
      New[S](TBoolean.typ) // TODO

    /** Gets the hour */
    case "hour" =>
      New[S](TNumber.typ) // TODO

    /** Returns true if the current instance is useless */
    case "is_invalid" =>
      New[S](TBoolean.typ) // TODO

    /** Compares dates for less */
    case "less" =>
      val List(other) = parameters // DateTime
      New[S](TBoolean.typ) // TODO

    /** Compares dates for less or equal */
    case "less_or_equals" =>
      val List(other) = parameters // DateTime
      New[S](TBoolean.typ) // TODO

    /** Gets the millisecond */
    case "millisecond" =>
      New[S](TNumber.typ) // TODO

    /** Gets the minute */
    case "minute" =>
      New[S](TNumber.typ) // TODO

    /** Gets the month */
    case "month" =>
      New[S](TNumber.typ) // TODO

    /** Compares dates for disequality */
    case "not_equals" =>
      val List(other) = parameters // DateTime
      New[S](TBoolean.typ) // TODO

    /** Prints the date to the wall */
    case "post_to_wall" =>
      Skip; // TODO

    /** Gets the second */
    case "second" =>
      New[S](TNumber.typ) // TODO

    /** Computes the difference between date-times in seconds */
    case "subtract" =>
      val List(value) = parameters // DateTime
      New[S](TNumber.typ) // TODO

    /** Converts to the local time */
    case "to_local_time" =>
      New[S](TDateTime.typ) // TODO

    /** Converts a dates to a string */
    case "to_string" =>
      New[S](TString.typ) // TODO

    /** Converts coordinated universal time */
    case "to_universal_time" =>
      New[S](TDateTime.typ) // TODO

    /** Gets the day of the week (sunday = 0, monday = 1, ... saturday = 6) */
    case "week_day" =>
      New[S](TNumber.typ) // TODO

    /** Gets the year */
    case "year" =>
      New[S](TNumber.typ) // TODO

    /** Gets the day of the year between 1 and 366 */
    case "year_day" =>
      New[S](TNumber.typ) // TODO

    case _ =>
      Unimplemented[S](this0.getType().toString+"."+method)

  }
}
