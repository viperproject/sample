
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of DateTime
 *
 * A combination of date and time
 *
 * @author Lucas Brutschy
 */ 

object TDateTime {

  val typName = "DateTime"
  val typ = new TouchType(typName,isSingleton = false,List())

}

class TDateTime extends AAny {

  def getTyp = TDateTime.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Returns a date that adds the specified number of days to the value of this instance. */
    // case "add_days" => 
    //   val List(days) = parameters // Number
    //   Top[S](TDateTime.typ)

    /** Returns a date that adds the specified number of hours to the value of this instance. */
    // case "add_hours" => 
    //   val List(hours) = parameters // Number
    //   Top[S](TDateTime.typ)

    /** Returns a date that adds the specified number of milliseconds to the value of this instance. */
    // case "add_milliseconds" => 
    //   val List(milliseconds) = parameters // Number
    //   Top[S](TDateTime.typ)

    /** Returns a date that adds the specified number of minutes to the value of this instance. */
    // case "add_minutes" => 
    //   val List(minutes) = parameters // Number
    //   Top[S](TDateTime.typ)

    /** Returns a date that adds the specified number of months to the value of this instance. */
    // case "add_months" => 
    //   val List(months) = parameters // Number
    //   Top[S](TDateTime.typ)

    /** Returns a date that adds the specified number of seconds to the value of this instance. */
    // case "add_seconds" => 
    //   val List(seconds) = parameters // Number
    //   Top[S](TDateTime.typ)

    /** Returns a date that adds the specified number of years to the value of this instance. */
    // case "add_years" => 
    //   val List(years) = parameters // Number
    //   Top[S](TDateTime.typ)

    /** Gets the date */
    // case "date" => 
    //   Top[S](TDateTime.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the date */
    //   val field_date = new TouchField("date",TDateTime.typ)

    /** Gets the day of the month */
    // case "day" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the day of the month */
    //   val field_day = new TouchField("day",TNumber.typ)

    /** Compares dates for equality */
    // case "equals" => 
    //   val List(other) = parameters // DateTime
    //   Top[S](TBoolean.typ)

    /** Compares dates for greater */
    // case "greater" => 
    //   val List(other) = parameters // DateTime
    //   Top[S](TBoolean.typ)

    /** Compares dates for greater or equal */
    // case "greater_or_equal" => 
    //   val List(other) = parameters // DateTime
    //   Top[S](TBoolean.typ)

    /** Gets the hour */
    // case "hour" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the hour */
    //   val field_hour = new TouchField("hour",TNumber.typ)

    /** Compares dates for less */
    // case "less" => 
    //   val List(other) = parameters // DateTime
    //   Top[S](TBoolean.typ)

    /** Compares dates for less or equal */
    // case "less_or_equals" => 
    //   val List(other) = parameters // DateTime
    //   Top[S](TBoolean.typ)

    /** Gets the millisecond */
    // case "millisecond" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the millisecond */
    //   val field_millisecond = new TouchField("millisecond",TNumber.typ)

    /** Gets the minute */
    // case "minute" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the minute */
    //   val field_minute = new TouchField("minute",TNumber.typ)

    /** Gets the month */
    // case "month" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the month */
    //   val field_month = new TouchField("month",TNumber.typ)

    /** Compares dates for disequality */
    // case "not_equals" => 
    //   val List(other) = parameters // DateTime
    //   Top[S](TBoolean.typ)

    /** Gets the second */
    // case "second" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the second */
    //   val field_second = new TouchField("second",TNumber.typ)

    /** Computes the difference between date-times in seconds */
    // case "subtract" => 
    //   val List(value) = parameters // DateTime
    //   Top[S](TNumber.typ)

    /** Converts to the local time */
    // case "to_local_time" => 
    //   Top[S](TDateTime.typ)
    // DECLARATION AS FIELD: 
    //   /** Converts to the local time */
    //   val field_to_local_time = new TouchField("to_local_time",TDateTime.typ)

    /** Converts a dates to a string */
    // case "to_string" => 
    //   Top[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** Converts a dates to a string */
    //   val field_to_string = new TouchField("to_string",TString.typ)

    /** Converts coordinated universal time */
    // case "to_universal_time" => 
    //   Top[S](TDateTime.typ)
    // DECLARATION AS FIELD: 
    //   /** Converts coordinated universal time */
    //   val field_to_universal_time = new TouchField("to_universal_time",TDateTime.typ)

    /** Gets the day of the week (sunday = 0, monday = 1, ... saturday = 6) */
    // case "week_day" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the day of the week (sunday = 0, monday = 1, ... saturday = 6) */
    //   val field_week_day = new TouchField("week_day",TNumber.typ)

    /** Gets the year */
    // case "year" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the year */
    //   val field_year = new TouchField("year",TNumber.typ)

    /** Gets the day of the year between 1 and 366 */
    // case "year_day" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the day of the year between 1 and 366 */
    //   val field_year_day = new TouchField("year_day",TNumber.typ)

    // FIELDS: , field_date, field_day, field_hour, field_millisecond, field_minute, field_month, field_second, field_to_local_time, field_to_string, field_to_universal_time, field_week_day, field_year, field_year_day

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
