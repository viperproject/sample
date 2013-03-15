
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.Reporter

/**
 * Specifies the abstract semantics of DateTime
 *
 * A combination of date and time
 *
 * @author Lucas Brutschy
 */ 

object TDateTime {

  /** Gets the day of the month */
  val field_day = new TouchField("day",TNumber.typ)

  /** Gets the hour */
  val field_hour = new TouchField("hour",TNumber.typ)

  /** Gets the millisecond */
  val field_millisecond = new TouchField("millisecond",TNumber.typ)

  /** Gets the minute */
  val field_minute = new TouchField("minute",TNumber.typ)

  /** Gets the month */
  val field_month = new TouchField("month",TNumber.typ)

  /** Gets the second */
  val field_second = new TouchField("second",TNumber.typ)

  /** Gets the year */
  val field_year = new TouchField("year",TNumber.typ)

  val typName = "DateTime"
  val typ = new TouchType(typName,isSingleton = false,List(field_day, field_hour, field_millisecond, field_minute, field_month, field_second, field_year))

}

class TDateTime extends AAny {

  def getTyp = TDateTime.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Returns a date that adds the specified number of days to the value of this instance. */
    case "add_days" =>
      val List(days) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_day -> (Field[S](this0,TDateTime.field_day) + days)
      ))

    /** Returns a date that adds the specified number of hours to the value of this instance. */
    case "add_hours" =>
      val List(hours) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_hour -> (Field[S](this0,TDateTime.field_hour) + hours)
      ))

    /** Returns a date that adds the specified number of milliseconds to the value of this instance. */
    case "add_milliseconds" =>
      val List(milliseconds) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_millisecond -> (Field[S](this0,TDateTime.field_millisecond) + milliseconds)
      ))

    /** Returns a date that adds the specified number of minutes to the value of this instance. */
    case "add_minutes" =>
      val List(minutes) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_minute -> (Field[S](this0,TDateTime.field_minute) + minutes)
      ))

    /** Returns a date that adds the specified number of months to the value of this instance. */
    case "add_months" =>
      val List(months) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_month -> (Field[S](this0,TDateTime.field_month) + months)
      ))

    /** Returns a date that adds the specified number of seconds to the value of this instance. */
    case "add_seconds" =>
      val List(seconds) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_second -> (Field[S](this0,TDateTime.field_second) + seconds)
      ))

    /** Returns a date that adds the specified number of years to the value of this instance. */
    case "add_years" =>
      val List(years) = parameters // Number
      Clone[S](this0,Map(
        TDateTime.field_year -> (Field[S](this0,TDateTime.field_year) + years)
      ))

    /** Gets the date */
    case "date" =>
      New[S](TDateTime.typ,Map(
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

    /** Compares dates for greater */
    case "greater" =>
      val List(other) = parameters // DateTime
      Reporter.reportImprecision("DateTime.greater is a dummy",pp)
      Top[S](TBoolean.typ)

    /** Compares dates for greater or equal */
    case "greater_or_equal" =>
      val List(other) = parameters // DateTime
      Reporter.reportImprecision("DateTime.greater_or_equal is a dummy",pp)
      Top[S](TBoolean.typ)

    /** Compares dates for less */
    case "less" =>
      val List(other) = parameters // DateTime
      Reporter.reportImprecision("DateTime.less is a dummy",pp)
      Top[S](TBoolean.typ)

    /** Compares dates for less or equal */
    case "less_or_equals" =>
      val List(other) = parameters // DateTime
      Reporter.reportImprecision("DateTime.less_or_equals is a dummy",pp)
      Top[S](TBoolean.typ)

    /** Compares dates for disequality */
    case "not_equals" =>
      val List(other) = parameters // DateTime
      Reporter.reportImprecision("DateTime.not_equals is a dummy",pp)
       Top[S](TBoolean.typ)

    /** Computes the difference between date-times in seconds */
    case "subtract" =>
      val List(value) = parameters // DateTime
      Reporter.reportImprecision("DateTime.subtract is a dummy",pp)
      Top[S](TNumber.typ)

    /** Converts to the local time */
    // case "to_local_time" => 
    //   Top[S](TDateTime.typ)
    // DECLARATION AS FIELD: 
    //   /** Converts to the local time */
    //   val field_to_local_time = new TouchField("to_local_time",TDateTime.typ)

    /** Converts coordinated universal time */
    // case "to_universal_time" => 
    //   Top[S](TDateTime.typ)
    // DECLARATION AS FIELD: 
    //   /** Converts coordinated universal time */
    //   val field_to_universal_time = new TouchField("to_universal_time",TDateTime.typ)

    /** Gets the day of the week (sunday = 0, monday = 1, ... saturday = 6) */
    case "week_day" =>
      Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the day of the week (sunday = 0, monday = 1, ... saturday = 6) */
    //   val field_week_day = new TouchField("week_day",TNumber.typ)

    /** Gets the day of the year between 1 and 366 */
    case "year_day" =>
      Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Gets the day of the year between 1 and 366 */
    //   val field_year_day = new TouchField("year_day",TNumber.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
