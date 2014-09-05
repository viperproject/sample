
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of DateTime
 *
 * A combination of date and time
 *
 * @author Lucas Brutschy
 */ 

object TDateTime extends AAny {

  /** Gets the day of the month */
  lazy val field_day = new TouchField("day",TNumber.typeName)

  /** Gets the hour */
  lazy val field_hour = new TouchField("hour",TNumber.typeName)

  /** Gets the millisecond */
  lazy val field_millisecond = new TouchField("millisecond",TNumber.typeName)

  /** Gets the minute */
  lazy val field_minute = new TouchField("minute",TNumber.typeName)

  /** Gets the month */
  lazy val field_month = new TouchField("month",TNumber.typeName)

  /** Gets the second */
  lazy val field_second = new TouchField("second",TNumber.typeName)

  /** Gets the year */
  lazy val field_year = new TouchField("year",TNumber.typeName)

  lazy val typeName = TypeName("DateTime")

  override def possibleFields = super.possibleFields ++ List(field_day, field_hour, field_millisecond, field_minute, field_month, field_second, field_year)

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

    /** Compares dates for greater */
    case "greater" =>
      val List(other) = parameters // DateTime
      Dummy[S](this0,method)
      Top[S](TBoolean)

    /** Compares dates for greater or equal */
    case "greater or equal" =>
      val List(other) = parameters // DateTime
      Dummy[S](this0,method)
      Top[S](TBoolean)

    /** Compares dates for less */
    case "less" =>
      val List(other) = parameters // DateTime
      Dummy[S](this0,method)
      Top[S](TBoolean)

    /** Compares dates for less or equal */
    case "less or equals" =>
      val List(other) = parameters // DateTime
      Dummy[S](this0,method)
      Top[S](TBoolean)

    /** Compares dates for disequality */
    case "not equals" =>
      val List(other) = parameters // DateTime
      Dummy[S](this0,method)
       Top[S](TBoolean)

    /** Computes the difference between date-times in seconds */
    case "subtract" =>
      val List(value) = parameters // DateTime
      Top[S](TNumber)

    /** Converts to the local time */
    case "to local time" =>
      Top[S](TDateTime)

    /** Converts coordinated universal time */
    case "to universal time" =>
      Top[S](TDateTime)

    /** Gets the day of the week (sunday = 0, monday = 1, ... saturday = 6) */
    case "week day" =>
      Top[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Gets the day of the week (sunday = 0, monday = 1, ... saturday = 6) */
    //   lazy val field_week_day = new TouchField("week day",TNumber.typeName)

    /** Gets the day of the year between 1 and 366 */
    case "year day" =>
      Top[S](TNumber)
    // DECLARATION AS FIELD: 
    //   /** Gets the day of the year between 1 and 366 */
    //   lazy val field_year_day = new TouchField("year day",TNumber.typeName)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
