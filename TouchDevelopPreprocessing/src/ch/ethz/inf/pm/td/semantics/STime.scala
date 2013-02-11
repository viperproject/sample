package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of time
 *
 * time and dates
 *
 * @author Lucas Brutschy
 */

object STime {

  val typName = "time"
  val typ = new TouchType(typName,isSingleton = true,List())

}

class STime extends AAny {

  def getTyp = STime.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a new date instance */
    case "create" =>
      val List(year,month,day,hour,minute,second) = parameters // Number,Number,Number,Number,Number,Number
      New[S](TDateTime.typ,year,month,day,hour,minute,second)

    /** Aborts the execution if the condition is false. */
    case "fail_if_not" =>
      val List(condition) = parameters // Boolean
      Error[S](condition.not(),"fail if not "+condition+" might fail")
      Skip

    /** Appends this message to the debug log. Does nothing when the script is published. */
    case "log" =>
      val List(message) = parameters // String
      Skip

    /** Gets the current time */
    case "now" =>
       Return[S](Valid(TDateTime.typ))

    /** Waits for a specified amount of seconds */
    case "sleep" =>
      val List(seconds) = parameters // Number
      Skip

    /** Stops the execution and stays on the wall. */
    // case "stop" => 
    //   Skip;

    /** Stops the execution and leaves the wall. */
     case "stop_and_close" =>
       Skip // TODO: Treat this explicitly in control flow - may cause false alarms

    /** Gets today's date without time */
    case "today" =>
      Return[S](Valid(TDateTime.typ))

    /** Gets tomorrow's date without time */
    case "tomorrow" =>
       Return[S](Valid(TDateTime.typ))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}