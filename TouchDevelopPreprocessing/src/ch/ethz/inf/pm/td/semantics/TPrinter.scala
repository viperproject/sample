
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Printer
 *
 * A printer on the home network
 *
 * @author Lucas Brutschy
 */ 

object TPrinter {
  /** Gets the detailed information about this device */
  val field_device = new TouchField("device",TDevice.typ)

  /** Indicates additional information about why the Printer is in its current state. */
  val field_state_reason = new TouchField("state_reason",TString.typ)

  /** Gets the name of the printer */
  val field_name = new TouchField("name",TString.typ)

  /** Indicates if no jobs can be processed and intervention is needed. */
  val field_is_stopped = new TouchField("is_stopped",TBoolean.typ)

  /** Indicates if jobs are processing; new jobs will wait before processing, i.e., are said to be pending. */
  val field_is_processing = new TouchField("is_processing",TBoolean.typ)

  /** Indicates if new jobs can start processing immediately without waiting. */
  val field_is_idle = new TouchField("is_idle",TBoolean.typ)

  val typName = "Printer"
  val typ = TouchType(typName,isSingleton = false,List(field_device, field_is_idle, field_is_processing,
    field_is_stopped, field_name, field_state_reason))

}

class TPrinter extends AAny {

  def getTyp = TPrinter.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Queues a job to print the text. */
    case "print_text" =>
      val List(text) = parameters // String
      Error[S](Field[S](this0,TPrinter.field_is_stopped),"Check if printer is stopped before printing")
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}
      
