
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
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
  val field_device = new TouchField("device",TDevice.typName)

  /** Indicates additional information about why the Printer is in its current state. */
  val field_state_reason = new TouchField("state reason",TString.typName)

  /** Gets the name of the printer */
  val field_name = new TouchField("name",TString.typName)

  /** Indicates if no jobs can be processed and intervention is needed. */
  val field_is_stopped = new TouchField("is stopped",TBoolean.typName)

  /** Indicates if jobs are processing; new jobs will wait before processing, i.e., are said to be pending. */
  val field_is_processing = new TouchField("is processing",TBoolean.typName)

  /** Indicates if new jobs can start processing immediately without waiting. */
  val field_is_idle = new TouchField("is idle",TBoolean.typName)

  val typName = "Printer"
  val typ = DefaultTouchType(typName,isSingleton = false, fields = List(field_device, field_is_idle, field_is_processing,
    field_is_stopped, field_name, field_state_reason))

}

class TPrinter extends AAny {

  def getTyp = TPrinter.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Queues a job to print the text. */
    case "print text" =>
      val List(text) = parameters // String
      Error[S](Field[S](this0,TPrinter.field_is_stopped),"print text", "Check if printer is stopped before printing")
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
      
