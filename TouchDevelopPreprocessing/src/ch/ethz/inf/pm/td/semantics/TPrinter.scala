
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Printer
 *
 * A printer on the home network
 *
 * @author Lucas Brutschy
 */ 

object TPrinter extends AAny {

  /** Gets the detailed information about this device */
  lazy val field_device = new ApiField("device",TDevice.typeName)

  /** Indicates additional information about why the Printer is in its current state. */
  lazy val field_state_reason = new ApiField("state reason",TString.typeName)

  /** Gets the name of the printer */
  lazy val field_name = new ApiField("name",TString.typeName)

  /** Indicates if no jobs can be processed and intervention is needed. */
  lazy val field_is_stopped = new ApiField("is stopped",TBoolean.typeName)

  /** Indicates if jobs are processing; new jobs will wait before processing, i.e., are said to be pending. */
  lazy val field_is_processing = new ApiField("is processing",TBoolean.typeName)

  /** Indicates if new jobs can start processing immediately without waiting. */
  lazy val field_is_idle = new ApiField("is idle",TBoolean.typeName)

  lazy val typeName = TypeName("Printer")

  override def possibleFields = super.possibleFields ++ List(field_device, field_is_idle, field_is_processing,
    field_is_stopped, field_name, field_state_reason)

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
      
