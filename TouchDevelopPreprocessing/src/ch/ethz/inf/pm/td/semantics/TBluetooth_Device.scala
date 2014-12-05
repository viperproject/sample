
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TBluetooth_Device
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Bluetooth Device
 *
 * A device connected via Bluetooth
 *
 * @author Lucas Brutschy
 */

object TBluetooth_Device extends Default_TBluetooth_Device {

  /** Get the internal address of the device */
  lazy val field_address = ApiField("address", TString)

  /** Check if we're currently connected to device */
  lazy val field_connected = ApiField("connected", TBoolean)

  /** Get the user-friendly name of the device */
  lazy val field_name = ApiField("name", TString)

  override def possibleFields = super.possibleFields ++ List(field_address, field_connected, field_name)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Try to connect to the device; use `->connected` to check if it succeeded. */
    case "connect" =>
      AssignField[S](this0, TBluetooth_Device.field_connected, Valid(TBoolean)) // reset connect

    /** Close connection to the device. */
    case "disconnect" =>
      AssignField[S](this0, TBluetooth_Device.field_connected, False) // reset connect

    /** Read at most `max_length` bytes from the device */
    case "read buffer at most" =>
      val List(max_length) = parameters // Number
      Top[S](TBuffer, initialCollectionSize = Some(0 ndTo max_length))

    /** Read exactly `length` bytes from the device */
    case "read buffer" =>
      val List(length) = parameters // Number
      Top[S](TBuffer, initialCollectionSize = Some(0 ndTo length))

    /** Send the `buffer` to the device */
    case "write buffer" =>
      val List(buffer) = parameters // Buffer
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
