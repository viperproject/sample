
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Bluetooth Device
 *
 * A device connected via Bluetooth
 *
 * @author Lucas Brutschy
 */

object TBluetooth_Device {

  /** Get the internal address of the device */
  val field_address = new TouchField("address", TString.typName)

  /** Check if we're currently connected to device */
  val field_connected = new TouchField("connected", TBoolean.typName)

  /** Get the user-friendly name of the device */
  val field_name = new TouchField("name", TString.typName)

  val typName = "Bluetooth Device"
  val typ = DefaultTouchType(typName, isSingleton = false, fields = List(field_address, field_connected, field_name))

}

class TBluetooth_Device extends AAny {

  def getTyp = TBluetooth_Device.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Try to connect to the device; use `->connected` to check if it succeeded. */
    case "connect" =>
      AssignField[S](this0, TBluetooth_Device.field_connected, Valid(TBoolean.typ)) // reset connect

    /** Close connection to the device. */
    case "disconnect" =>
      AssignField[S](this0, TBluetooth_Device.field_connected, False) // reset connect

    /** Read at most `max_length` bytes from the device */
    case "read buffer at most" =>
      val List(max_length) = parameters // Number
      Top[S](TBuffer.typ, initialCollectionSize = Some(0 ndTo max_length))

    /** Read exactly `length` bytes from the device */
    case "read buffer" =>
      val List(length) = parameters // Number
      Top[S](TBuffer.typ, initialCollectionSize = Some(0 ndTo length))

    /** Send the `buffer` to the device */
    case "write buffer" =>
      val List(buffer) = parameters // Buffer
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
