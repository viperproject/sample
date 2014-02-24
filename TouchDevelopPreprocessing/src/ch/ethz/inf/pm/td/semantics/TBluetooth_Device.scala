
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Bluetooth Device
 *
 * A device connected via Bluetooth
 *
 * @author Lucas Brutschy
 */ 

object TBluetooth_Device {

  val typName = "Bluetooth Device"
  val typ = DefaultTouchType(typName,isSingleton = false)

}

class TBluetooth_Device extends AAny {

  def getTyp = TBluetooth_Device.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Get the internal address of the device */
    // case "address" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** Get the internal address of the device */
    //   val field_address = new TouchField("address",TString.typ)

    /** Try to connect to the device; use `->connected` to check if it succeeded. */
    // case "connect" => 
    //   val List() = parameters // 
    //   Skip

    /** Check if we're currently connected to device */
    // case "connected" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TBoolean.typ)
    // DECLARATION AS FIELD: 
    //   /** Check if we're currently connected to device */
    //   val field_connected = new TouchField("connected",TBoolean.typ)

    /** Close connection to the device. */
    // case "disconnect" => 
    //   val List() = parameters // 
    //   Skip

    /** Get the user-friendly name of the device */
    // case "name" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** Get the user-friendly name of the device */
    //   val field_name = new TouchField("name",TString.typ)

    /** Read at most `max_length` bytes from the device */
    // case "read buffer at most" => 
    //   val List(max_length) = parameters // Number
    //   TopWithInvalid[S](TBuffer.typ)
    // DECLARATION AS FIELD: 
    //   /** Read at most `max_length` bytes from the device */
    //   val field_read_buffer_at_most = new TouchField("read buffer at most",TBuffer.typ)

    /** Read exactly `length` bytes from the device */
    // case "read buffer" => 
    //   val List(length) = parameters // Number
    //   TopWithInvalid[S](TBuffer.typ)
    // DECLARATION AS FIELD: 
    //   /** Read exactly `length` bytes from the device */
    //   val field_read_buffer = new TouchField("read buffer",TBuffer.typ)

    /** Send the `buffer` to the device */
    // case "write buffer" => 
    //   val List(buffer) = parameters // Buffer
    //   Skip

    // FIELDS: field_address, field_connected, field_name, field_read_buffer_at_most, field_read_buffer

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
