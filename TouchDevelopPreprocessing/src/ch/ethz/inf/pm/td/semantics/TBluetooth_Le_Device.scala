
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Bluetooth Le Device
 *
 * A device connected via BluetoothLE
 *
 * @author Lucas Brutschy
 */ 

object TBluetooth_Le_Device {

  val typName = "Bluetooth Le Device"
  val typ = DefaultTouchType(typName,isSingleton = true)

}

class TBluetooth_Le_Device extends AAny {

  def getTyp = TBluetooth_Le_Device.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** [**dbg**] Check if we're currently connected to device */
    // case "connected" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TBoolean.typ)
    // DECLARATION AS FIELD: 
    //   /** [**dbg**] Check if we're currently connected to device */
    //   val field_connected = new TouchField("connected",TBoolean.typName)

    /** [**dbg**] Get the device id */
    // case "id" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** [**dbg**] Get the device id */
    //   val field_id = new TouchField("id",TString.typName)

    /** [**dbg**] Get the user-friendly name of the device */
    // case "name" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** [**dbg**] Get the user-friendly name of the device */
    //   val field_name = new TouchField("name",TString.typName)

    /** [**dbg**] Read the value for a given service and characteristic. */
    // case "read value" => 
    //   val List(service_Id,characteristic_Id) = parameters // String,String
    //   TopWithInvalid[S](T{"g":"Collection","a":["Buffer"]}.typ)
    // DECLARATION AS FIELD: 
    //   /** [**dbg**] Read the value for a given service and characteristic. */
    //   val field_read_value = new TouchField("read value",T{"g":"Collection","a":["Buffer"]}.typName)

    /** [**dbg**] Gets the list of service UUID installed on the device */
    // case "services" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString_Collection.typ)
    // DECLARATION AS FIELD: 
    //   /** [**dbg**] Gets the list of service UUID installed on the device */
    //   val field_services = new TouchField("services",TString_Collection.typName)

    /** [**dbg**] Writes the `buffer` to the service characteristic. */
    // case "write value" => 
    //   val List(service_Id,characteristic_Id,with_Response,buffer) = parameters // String,String,Boolean,Buffer
    //   Skip

    // FIELDS: field_connected, field_id, field_name, field_read_value, field_services

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
