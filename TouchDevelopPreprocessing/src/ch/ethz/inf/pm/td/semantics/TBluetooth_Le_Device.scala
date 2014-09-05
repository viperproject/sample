
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Bluetooth Le Device
 *
 * A device connected via BluetoothLE
 *
 * @author Lucas Brutschy
 */ 

object TBluetooth_Le_Device extends AAny {

  lazy val typeName = TypeName("Bluetooth Le Device")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** [**dbg**] Check if we're currently connected to device */
    // case "connected" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TBoolean)
    // DECLARATION AS FIELD: 
    //   /** [**dbg**] Check if we're currently connected to device */
    //   lazy val field_connected = new TouchField("connected",TBoolean.typeName)

    /** [**dbg**] Get the device id */
    // case "id" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString)
    // DECLARATION AS FIELD: 
    //   /** [**dbg**] Get the device id */
    //   lazy val field_id = new TouchField("id",TString.typeName)

    /** [**dbg**] Get the user-friendly name of the device */
    // case "name" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString)
    // DECLARATION AS FIELD: 
    //   /** [**dbg**] Get the user-friendly name of the device */
    //   lazy val field_name = new TouchField("name",TString.typeName)

    /** [**dbg**] Read the value for a given service and characteristic. */
    // case "read value" => 
    //   val List(service_Id,characteristic_Id) = parameters // String,String
    //   TopWithInvalid[S](T{"g":"Collection","a":["Buffer"]})
    // DECLARATION AS FIELD: 
    //   /** [**dbg**] Read the value for a given service and characteristic. */
    //   lazy val field_read_value = new TouchField("read value",T{"g":"Collection","a":["Buffer"]}.typeName)

    /** [**dbg**] Gets the list of service UUID installed on the device */
    // case "services" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString_Collection)
    // DECLARATION AS FIELD: 
    //   /** [**dbg**] Gets the list of service UUID installed on the device */
    //   lazy val field_services = new TouchField("services",TString_Collection.typeName)

    /** [**dbg**] Writes the `buffer` to the service characteristic. */
    // case "write value" => 
    //   val List(service_Id,characteristic_Id,with_Response,buffer) = parameters // String,String,Boolean,Buffer
    //   Skip

    // FIELDS: field_connected, field_id, field_name, field_read_value, field_services

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
