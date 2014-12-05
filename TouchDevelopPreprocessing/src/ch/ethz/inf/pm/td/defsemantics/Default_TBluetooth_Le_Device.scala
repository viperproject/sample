
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Bluetooth Le Device
 *
 * A device connected via BluetoothLE
 *
 * @author Lucas Brutschy
 */

trait Default_TBluetooth_Le_Device extends AAny {

  lazy val typeName = TypeName("Bluetooth Le Device")
          
  /** Never used: [**dbg**] Check if we're currently connected to device */
  def member_connected = ApiMember(
    name = "connected",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Get the device id */
  def member_id = ApiMember(
    name = "id",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Get the user-friendly name of the device */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Read the value for a given service and characteristic. */
  def member_read_value = ApiMember(
    name = "read value",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = GCollection(TBuffer),
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Gets the list of service UUID installed on the device */
  def member_services = ApiMember(
    name = "services",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString_Collection,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Writes the `buffer` to the service characteristic. */
  def member_write_value = ApiMember(
    name = "write value",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TBoolean), ApiParam(TBuffer)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "connected" -> member_connected,
    "id" -> member_id,
    "name" -> member_name,
    "read value" -> member_read_value,
    "services" -> member_services,
    "write value" -> member_write_value
  )
            

}
          
