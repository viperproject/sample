/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Bluetooth Device
 *
 * A device connected via Bluetooth
 *
 * @author Lucas Brutschy
 */

trait Default_TBluetooth_Device extends AAny {

  lazy val typeName = TypeName("Bluetooth Device")
          
  /** Never used: Get the internal address of the device */
  def member_address = ApiMember(
    name = "address",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Try to connect to the device; use `->connected` to check if it succeeded. */
  def member_connect = ApiMember(
    name = "connect",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Check if we're currently connected to device */
  def member_connected = ApiMember(
    name = "connected",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Close connection to the device. */
  def member_disconnect = ApiMember(
    name = "disconnect",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Get the user-friendly name of the device */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Read at most `max_length` bytes from the device */
  def member_read_buffer_at_most = ApiMember(
    name = "read buffer at most",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBuffer,
    semantics = DefaultSemantics
  )

  /** Never used: Read exactly `length` bytes from the device */
  def member_read_buffer = ApiMember(
    name = "read buffer",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBuffer,
    semantics = DefaultSemantics
  )

  /** Never used: Send the `buffer` to the device */
  def member_write_buffer = ApiMember(
    name = "write buffer",
    paramTypes = List(ApiParam(TBuffer)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "address" -> member_address,
    "connect" -> member_connect,
    "connected" -> member_connected,
    "disconnect" -> member_disconnect,
    "name" -> member_name,
    "read buffer at most" -> member_read_buffer_at_most,
    "read buffer" -> member_read_buffer,
    "write buffer" -> member_write_buffer
  )
            

}
          
