
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Web Socket
 *
 * A web socket
 *
 * @author Lucas Brutschy
 */

trait Default_TWeb_Socket extends AAny {

  lazy val typeName = TypeName("Web Socket")
          
  /** Never used: The number of bytes of data that have been queued using calls to send() but not yet transmitted to the network. This value does not reset to zero when the connection is closed; if you keep calling send(), this will continue to climb. */
  def member_buffered_amount = ApiMember(
    name = "buffered amount",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Closes the socket */
  def member_close = ApiMember(
    name = "close",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the ready state of the web socket, "connection", "closed", "closing", "open" */
  def member_ready_state = ApiMember(
    name = "ready state",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Receives a message */
  def member_receive = ApiMember(
    name = "receive",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TWeb_Socket_Message,
    semantics = DefaultSemantics
  )

  /** Never used: Sends buffer data to the server */
  def member_send_buffer = ApiMember(
    name = "send buffer",
    paramTypes = List(ApiParam(TBuffer)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Transmits JSON data to the server */
  def member_send_json = ApiMember(
    name = "send json",
    paramTypes = List(ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Transmits string data to the server */
  def member_send = ApiMember(
    name = "send",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "buffered amount" -> member_buffered_amount,
    "close" -> member_close,
    "ready state" -> member_ready_state,
    "receive" -> member_receive,
    "send buffer" -> member_send_buffer,
    "send json" -> member_send_json,
    "send" -> member_send
  )
            

}
          
