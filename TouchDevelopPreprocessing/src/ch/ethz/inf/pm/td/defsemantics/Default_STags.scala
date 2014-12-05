
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Tags
 *
 * 2D barcodes, QR codes and NFC tags
 *
 * @author Lucas Brutschy
 */

trait Default_STags extends ASingleton {

  lazy val typeName = TypeName("Tags")
          
  /** Never used: Receives a picture through NFC. */
  def member_nfc_receive_picture = ApiMember(
    name = "nfc receive picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Never used: Receives text through NFC. `type` may also be a mime type. */
  def member_nfc_receive = ApiMember(
    name = "nfc receive",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Sends a url, text or any other format using NFC. `type` may be a mime type. */
  def member_nfc_send_picture = ApiMember(
    name = "nfc send picture",
    paramTypes = List(ApiParam(TPicture)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sends a url, text or any other text format using NFC. `type` may be a mime type. */
  def member_nfc_send = ApiMember(
    name = "nfc send",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Writes a static NFC tag with url, text or any other format. `type` may be a mime type. */
  def member_nfc_write_tag = ApiMember(
    name = "nfc write tag",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**dbg**] Scans an id tag created by TouchDevelop and returns the embeded text. */
  def member_scan = ApiMember(
    name = "scan",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Generates a 2D barcode pointing to the text using Microsoft Tag. text must be less than 1000 character long and size must be between 0.75 and 5 inches. */
  def member_tag_text = ApiMember(
    name = "tag text",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber), ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Rarely used: Generates a 2D barcode pointing to the url using Microsoft Tag. url must be less than 1000 character long and size must be between 0.75 and 5 inches. */
  def member_tag_url = ApiMember(
    name = "tag url",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber), ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "nfc receive picture" -> member_nfc_receive_picture,
    "nfc receive" -> member_nfc_receive,
    "nfc send picture" -> member_nfc_send_picture,
    "nfc send" -> member_nfc_send,
    "nfc write tag" -> member_nfc_write_tag,
    "scan" -> member_scan,
    "tag text" -> member_tag_text,
    "tag url" -> member_tag_url
  )
            

}
          
