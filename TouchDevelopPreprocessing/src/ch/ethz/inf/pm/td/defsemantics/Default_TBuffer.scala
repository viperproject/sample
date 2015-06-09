
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Buffer
 *
 * Contains binary data
 *
 * @author Lucas Brutschy
 */

trait Default_TBuffer extends ALinearCollection {

  lazy val typeName = TypeName("Buffer")
          
  def keyType = TNumber

  def valueType = TNumber

  /** Never used: Copies all bytes from `source` to current buffer at `offset` */
  def member_clone = ApiMember(
    name = "clone",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBuffer,
    semantics = DefaultSemantics
  )

  /** Never used: Return a new buffer consiting of the current and `other` in sequence */
  def member_concat = ApiMember(
    name = "concat",
    paramTypes = List(ApiParam(TBuffer)),
    thisType = ApiParam(this),
    returnType = TBuffer,
    semantics = DefaultSemantics
  )

  /** Never used: Copies all bytes from `source` to current buffer at `offset` */
  def member_copy_from = ApiMember(
    name = "copy from",
    paramTypes = List(ApiParam(TNumber), ApiParam(TBuffer)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Fills the buffer with random values */
  def member_fill_random = ApiMember(
    name = "fill random",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets all bytes in buffer to `value` */
  def member_fill = ApiMember(
    name = "fill",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Read a binary number at `offset` */
  def member_read_number = ApiMember(
    name = "read number",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Set byte at `index` to `value` */
  def member_set = ApiMember(
    name = "set",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Return the SHA-256 hash of the buffer encoded as lowercase hex */
  def member_sha256 = ApiMember(
    name = "sha256",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a read-write view of the current buffer. */
  def member_sub_buffer = ApiMember(
    name = "sub buffer",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TBuffer,
    semantics = DefaultSemantics
  )

  /** Never used: Convert the buffer to a string */
  def member_to_string = ApiMember(
    name = "to string",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Write a binary number `value` at `offset` */
  def member_write_number = ApiMember(
    name = "write number",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "clone" -> member_clone,
    "concat" -> member_concat,
    "copy from" -> member_copy_from,
    "fill random" -> member_fill_random,
    "fill" -> member_fill,
    "read number" -> member_read_number,
    "set" -> member_set,
    "sha256" -> member_sha256,
    "sub buffer" -> member_sub_buffer,
    "to string" -> member_to_string,
    "write number" -> member_write_number
  )
            

}
          
