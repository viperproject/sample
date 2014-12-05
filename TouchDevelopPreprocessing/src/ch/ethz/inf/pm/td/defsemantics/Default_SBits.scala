
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Bits
 *
 * Arithmetic and bitwise operations on 32 bit integers
 *
 * @author Lucas Brutschy
 */

trait Default_SBits extends ASingleton {

  lazy val typeName = TypeName("Bits")
          
  /** Never used: Add two signed 32 bit numbers */
  def member_add_int32 = ApiMember(
    name = "add int32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Add two unsigned 32 bit numbers */
  def member_add_uint32 = ApiMember(
    name = "add uint32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Perform bitwise and (`&` in C) */
  def member_and_uint32 = ApiMember(
    name = "and uint32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an empty binary buffer of `size` bytes */
  def member_create_buffer = ApiMember(
    name = "create buffer",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBuffer,
    semantics = DefaultSemantics
  )

  /** Never used: Multiply two signed 32 bit numbers */
  def member_multiply_int32 = ApiMember(
    name = "multiply int32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Multiply two unsigned 32 bit numbers */
  def member_multiply_uint32 = ApiMember(
    name = "multiply uint32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Perform bitwise negation (`~` in C) */
  def member_not_uint32 = ApiMember(
    name = "not uint32",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Perform bitwise or (`|` in C) */
  def member_or_uint32 = ApiMember(
    name = "or uint32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Rotate `x` by `bits` left (rotl) */
  def member_rotate_left_uint32 = ApiMember(
    name = "rotate left uint32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Rotate `x` by `bits` right (rotr) */
  def member_rotate_right_uint32 = ApiMember(
    name = "rotate right uint32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Shift `x` by `bits` left (`<<` in C) */
  def member_shift_left_uint32 = ApiMember(
    name = "shift left uint32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Shift `x` by `bits` right (`>>` in C, `>>>` in JavaScript) */
  def member_shift_right_uint32 = ApiMember(
    name = "shift right uint32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Decodes string into a binary buffer */
  def member_string_to_buffer = ApiMember(
    name = "string to buffer",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBuffer,
    semantics = DefaultSemantics
  )

  /** Never used: Subtract two signed 32 bit numbers */
  def member_subtract_int32 = ApiMember(
    name = "subtract int32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Subtract two unsigned 32 bit numbers */
  def member_subtract_uint32 = ApiMember(
    name = "subtract uint32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Perform bitwise exclusive or (`^` in C) */
  def member_xor_uint32 = ApiMember(
    name = "xor uint32",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add int32" -> member_add_int32,
    "add uint32" -> member_add_uint32,
    "and uint32" -> member_and_uint32,
    "create buffer" -> member_create_buffer,
    "multiply int32" -> member_multiply_int32,
    "multiply uint32" -> member_multiply_uint32,
    "not uint32" -> member_not_uint32,
    "or uint32" -> member_or_uint32,
    "rotate left uint32" -> member_rotate_left_uint32,
    "rotate right uint32" -> member_rotate_right_uint32,
    "shift left uint32" -> member_shift_left_uint32,
    "shift right uint32" -> member_shift_right_uint32,
    "string to buffer" -> member_string_to_buffer,
    "subtract int32" -> member_subtract_int32,
    "subtract uint32" -> member_subtract_uint32,
    "xor uint32" -> member_xor_uint32
  )
            

}
          
