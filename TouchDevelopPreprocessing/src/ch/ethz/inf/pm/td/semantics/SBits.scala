
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Bits
 *
 * Arithmetic and bitwise operations on 32 bit integers
 *
 * @author Lucas Brutschy
 */

object SBits extends ASingleton {

  lazy val typeName = TypeName("Bits")

  /** Never used: Add two signed 32 bit numbers */
  lazy val member_add_int32 = new ApiMember("add int32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Add two unsigned 32 bit numbers */
  lazy val member_add_uint32 = new ApiMember("add uint32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Perform bitwise and (`&` in C) */
  lazy val member_and_uint32 = new ApiMember("and uint32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Creates an empty binary buffer of `size` bytes */
  lazy val member_create_buffer = new ApiMember("create buffer", List(ApiParam(TNumber)), ApiParam(this), TBuffer) with ValidPureSemantics

  /** Never used: Multiply two signed 32 bit numbers */
  lazy val member_multiply_int32 = new ApiMember("multiply int32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Multiply two unsigned 32 bit numbers */
  lazy val member_multiply_uint32 = new ApiMember("multiply uint32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Perform bitwise negation (`~` in C) */
  lazy val member_not_uint32 = new ApiMember("not uint32", List(ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Perform bitwise or (`|` in C) */
  lazy val member_or_uint32 = new ApiMember("or uint32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Rotate `x` by `bits` left (rotl) */
  lazy val member_rotate_left_uint32 = new ApiMember("rotate left uint32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Rotate `x` by `bits` right (rotr) */
  lazy val member_rotate_right_uint32 = new ApiMember("rotate right uint32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Shift `x` by `bits` left (`<<` in C) */
  lazy val member_shift_left_uint32 = new ApiMember("shift left uint32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Shift `x` by `bits` right (`>>` in C, `>>>` in JavaScript) */
  lazy val member_shift_right_uint32 = new ApiMember("shift right uint32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Decodes string into a binary buffer */
  lazy val member_string_to_buffer = new ApiMember("string to buffer", List(ApiParam(TString), ApiParam(TString)), ApiParam(this), TBuffer) with ValidPureSemantics

  /** Never used: Subtract two signed 32 bit numbers */
  lazy val member_subtract_int32 = new ApiMember("subtract int32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Subtract two unsigned 32 bit numbers */
  lazy val member_subtract_uint32 = new ApiMember("subtract uint32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics

  /** Never used: Perform bitwise exclusive or (`^` in C) */
  lazy val member_xor_uint32 = new ApiMember("xor uint32", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(this), TNumber) with ValidPureSemantics


  override lazy val declarations:Map[String,ApiMember] = super.declarations ++ Map(
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

