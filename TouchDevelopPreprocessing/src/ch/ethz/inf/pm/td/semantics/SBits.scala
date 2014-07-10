
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._

/**
 * Specifies the abstract semantics of Bits
 *
 * Arithmetic and bitwise operations on 32 bit integers
 *
 * @author Lucas Brutschy
 */

object SBits {

  val typName = "Bits"
  val typ = DefaultTouchType(typName, isSingleton = true)

}

class SBits extends AAny {

  def getTyp = SBits.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Add two signed 32 bit numbers */
    case "add int32" =>
      val List(x, y) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Add two unsigned 32 bit numbers */
    case "add uint32" =>
      val List(x, y) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Perform bitwise and (`&` in C) */
    case "and uint32" =>
      val List(x, y) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Creates an empty binary buffer of `size` bytes */
    case "create buffer" =>
      val List(size) = parameters // Number
      New[S](TBuffer.typ, initialCollectionSize = Some(size))

    /** Multiply two signed 32 bit numbers */
    case "multiply int32" =>
      val List(x, y) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Multiply two unsigned 32 bit numbers */
    case "multiply uint32" =>
      val List(x, y) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Perform bitwise negation (`~` in C) */
    case "not uint32" =>
      val List(x) = parameters // Number
      Top[S](TNumber.typ)

    /** Perform bitwise or (`|` in C) */
    case "or uint32" =>
      val List(x, y) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Rotate `x` by `bits` left (rotl) */
    case "rotate left uint32" =>
      val List(x, bits) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Rotate `x` by `bits` right (rotr) */
    case "rotate right uint32" =>
      val List(x, bits) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Shift `x` by `bits` left (`<<` in C) */
    case "shift left uint32" =>
      val List(x, bits) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Shift `x` by `bits` right (`>>` in C, `>>>` in JavaScript) */
    case "shift right uint32" =>
      val List(x, bits) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Decodes string into a binary buffer */
    case "string to buffer" =>
      val List(s, encoding) = parameters // String,String
      Top[S](TBuffer.typ)

    /** Subtract two signed 32 bit numbers */
    case "subtract int32" =>
      val List(x, y) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Subtract two unsigned 32 bit numbers */
    case "subtract uint32" =>
      val List(x, y) = parameters // Number,Number
      Top[S](TNumber.typ)

    /** Perform bitwise exclusive or (`^` in C) */
    case "xor uint32" =>
      val List(x, y) = parameters // Number,Number
      Top[S](TNumber.typ)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
