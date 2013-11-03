
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Buffer
 *
 * Contains binary data
 *
 * @author Lucas Brutschy
 */ 

object TBuffer {

  val typName = "Buffer"
  val typ = new TouchType(typName,isSingleton = true)

}

class TBuffer extends AAny {

  def getTyp = TBuffer.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Get byte at `index` */
    // case "at" =>
    //   val List(index) = parameters // Number
    //   TopWithInvalid[S](TNumber.typ)
    // DECLARATION AS FIELD:
    //   /** Get byte at `index` */
    //   val field_at = new TouchField("at",TNumber.typ)

    /** Copies all bytes from `source` to current buffer at `offset` */
    // case "clone" =>
    //   val List() = parameters //
    //   Skip

    /** Copies all bytes from `source` to current buffer at `offset` */
    // case "copy from" =>
    //   val List(target_offset,source) = parameters // Number,Buffer
    //   Skip

    /** Return the number of bytes in the buffer */
    // case "count" =>
    //   val List() = parameters //
    //   TopWithInvalid[S](TNumber.typ)
    // DECLARATION AS FIELD:
    //   /** Return the number of bytes in the buffer */
    //   val field_count = new TouchField("count",TNumber.typ)

    /** Fills the buffer with random values */
    // case "fill random" =>
    //   val List() = parameters //
    //   Skip

    /** Sets all bytes in buffer to `value` */
    // case "fill" => 
    //   val List(value) = parameters // Number
    //   Skip

    /** Set byte at `index` to `value` */
    // case "set" => 
    //   val List(index,value) = parameters // Number,Number
    //   Skip

    /** Creates a read-write view of the current buffer. */
    // case "sub buffer" =>
    //   val List(start,length) = parameters // Number,Number
    //   TopWithInvalid[S](TBuffer.typ)
    // DECLARATION AS FIELD:
    //   /** Creates a read-write view of the current buffer. */
    //   val field_sub_buffer = new TouchField("sub buffer",TBuffer.typ)

    /** Convert the buffer to a string */
    // case "to string" =>
    //   val List(encoding) = parameters // String
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD:
    //   /** Convert the buffer to a string */
    //   val field_to_string = new TouchField("to string",TString.typ)

    // FIELDS: field_at, field_count, field_sub_buffer, field_to_string

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
