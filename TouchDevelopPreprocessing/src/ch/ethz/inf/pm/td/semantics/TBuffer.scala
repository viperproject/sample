
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Buffer
 *
 * Contains binary data
 *
 * @author Lucas Brutschy
 */ 

object TBuffer {

  val typName = "Buffer"
  val typ = new TouchCollection(typName,TNumber.typName,TNumber.typName)

}

class TBuffer extends ALinearCollection {

  def getTyp = TBuffer.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Copies all bytes from `source` to current buffer at `offset` */
    case "clone" =>
      // TODO CHECK RANGE
      Assign[S](CollectionSummary[S](this0),Top[S](TNumber.typ).getExpression)

    /** Copies all bytes from `source` to current buffer at `offset` */
    case "copy from" =>
       val List(target_offset,source) = parameters // Number,Buffer
      // TODO CHECK RANGE
      Assign[S](CollectionSummary[S](this0),Top[S](TNumber.typ).getExpression)

    /** Fills the buffer with random values */
    case "fill random" =>
       val List() = parameters //
      Assign[S](CollectionSummary[S](this0),Top[S](TNumber.typ).getExpression)

    /** Sets all bytes in buffer to `value` */
    case "fill" =>
       val List(value) = parameters // Number
       Assign[S](CollectionSummary[S](this0),value)

    /** Set byte at `index` to `value` */
    case "set" =>
      val List(index,value) = parameters // Number,Number
      If[S](CollectionIndexInRange[S](this0, index), Then=(state) => {
        val newState = CollectionRemove[S](this0, index)(state, pp)
        CollectionInsert[S](this0, index, value)(newState, pp)
      }, Else=(state) => {
        state
      })

    /** Creates a read-write view of the current buffer. */
    case "sub buffer" =>
       val List(start,length) = parameters // Number,Number
       // TODO CHECK RANGE
       TopWithInvalid[S](TBuffer.typ)

    /** Convert the buffer to a string */
    case "to string" =>
       val List(encoding) = parameters // String
       Top[S](TString.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
