
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Converter
 *
 * A generic atomic conversion function
 *
 * @author Lucas Brutschy
 */ 

object TConverter {

  val typName = "Converter"
  val typ = DefaultTouchType(typName,isSingleton = false)

}

class TConverter extends AAny {

  def getTyp = TConverter.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Run the inline action. */
    // case "run" => 
    //   val List(elt) = parameters // From
    //   TopWithInvalid[S](TTo.typ)
    // DECLARATION AS FIELD: 
    //   /** Run the inline action. */
    //   val field_run = new TouchField("run",TTo.typName)

    // FIELDS: field_run

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
