
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Unknown
 *
 * an unknown value
 *
 * @author Lucas Brutschy
 */ 

object TUnknown {

  val typName = "Unknown"
  val typ = new TouchType(typName,isSingleton = true)

}

class TUnknown extends AAny {

  def getTyp = TUnknown.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /**  */
    case ":=" =>
      val List(right) = parameters // Unknown,Unknown
      Assign[S](this0,right)

    /**  */
    // case "," => 
    //   val List(left,right) = parameters // Unknown,Unknown
    //   TopWithInvalid[S](TUnknown.typ)
    // DECLARATION AS FIELD: 
    //   /**  */
    //   val field_, = new TouchField(",",TUnknown.typ)

    // FIELDS: field_:=, field_,

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
