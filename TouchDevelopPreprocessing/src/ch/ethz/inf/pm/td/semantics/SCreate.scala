
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Create
 *
 * Create collections of items.
 *
 * @author Lucas Brutschy
 */ 

object SCreate {

  val typName = "Create"
  val typ = DefaultTouchType(typName,isSingleton = false)

}

class SCreate extends AAny {

  def getTyp = SCreate.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** [**beta**] Creates an empty collection of arbitrary type */
    // case "collection of" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TUnfinished_Type.typ)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Creates an empty collection of arbitrary type */
    //   val field_collection_of = new TouchField("collection of",TUnfinished_Type.typName)

    // FIELDS: field_collection_of

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
