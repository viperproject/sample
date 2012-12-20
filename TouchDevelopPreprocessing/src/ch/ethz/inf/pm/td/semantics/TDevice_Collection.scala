
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Device Collection
 *
 * A collection of devices
 *
 * @author Lucas Brutschy
 */ 

object TDevice_Collection {

  val typName = "Device Collection"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TDevice_Collection extends AAny {

  def getTyp = TDevice_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the device at index */
    // case "at" => 
    //   val List(index) = parameters // Number
    //   Return[S](Valid(TDevice.typ))

    /** Gets the number of devices */
    // case "count" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the number of devices */
    //   val field_count = new TouchField("count",TNumber.typ)

    // FIELDS: , field_count

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
