
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Media Link Collection
 *
 * A list of media links on the home network
 *
 * @author Lucas Brutschy
 */ 

object TMedia_Link_Collection {

  val typName = "Media Link Collection"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TMedia_Link_Collection extends AAny {

  def getTyp = TMedia_Link_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the i-th media link */
    // case "at" => 
    //   val List(index) = parameters // Number
    //   Return[S](Valid(TMedia_Link.typ))

    /** Gets the number of elements */
    // case "count" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the number of elements */
    //   val field_count = new TouchField("count",TNumber.typ)

    // FIELDS: , field_count

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
