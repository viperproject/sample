
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Media Server Collection
 *
 * A collection of media servers
 *
 * @author Lucas Brutschy
 */ 

object TMedia_Server_Collection {

  val typName = "Media Server Collection"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TMedia_Server_Collection extends AAny {

  def getTyp = TMedia_Server_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the media player at index */
    // case "at" => 
    //   val List(index) = parameters // Number
    //   Return[S](Valid(TMedia_Server.typ))

    /** Gets the number of media players */
    // case "count" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the number of media players */
    //   val field_count = new TouchField("count",TNumber.typ)

    // FIELDS: , field_count

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
