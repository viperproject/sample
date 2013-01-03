
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Page Collection
 *
 * A collection of page
 *
 * @author Lucas Brutschy
 */ 

object TPage_Collection {

  val typName = "Page Collection"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TPage_Collection extends AAny {

  def getTyp = TPage_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the pages at index */
    // case "at" => 
    //   val List(index) = parameters // Number
    //   Return[S](Valid(TPage.typ))

    /** Gets the number of pages */
    // case "count" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the number of pages */
    //   val field_count = new TouchField("count",TNumber.typ)

    // FIELDS: , field_count

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
