
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of tags
 *
 * 2D barcode generation and scanning
 *
 * @author Lucas Brutschy
 */ 

object STags {

  val typName = "tags"
  val typ = TouchType(typName,isSingleton = true,List())

}

class STags extends AAny {

  def getTyp = STags.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Generates a 2D barcode pointing to the text using Microsoft Tag. text must be less than 1000 character long and size must be between 0.75 and 5 inches. */
    // case "tag_text" => 
    //   val List(text,size,bw) = parameters // String,Number,Boolean
    //   Return[S](Valid(TPicture.typ))

    /** Generates a 2D barcode pointing to the url using Microsoft Tag. url must be less than 1000 character long and size must be between 0.75 and 5 inches. */
    // case "tag_url" => 
    //   val List(url,size,bw) = parameters // String,Number,Boolean
    //   Return[S](Valid(TPicture.typ))

    // FIELDS: 

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
