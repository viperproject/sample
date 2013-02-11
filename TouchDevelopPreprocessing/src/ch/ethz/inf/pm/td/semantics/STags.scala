
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.semantics.RichExpression._

/**
 * Specifies the abstract semantics of tags
 *
 * 2D barcode generation and scanning
 *
 * @author Lucas Brutschy
 */ 

object STags {

  val typName = "tags"
  val typ = new TouchType(typName,isSingleton = true,List())

}

class STags extends AAny {

  def getTyp = STags.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Generates a 2D barcode pointing to the text using Microsoft Tag. text must be less than 1000 character long and size must be between 0.75 and 5 inches. */
    case "tag_text" =>
      val List(text,size,bw) = parameters // String,Number,Boolean
      // TODO: Add check for text size
      CheckInRangeInclusive[S](size,0.75,5,"tag_url","size")
      New[S](TPicture.typ,Map(TPicture.field_width.asInstanceOf[Identifier] -> toRichExpression(601), TPicture.field_height -> toRichExpression(601)))

    /** Generates a 2D barcode pointing to the url using Microsoft Tag. url must be less than 1000 character long and size must be between 0.75 and 5 inches. */
    case "tag_url" =>
      val List(url,size,bw) = parameters // String,Number,Boolean
      // TODO: Add check for text size
      CheckInRangeInclusive[S](size,0.75,5,"tag_url","size")
      New[S](TPicture.typ,Map(TPicture.field_width.asInstanceOf[Identifier] -> toRichExpression(601), TPicture.field_height -> toRichExpression(601)))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
