
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Cloud Picture
 *
 * A picture hosted on OneDrive.
 *
 * @author Lucas Brutschy
 */ 

object TCloud_Picture {

  val typName = "Cloud Picture"
  val typ = DefaultTouchType(typName)

}

class TCloud_Picture extends AAny {

  def getTyp = TCloud_Picture.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Downloads the picture with a particular size. */
    case "download picture" =>
    //  val List(media) = parameters // String
      TopWithInvalid[S](TPicture.typ,"download may fail")

    /** Gets the picture with a particular size. */
    case "to picture" =>
    //   val List(media) = parameters // String
       TopWithInvalid[S](TPicture.typ,"conversion may fail")

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
