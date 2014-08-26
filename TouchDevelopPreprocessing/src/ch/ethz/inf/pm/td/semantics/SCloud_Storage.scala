
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Cloud Storage
 *
 * OneDrive, OneNote operations
 *
 * @author Lucas Brutschy
 */ 

object SCloud_Storage {

  val typName = "Cloud Storage"
  val typ = DefaultTouchType(typName,isSingleton = true)

}

class SCloud_Storage extends AAny {

  def getTyp = SCloud_Storage.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Uploads a new OneNote page and returns the web client url if successful. The 'Presentation' field must contain the well-formed HTML. Additional pictures can be stored in other fields. */
    case "upload note" =>
    //   val List(form) = parameters // Form_Builder
      TopWithInvalid[S](TString.typ,"upload may fail")

    /** Prompts the user to upload a picture to OneDrive. If the filename is empty, a default filename gets generated. */
    case "upload picture" =>
    //   val List(pic,folder,filename) = parameters // Picture,String,String
      TopWithInvalid[S](TCloud_Picture.typ,"upload may fail")

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
