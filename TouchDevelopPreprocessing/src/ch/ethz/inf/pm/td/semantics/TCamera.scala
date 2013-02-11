
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Camera
 *
 * The front or back camera
 *
 * @author Lucas Brutschy
 */ 

object TCamera {

  val typName = "Camera"
  val typ = new TouchType(typName,isSingleton = false,List())

}

class TCamera extends AAny {

  def getTyp = TCamera.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the height of the camera image in pixels. */
    // case "height" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the height of the camera image in pixels. */
    //   val field_height = new TouchField("height",TNumber.typ)

    /** Indicates if this camera is in front of the phone; false if this is the primary (back) camera. */
    // case "is_front" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if this camera is in front of the phone; false if this is the primary (back) camera. */
    //   val field_is_front = new TouchField("is_front",TBoolean.typ)

    /** Takes a low quality picture from the camera. */
    // case "preview" => 
    //   Return[S](Valid(TPicture.typ))
    // DECLARATION AS FIELD: 
    //   /** Takes a low quality picture from the camera. */
    //   val field_preview = new TouchField("preview",TPicture.typ)

    /** Gets the width of the camera image in pixels. */
    // case "width" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the width of the camera image in pixels. */
    //   val field_width = new TouchField("width",TNumber.typ)

    // FIELDS: , field_height, field_is_front, field_preview, field_width

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
