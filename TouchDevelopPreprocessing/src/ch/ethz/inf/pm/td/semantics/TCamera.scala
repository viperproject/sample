
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
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

  /** Gets the height of the camera image in pixels. */
  val field_height = new TouchField("height",TNumber.typName)

  /** Gets the width of the camera image in pixels. */
  val field_width = new TouchField("width",TNumber.typName)

  /** Indicates if this camera is in front of the phone; false if this is the primary (back) camera. */
  val field_is_front = new TouchField("is front",TBoolean.typName)

  val typName = "Camera"
  val typ = DefaultTouchType(typName,isSingleton = false,fields = List(field_height, field_is_front, field_width))

}

class TCamera extends AAny {

  def getTyp = TCamera.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Takes a low quality picture from the camera. */
    case "preview" =>
      Top[S](TPicture.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
