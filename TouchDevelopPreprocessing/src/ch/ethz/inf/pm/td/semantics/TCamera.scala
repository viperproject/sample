
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Camera
 *
 * The front or back camera
 *
 * @author Lucas Brutschy
 */ 

object TCamera extends AAny {

  /** Gets the height of the camera image in pixels. */
  lazy val field_height = new TouchField("height",TNumber.typeName)

  /** Gets the width of the camera image in pixels. */
  lazy val field_width = new TouchField("width",TNumber.typeName)

  /** Indicates if this camera is in front of the phone; false if this is the primary (back) camera. */
  lazy val field_is_front = new TouchField("is front",TBoolean.typeName)

  lazy val typeName = TypeName("Camera")

  override def possibleFields = super.possibleFields ++ List(field_height, field_is_front, field_width)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Takes a low quality picture from the camera. */
    case "preview" =>
      Top[S](TPicture)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
