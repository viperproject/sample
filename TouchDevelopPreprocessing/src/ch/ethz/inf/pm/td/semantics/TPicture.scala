package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{VariableIdentifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:11 PM
 */
object TPicture {

  val field_width = new TouchField("width", TNumber.typ)
  val field_height = new TouchField("height", TNumber.typ)

  val typName = "Picture"
  val typ = TouchType(typName,isSingleton = false,List(field_width,field_height))

}

class TPicture extends Any {

  def getTypeName = TPicture.typName

  def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String,parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    case "set_pixel" =>
      val List(x,y,color) = parameters

      Error (x < 0, "set_pixel: Parameter X ("+x+") might be negative")(state,pp)
      Error (y < 0, "set_pixel: Parameter Y ("+y+") might be negative")(state,pp)
      Error (x >= Field[S](this0,TPicture.field_width), "set_pixel: Parameter X ("+x+") might be greater than width")(state,pp)
      Error (y >= Field[S](this0,TPicture.field_height), "set_pixel: Parameter Y ("+y+") might be greater than height")(state,pp)

      Skip

    case "draw_text" =>
      val List(x,y,text,font,degree,color) = parameters

      Error (x < 0, "draw_text: Parameter X ("+x+") might be negative")(state,pp)
      Error (y < 0, "draw_text: Parameter Y ("+y+") might be negative")(state,pp)
      Error (x >= Field[S](this0,TPicture.field_width), "draw_text: Parameter X ("+x+") might be greater than width")(state,pp)
      Error (y >= Field[S](this0,TPicture.field_height), "draw_text: Parameter Y ("+y+") might be greater than height")(state,pp)

      Skip

    case "save_to_library" =>
      Skip // TODO: Update environment, we have a picture

    case "update_on_wall" =>
      Skip // TODO: Update environment, store reference?

    case _ =>
      Unimplemented[S](this0.getType().toString+"."+method)

  }

}
