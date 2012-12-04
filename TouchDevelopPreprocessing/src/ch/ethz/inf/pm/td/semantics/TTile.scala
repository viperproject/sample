package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State, VariableIdentifier}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * User: lucas
 * Date: 12/3/12
 * Time: 2:04 PM
 */

object TTile {

  /** Gets the back icon picture */
  val field_back_icon = new TouchField("back_icon",TPicture.typ)

  /** Gets the back title */
  val field_back_title = new TouchField("back_title",TString.typ)

  /** Gets the background color */
  val field_background = new TouchField("background",TColor.typ)

  /** Gets the content */
  val field_content = new TouchField("content",TString.typ)

  /** Gets the counter */
  val field_counter = new TouchField("counter",TNumber.typ)

  /** Gets the height in pixels */
  val field_height = new TouchField("height",TNumber.typ)

  /** Gets the icon picture */
  val field_icon = new TouchField("icon",TPicture.typ)

  /** Gets the front title */
  val field_title = new TouchField("title",TString.typ)

  /** Gets the width in pixels */
  val field_width = new TouchField("width",TNumber.typ)

  val typName = "Tile"
  val typ = TouchType(typName,isSingleton = false,List(
    field_back_icon,
    field_back_title,
    field_background,
    field_content,
    field_counter,
    field_height,
    field_icon,
    field_title,
    field_width
  ))


}

class TTile extends Any {

  def getTyp = TTile.typ
  def getTypeName = getTyp.name

  def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Clears the back icon image if any */
    case "clear_back_icon" =>
      AssignField[S](this0,TTile.field_back_icon,Invalid(TPicture.typ))

    /** Clears the front icon image if any */
    case "clear_icon" =>
      AssignField[S](this0,TTile.field_icon,Invalid(TPicture.typ))

    /** Pins the tile to the start menu, after asking for user consent. */
    case "pin_to_start" =>
      Skip

    /** Sets the back icon image, cropped to the tile size */
    case "set_back_icon" =>
      val List(pic) = parameters // Picture
      AssignField[S](this0,TTile.field_back_icon,pic)

    /** Sets the back title. */
    case "set_back_title" =>
      val List(title) = parameters // String
      AssignField[S](this0,TTile.field_back_title,title)

    /** Sets the background color for the front and back tile. */
    case "set_background" =>
      val List(color) = parameters // Color
      AssignField[S](this0,TTile.field_background,color)

    /** Sets the text content that shows on the back tile. */
    case "set_content" =>
      val List(content) = parameters // String
      AssignField[S](this0,TTile.field_content,content)

    /** Sets the counter; counters â‰¤ 0 are not displayed. */
    case "set_counter" =>
      val List(counter) = parameters // Number
      AssignField[S](this0,TTile.field_counter,counter)

    /** Sets the front icon image, cropped to the tile size */
    case "set_icon" =>
      val List(pic) = parameters // Picture
      AssignField[S](this0,TTile.field_icon,pic)

    /** Sets the front title. If empty, the tile will use the script name */
    case "set_title" =>
      val List(title) = parameters // String
      AssignField[S](this0,TTile.field_title,title)

    case _ =>
      MatchFields[S](this0,parameters,getTyp,method)

  }
}