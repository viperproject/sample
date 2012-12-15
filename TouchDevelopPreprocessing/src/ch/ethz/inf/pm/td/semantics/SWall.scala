package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 *
 * Semantics of the TouchDevelop singleton "wall"
 *
 * Ask or display values on the wall...
 *
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */

object SWall {

  val typName = "wall"
  val typ = TouchType(typName, isSingleton = true)

}

class SWall extends AAny {

  def getTyp = SWall.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Add a new button. icon must be the name of a built-in icon, text must be non-empty. */
    case "add_button" =>
      val List(icon,text) = parameters // String,String
      New[S](TPage_Button.typ) // TODO

    /** Prompts the user with ok and cancel buttons */
    case "ask_boolean" =>
      val List(text,caption) = parameters // String,String
      Return[S](Valid(TBoolean.typ))

    /** Prompts the user to input a number */
    case "ask_number" =>
      val List(text) = parameters // String
      Return[S](Valid(TNumber.typ))

    /** Prompts the user to input a string */
    case "ask_string" =>
      val List(text) = parameters // String
      New[S](TString.typ) // TODO

    /** Gets the list of available page button names. */
    case "button_icon_names" =>
      New[S](TString_Collection.typ) // TODO

    /** Clears the entries */
    case "clear" =>
      Skip; // TODO

    /** Clears the application bar buttons and hides the bar */
    case "clear_buttons" =>
      Skip; // TODO

    /** Creates an updatable text box */
    case "create_text_box" =>
      val List(text,font_size) = parameters // String,Number
      New[S](TTextBox.typ) // TODO

    /** Gets the current page displayed on the wall */
    case "current_page" =>
      New[S](TPage.typ) // TODO

    /** Indicates whether to show or hide the search icon */
    case "display_search" =>
      val List(on) = parameters // Boolean
      Skip; // TODO

    /** Returns the current back stack of pages, starting from the current page to the bottom page. */
    case "pages" =>
      New[S](TPage_Collection.typ) // TODO

    /** Prompts the user to pick a date. Returns a datetime whose date is set, the time is 12:00:00. */
    case "pick_date" =>
      val List(text,caption) = parameters // String,String
      New[S](TDateTime.typ) // TODO

    /** Prompts the user to pick a string from a list. Returns the selected index. */
    case "pick_string" =>
      val List(text,caption,values) = parameters // String,String,String_Collection
      New[S](TNumber.typ) // TODO

    /** Prompts the user to pick a time. Returns a datetime whose time is set, the date is undefined. */
    case "pick_time" =>
      val List(text,caption) = parameters // String,String
      New[S](TDateTime.typ) // TODO

    /** Pops the current page and restores the previous wall page. Returns false if already on the default page. */
    case "pop_page" =>
      New[S](TBoolean.typ) // TODO

    /** Prompts the user with a ok button */
    case "prompt" =>
      val List(text) = parameters // String
      Skip; // TODO

    /** Pushes an empty page on the wall. */
    case "push_new_page" =>
      New[S](TPage.typ) // TODO

    /** Takes a screenshot of the wall. */
    case "screenshot" =>
      New[S](TPicture.typ) // TODO

    /** Sets the wall background color. */
    case "set_background" =>
      val List(color) = parameters // Color
      Skip; // TODO

    /** Sets the wall background camera. */
    case "set_background_camera" =>
      val List(camera) = parameters // Camera
      Skip; // TODO

    /** Sets the wall background picture. The picture will be resized and clipped to the screen background as needed. */
    case "set_background_picture" =>
      val List(picture) = parameters // Picture
      Skip; // TODO

    /** Sets the wall foreground color of elements. */
    case "set_foreground" =>
      val List(color) = parameters // Color
      Skip; // TODO

    /** Reverses the elements on the wall and inserts new ones at the bottom. */
    case "set_reversed" =>
      val List(bottom) = parameters // Boolean
      Skip; // TODO

    /** Sets the subtitle of the wall. */
    case "set_subtitle" =>
      val List(title) = parameters // String
      Skip; // TODO

    /** Sets the title of the wall. */
    case "set_title" =>
      val List(title) = parameters // String
      Skip; // TODO

    /** Sets the 3x3 affine matrix transformation applied to the wall. */
    case "set_transform_matrix" =>
      val List(m11,m12,m21,m22,offsetx,offsety) = parameters // Number,Number,Number,Number,Number,Number
      Skip; // TODO

    case _ =>
      super.forwardSemantics(this0,method,parameters)
  }
}
