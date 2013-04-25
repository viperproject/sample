package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
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

  val typName = "Wall"
  val typ = new TouchType(typName, isSingleton = true)

}

class SWall extends AAny {

  def getTyp = SWall.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    /** Add a new button. icon must be the name of a built-in icon, text must be non-empty. */
    case "add button" =>
      val List(icon,text) = parameters // String,String
      New[S](TPage_Button.typ) // TODO

    /** Prompts the user with ok and cancel buttons */
    case "ask boolean" =>
      val List(text,caption) = parameters // String,String
      Top[S](TBoolean.typ)

    /** Prompts the user to input a number */
    // The javascript version does not return invalid but 0 if user cancels
    case "ask number" =>
      val List(text) = parameters // String
      Top[S](TNumber.typ)

    /** Prompts the user to input a string */
    // The javascript version does not return invalid but ""if user cancels
    case "ask string" =>
      val List(text) = parameters // String
      Top[S](TString.typ)

    /** Gets the list of available page button names. */
    case "button icon names" =>
      New[S](TString_Collection.typ) // TODO

    /** Clears the entries */
    case "clear" =>
      Skip; // TODO

    /** Clears the application bar buttons and hides the bar */
    case "clear buttons" =>
      Skip; // TODO

    /** Creates an updatable text box */
    case "create text box" =>
      val List(text,font_size) = parameters // String,Number
      New[S](TTextBox.typ) // TODO

    /** Gets the current page displayed on the wall */
    case "current page" =>
      New[S](TPage.typ) // TODO

    /** Indicates whether to show or hide the search icon */
    case "display search" =>
      val List(on) = parameters // Boolean
      Skip; // TODO

    /** Returns the current back stack of pages, starting from the current page to the bottom page. */
    case "pages" =>
      New[S](TPage_Collection.typ) // TODO

    /** Prompts the user to pick a date. Returns a datetime whose date is set, the time is 12:00:00. */
    case "pick date" =>
      val List(text,caption) = parameters // String,String
      New[S](TDateTime.typ) // TODO

    /** Prompts the user to pick a string from a list. Returns the selected index. */
    case "pick string" =>
      val List(text,caption,values) = parameters // String,String,String_Collection
      Return[S](Invalid(TNumber.typ),0 ndTo (CollectionSize[S](values) - 1))

    /** Prompts the user to pick a time. Returns a datetime whose time is set, the date is undefined. */
    case "pick time" =>
      val List(text,caption) = parameters // String,String
      New[S](TDateTime.typ) // TODO

    /** Pops the current page and restores the previous wall page. Returns false if already on the default page. */
    case "pop page" =>
      New[S](TBoolean.typ) // TODO

    /** Prompts the user with a ok button */
    case "prompt" =>
      val List(text) = parameters // String
      Skip; // TODO

    /** Pushes an empty page on the wall. */
    case "push new page" =>
      New[S](TPage.typ) // TODO

    /** Takes a screenshot of the wall. */
    case "screenshot" =>
      New[S](TPicture.typ) // TODO

    /** Sets the wall background color. */
    case "set background" =>
      val List(color) = parameters // Color
      Skip; // TODO

    /** Sets the wall background camera. */
    case "set background camera" =>
      val List(camera) = parameters // Camera
      Skip; // TODO

    /** Sets the wall background picture. The picture will be resized and clipped to the screen background as needed. */
    case "set background picture" =>
      val List(picture) = parameters // Picture
      Skip; // TODO

    /** Sets the wall foreground color of elements. */
    case "set foreground" =>
      val List(color) = parameters // Color
      Skip; // TODO

    /** Reverses the elements on the wall and inserts new ones at the bottom. */
    case "set reversed" =>
      val List(bottom) = parameters // Boolean
      Skip; // TODO

    /** Sets the subtitle of the wall. */
    case "set subtitle" =>
      val List(title) = parameters // String
      Skip; // TODO

    /** Sets the title of the wall. */
    case "set title" =>
      val List(title) = parameters // String
      Skip; // TODO

    /** Sets the 3x3 affine matrix transformation applied to the wall. */
    case "set transform matrix" =>
      val List(m11,m12,m21,m22,offsetx,offsety) = parameters // Number,Number,Number,Number,Number,Number
      Skip; // TODO

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)
  }
}
