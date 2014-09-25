
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{TopTouchType, TouchType}
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Wall
 *
 * Ask or display values on the wall...
 *
 * @author Lucas Brutschy
 */

object SWall extends ASingleton {

  /** Gets the list of available page button names. */
  lazy val field_button_icon_names = new TouchField("button icon names", TString_Collection.typeName)

  /** Gets the width of the screen (in pixels). */
  lazy val field_width = new TouchField("width", TNumber.typeName)

  /** Gets the height of the screen (in pixels). */
  lazy val field_height = new TouchField("height", TNumber.typeName)

  /** Sets the wall background camera. */
  lazy val field_background_camera = new TouchField("background camera", TCamera.typeName)

  /** Sets the wall background picture. The picture will be resized and clipped to the screen background as needed. */
  lazy val field_background_picture = new TouchField("background picture", TPicture.typeName)

  /** Sets the wall background color. */
  lazy val field_background = new TouchField("background", TColor.typeName)

  /** Sets the wall foreground color of elements. */
  lazy val field_foreground = new TouchField("foreground", TColor.typeName)

  /** [**dbg**] Sets the animation for push/pop of pages. */
  lazy val field_page_transition_style = new TouchField("page transition style", TString.typeName)

  /** Reverses the elements on the wall and inserts new ones at the bottom. */
  lazy val field_reversed = new TouchField("reversed", TBoolean.typeName)

  /** Sets the subtitle of the wall. */
  lazy val field_subtitle = new TouchField("subtitle", TBoolean.typeName)

  /** Sets the title of the wall. */
  lazy val field_title = new TouchField("title", TBoolean.typeName)

  /** Returns the current back stack of pages, starting from the current page to the bottom page. */
  lazy val field_pages = new TouchField("pages", TPage_Collection.typeName)

  /** Indicates whether to show or hide the search icon */
  lazy val field_display_search = new TouchField("display search", TBoolean.typeName)

  lazy val typeName = TypeName("Wall")

  override def possibleFields = super.possibleFields ++ List(
    field_button_icon_names,
    field_width,
    field_height,
    field_background_camera,
    field_background_picture,
    field_background,
    field_foreground,
    field_page_transition_style,
    field_reversed,
    field_subtitle,
    field_title,
    field_pages,
    field_display_search
  )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Add a new button. icon must be the name of a built-in icon, text must be non-empty. */
    case "add button" =>
      val List(icon, text) = parameters // String,String
    val pages = Field[S](this0, SWall.field_pages)
      val currentPage = TPage_Collection.collectionAt[S](pages, TPage_Collection.collectionSize[S](pages) - 1)
      New[S](TPage_Button, initials = Map(
        TPage_Button.field_icon -> icon,
        TPage_Button.field_text -> text,
        TPage_Button.field_page -> currentPage
      ))

    /** Prompts the user with ok and cancel buttons */
    case "ask boolean" =>
      val List(text, caption) = parameters // String,String
      Top[S](TBoolean)

    /** Prompts the user to input a number */
    case "ask number" =>
      val List(text) = parameters // String
      Top[S](TNumber)

    /** Prompts the user to input a string */
    case "ask string" =>
      val List(text) = parameters // String
      Top[S](TString)

    /** Clears the background color, picture and camera */
    case "clear background" =>
      var curState = state
      curState = AssignField[S](this0, SWall.field_background, Invalid(TColor, "background may have been cleared"))(curState, pp)
      curState = AssignField[S](this0, SWall.field_background_camera, Invalid(TCamera, "background camera may have been cleared"))(curState, pp)
      curState = AssignField[S](this0, SWall.field_background_picture, Invalid(TPicture, "background picture may have been cleared"))(curState, pp)
      curState

    /** Gets the current page displayed on the wall */
    case "current page" =>
      val pages = Field[S](this0, SWall.field_pages)
      Return[S](TPage_Collection.collectionAt[S](pages, TPage_Collection.collectionSize[S](pages) - 1))

    /** Clears the application bar buttons and hides the bar */
    case "clear buttons" =>
      Skip

    /** Clears the background, buttons and entries */
    case "clear" =>
      var curState = state
      curState = CallApi[S](this0, "clear background", Nil, TNothing)(curState, pp)
      curState = CallApi[S](this0, "clear buttons", Nil, TNothing)(curState, pp)
      curState

    /** Creates an updatable text box */
    case "create text box" =>
      val List(text, font_size) = parameters // String,Number
      New[S](TTextBox, initials = Map(TTextBox.field_text -> text, TTextBox.field_font_size -> font_size))

    /** Indicates whether to show or hide the search icon */
    case "display search" =>
      val List(on) = parameters // Boolean
      AssignField[S](this0, SWall.field_display_search, on)

    /** Use button icon names instead. */
    case "icon names" =>
      Return[S](Field[S](this0, SWall.field_button_icon_names))

    /** Prompts the user to pick a date. Returns a datetime whose date is set, the time is 12:00:00. */
    case "pick date" =>
      val List(text, caption) = parameters // String,String
      TopWithInvalid[S](TDateTime, "user may cancel date selection") // INVALID VALUE VERIFIED IN ONLINE VERSION

    /** Prompts the user to pick a string from a list. Returns the selected index. */
    case "pick string" =>
      val List(text, caption, values) = parameters // String,String,String_Collection
      If(TPage_Collection.collectionSize[S](values) > 0, Then = { s: S => Return[S](0 ndTo (TPage_Collection.collectionSize[S](values) - 1))(s, pp)},
        Else = { s: S => Error[S](True, "pick string", "User may have to select string from empty string collection")})

    /** Prompts the user to pick a time. Returns a datetime whose time is set, the date is undefined. */
    case "pick time" =>
      val List(text, caption) = parameters // String,String
      TopWithInvalid[S](TDateTime, "user may cancel time selection")

    /** Same as `wall->pop_page`, but lets you use specific animation. */
    case "pop page with transition" =>
      val List(style) = parameters // String
    val pages = Field[S](this0, SWall.field_pages)
      If[S](TPage_Collection.collectionSize[S](pages) > 0, Then = { s: S =>
        Return[S](True)(CollectionRemoveFirst[S](pages, TPage_Collection.collectionAt[S](pages, TPage_Collection.collectionSize[S](pages) - 1))(s, pp), pp)
      }, Else = {
        Return[S](False)(_, pp)
      })

    /** Pops the current page and restores the previous wall page. Returns false if already on the default page. */
    case "pop page" =>
      val pages = Field[S](this0, SWall.field_pages)
      If[S](TPage_Collection.collectionSize[S](pages) > 0, Then = { s: S =>
        Return[S](True)(TPage_Collection.collectionRemoveFirst[S](pages, TPage_Collection.collectionAt[S](pages, TPage_Collection.collectionSize[S](pages) - 1))(s, pp), pp)
      }, Else = {
        Return[S](False)(_, pp)
      })

    /** Prompts the user with a ok button */
    case "prompt" =>
      val List(text) = parameters // String
      Skip

    /** Pushes an empty page on the wall. */
    case "push new page" =>
      val pages = Field[S](this0, SWall.field_pages)
      var curState = state
      curState = New[S](TPage)(curState, pp)
      val newPage = curState.expr
      curState = TPage_Collection.collectionInsert[S](pages, TPage_Collection.collectionSize[S](pages), newPage)(curState, pp)
      curState = TPage_Collection.collectionIncreaseLength[S](pages)(curState, pp)
      Return[S](newPage)(curState, pp)

    /** Takes a screenshot of the wall. */
    case "screenshot" =>
      Top[S](TPicture)

    /** Sets the 3x3 affine matrix transformation applied to the wall. */
    case "set transform matrix" =>
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
