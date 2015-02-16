
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{TopTouchType, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_SWall
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Wall
 *
 * Ask or display values on the wall...
 *
 * @author Lucas Brutschy
 */

object SWall extends Default_SWall {

  /** Gets the list of available page button names. */
  lazy val field_button_icon_names = ApiField("button icon names", GCollection(TString))

  /** Gets the width of the screen (in pixels). */
  lazy val field_width = ApiField("width", TNumber)

  /** Gets the height of the screen (in pixels). */
  lazy val field_height = ApiField("height", TNumber)

  /** Sets the wall background camera. */
  lazy val field_background_camera = ApiField("background camera", TCamera)

  /** Sets the wall background picture. The picture will be resized and clipped to the screen background as needed. */
  lazy val field_background_picture = ApiField("background picture", TPicture)

  /** Sets the wall background color. */
  lazy val field_background = ApiField("background", TColor)

  /** Sets the wall foreground color of elements. */
  lazy val field_foreground = ApiField("foreground", TColor)

  /** [**dbg**] Sets the animation for push/pop of pages. */
  lazy val field_page_transition_style = ApiField("page transition style", TString)

  /** Reverses the elements on the wall and inserts new ones at the bottom. */
  lazy val field_reversed = ApiField("reversed", TBoolean)

  /** Sets the subtitle of the wall. */
  lazy val field_subtitle = ApiField("subtitle", TBoolean)

  /** Sets the title of the wall. */
  lazy val field_title = ApiField("title", TBoolean)

  /** Returns the current back stack of pages, starting from the current page to the bottom page. */
  lazy val field_pages = ApiField("pages", GCollection(TPage))

  /** Indicates whether to show or hide the search icon */
  lazy val field_display_search = ApiField("display search", TBoolean)

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
      val currentPage = GCollection(TPage).collectionAt[S](pages, GCollection(TPage).collectionSize[S](pages) - 1)
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
      Return[S](GCollection(TPage).collectionAt[S](pages, GCollection(TPage).collectionSize[S](pages) - 1))

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
      If(GCollection(TString).collectionSize[S](values) > 0,
        Then = { s: S => Return[S](0 ndToIncl (GCollection(TString).collectionSize[S](values) - 1))(s, pp)},
        Else = { s: S => Error[S](True, "pick string", "User may have to select string from empty string collection")})

    /** Prompts the user to pick a time. Returns a datetime whose time is set, the date is undefined. */
    case "pick time" =>
      val List(text, caption) = parameters // String,String
      TopWithInvalid[S](TDateTime, "user may cancel time selection")

    /** Same as `wall->pop_page`, but lets you use specific animation. */
    case "pop page with transition" =>
      val List(style) = parameters // String
    val pages = Field[S](this0, SWall.field_pages)
      If[S](GCollection(TPage).collectionSize[S](pages) > 0, Then = { s: S =>
        Return[S](True)(GCollection(TPage).collectionRemoveFirst[S](pages, GCollection(TPage).collectionAt[S](pages, GCollection(TPage).collectionSize[S](pages) - 1))(s, pp), pp)
      }, Else = {
        Return[S](False)(_, pp)
      })

    /** Pops the current page and restores the previous wall page. Returns false if already on the default page. */
    case "pop page" =>
      val pages = Field[S](this0, SWall.field_pages)
      If[S](GCollection(TPage).collectionSize[S](pages) > 0, Then = { s: S =>
        Return[S](True)(GCollection(TPage).collectionRemoveFirst[S](pages, GCollection(TPage).collectionAt[S](pages, GCollection(TPage).collectionSize[S](pages) - 1))(s, pp), pp)
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
      curState = GCollection(TPage).collectionInsert[S](pages, GCollection(TPage).collectionSize[S](pages), newPage)(curState, pp)
      curState = GCollection(TPage).collectionIncreaseLength[S](pages)(curState, pp)
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
      
