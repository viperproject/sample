package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._

/**
 * User: lucas
 * Date: 12/3/12
 * Time: 2:04 PM
 */

object TTile {

  /** Gets the back icon picture */
  val field_back_icon = new TouchField("back icon", TPicture.typName)

  /** Gets the back title */
  val field_back_title = new TouchField("back title", TString.typName)

  /** Gets the background color */
  val field_background = new TouchField("background", TColor.typName)

  /** Gets the content */
  val field_content = new TouchField("content", TString.typName)

  /** Gets the counter */
  val field_counter = new TouchField("counter", TNumber.typName)

  /** Gets the height in pixels */
  val field_height = new TouchField("height", TNumber.typName,
    default = ExpressionInitializer(0 ndTo PositiveInfinity(null)),
    topDefault = ExpressionInitializer(0 ndTo PositiveInfinity(null)))

  /** Gets the icon picture */
  val field_icon = new TouchField("icon", TPicture.typName)

  /** This property is deprecated. */
  val field_panorama = new TouchField("panorama", TBoolean.typName)

  /** Gets the front title */
  val field_title = new TouchField("title", TString.typName)

  /** Gets the width in pixels */
  val field_width = new TouchField("width", TNumber.typName,
    default = ExpressionInitializer(0 ndTo PositiveInfinity(null)),
    topDefault = ExpressionInitializer(0 ndTo PositiveInfinity(null)))

  val typName = "Tile"
  val typ = DefaultTouchType(typName, isSingleton = false, fields = List(
    field_back_icon,
    field_back_title,
    field_background,
    field_content,
    field_counter,
    field_height,
    field_icon,
    field_panorama,
    field_title,
    field_width
  ))


}

class TTile extends AAny {

  def getTyp = TTile.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)(implicit pp: ProgramPoint, state: S): S = method match {

    /** Clears the back icon image if any */
    case "clear back icon" =>
      AssignField[S](this0, TTile.field_back_icon, Invalid(TPicture.typ, "back icon may have been cleared"))

    /** Clears the front icon image if any */
    case "clear icon" =>
      AssignField[S](this0, TTile.field_icon, Invalid(TPicture.typ, "icon may have been cleared"))

    /** Pins the tile to the start menu, after asking for user consent. */
    case "pin to start" =>
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}