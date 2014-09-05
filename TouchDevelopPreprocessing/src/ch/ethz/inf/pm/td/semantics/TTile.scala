package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ExpressionInitializer, TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 12/3/12
 * Time: 2:04 PM
 */

object TTile extends AAny {

  /** Gets the back icon picture */
  lazy val field_back_icon = new TouchField("back icon", TPicture.typeName)

  /** Gets the back title */
  lazy val field_back_title = new TouchField("back title", TString.typeName)

  /** Gets the background color */
  lazy val field_background = new TouchField("background", TColor.typeName)

  /** Gets the content */
  lazy val field_content = new TouchField("content", TString.typeName)

  /** Gets the counter */
  lazy val field_counter = new TouchField("counter", TNumber.typeName)

  /** Gets the height in pixels */
  lazy val field_height = new TouchField("height", TNumber.typeName,
    default = ExpressionInitializer(0 ndTo PositiveInfinity(null)),
    topDefault = ExpressionInitializer(0 ndTo PositiveInfinity(null)))

  /** Gets the icon picture */
  lazy val field_icon = new TouchField("icon", TPicture.typeName)

  /** This property is deprecated. */
  lazy val field_panorama = new TouchField("panorama", TBoolean.typeName)

  /** Gets the front title */
  lazy val field_title = new TouchField("title", TString.typeName)

  /** Gets the width in pixels */
  lazy val field_width = new TouchField("width", TNumber.typeName,
    default = ExpressionInitializer(0 ndTo PositiveInfinity(null)),
    topDefault = ExpressionInitializer(0 ndTo PositiveInfinity(null)))

  val typeName = TypeName("Tile")

  override def possibleFields = super.possibleFields ++ List(
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
  )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)(implicit pp: ProgramPoint, state: S): S = method match {

    /** Clears the back icon image if any */
    case "clear back icon" =>
      AssignField[S](this0, TTile.field_back_icon, Invalid(TPicture, "back icon may have been cleared"))

    /** Clears the front icon image if any */
    case "clear icon" =>
      AssignField[S](this0, TTile.field_icon, Invalid(TPicture, "icon may have been cleared"))

    /** Pins the tile to the start menu, after asking for user consent. */
    case "pin to start" =>
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}