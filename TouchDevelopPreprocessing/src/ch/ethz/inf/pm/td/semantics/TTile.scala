package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ExpressionInitializer, ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_TTile
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 12/3/12
 * Time: 2:04 PM
 */

object TTile extends Default_TTile {

  /** Gets the back icon picture */
  lazy val field_back_icon = ApiField("back icon", TPicture)

  /** Gets the back title */
  lazy val field_back_title = ApiField("back title", TString)

  /** Gets the background color */
  lazy val field_background = ApiField("background", TColor)

  /** Gets the content */
  lazy val field_content = ApiField("content", TString)

  /** Gets the counter */
  lazy val field_counter = ApiField("counter", TNumber)

  /** Gets the height in pixels */
  lazy val field_height = new ApiField("height", TNumber,
    default = ExpressionInitializer(0 ndTo PositiveInfinity(null)),
    topDefault = ExpressionInitializer(0 ndTo PositiveInfinity(null)))

  /** Gets the icon picture */
  lazy val field_icon = ApiField("icon", TPicture)

  /** This property is deprecated. */
  lazy val field_panorama = ApiField("panorama", TBoolean)

  /** Gets the front title */
  lazy val field_title = ApiField("title", TString)

  /** Gets the width in pixels */
  lazy val field_width = new ApiField("width", TNumber,
    default = ExpressionInitializer(0 ndTo PositiveInfinity(null)),
    topDefault = ExpressionInitializer(0 ndTo PositiveInfinity(null)))

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