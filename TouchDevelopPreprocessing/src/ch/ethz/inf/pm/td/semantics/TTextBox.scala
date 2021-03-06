/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_TTextBox
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of TextBox
 *
 * A text box
 *
 * @author Lucas Brutschy
 */

object TTextBox extends Default_TTextBox {

  lazy val field_background = ApiField("background", TColor)
  lazy val field_border = ApiField("border", TColor)
  lazy val field_font_size = ApiField("font size", TNumber)
  lazy val field_foreground = ApiField("foreground", TColor)
  lazy val field_icon = ApiField("icon", TPicture)
  lazy val field_text = ApiField("text", TString)

  override def mutedFields = super.mutedFields ++ List(field_background,field_border,field_font_size,field_foreground,field_icon,field_text)

  override def possibleFields = super.possibleFields ++ List(field_background,field_border,field_font_size,field_foreground,field_icon,field_text)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Sets the font size (small = 14, normal = 15, medium = 17, medium large = 19, large = 24, extra large = 32,
      * extra extra large = 54, huge = 140 */
     case "set font size" =>
       val List(size) = parameters // Number
       //if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
       // CheckInRangeInclusive[S](size,14,140,"set font size","size") THIS IS NOT VALID ANYMORE
       //}
       super.forwardSemantics(this0,method,parameters,returnedType) // Handle setter

    /** Sets the icon picture (max 96 x 96) */
    case "set icon" =>
       val List(pic) = parameters // Picture
       if (TouchAnalysisParameters.get.reportNoncriticalParameterBoundViolations) {
         CheckInRangeInclusive[S](Field[S](pic,TPicture.field_width),0,96,"set icon","Icon Width")
         CheckInRangeInclusive[S](Field[S](pic,TPicture.field_height),0,96,"set icon","Icon Height")
       }
       super.forwardSemantics(this0,method,parameters,returnedType) // Handle setter

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}