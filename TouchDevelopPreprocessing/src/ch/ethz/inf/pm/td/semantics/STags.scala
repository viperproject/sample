/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_STags
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of tags
 *
 * 2D barcode generation and scanning
 *
 * @author Lucas Brutschy
 */

object STags extends Default_STags {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {


    /** Receives a picture through NFC. */
    case "nfc receive picture" =>
      TopWithInvalid[S](TPicture, "NFC may not be available")

    /** Receives text through NFC. `type` may also be a mime type. */
    case "nfc receive" =>
      val List(typ) = parameters // String
      TopWithInvalid[S](TString, "NFC may not be available")

    /** Sends a url, text or any other format using NFC. `type` may be a mime type. */
    case "nfc send picture" =>
      val List(pic) = parameters // Picture
      Skip

    /** Sends a url, text or any other text format using NFC. `type` may be a mime type. */
    case "nfc send" =>
      val List(typ, value) = parameters // String,String
      Skip

    /** Writes a static NFC tag with url, text or any other format. `type` may be a mime type. */
    case "nfc write tag" =>
      val List(typ, value) = parameters // String,String
      Skip

    /** [**dbg**] Scans an id tag create by TouchDevelop and returns the embeded text. */
    case "scan" =>
      val List() = parameters //
      TopWithInvalid[S](TString, "ID tag may not be successfully scanned")
    // DECLARATION AS FIELD:
    //   /** [**dbg**] Scans an id tag create by TouchDevelop and returns the embeded text. */
    //   lazy val field_scan = new TouchField("scan",TString)

    /** Generates a 2D barcode pointing to the text using Microsoft Tag. text must be less than 1000 character long and size must be between 0.75 and 5 inches. */
    case "tag text" =>
      val List(text, size, bw) = parameters // String,Number,Boolean
      // TODO: Add check for text size
      if (TouchAnalysisParameters.get.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](size, 0.75, 5, "tag url", "size")
      }
      New[S](TPicture, Map(
        TPicture.field_width -> toRichExpression(601),
        TPicture.field_height -> toRichExpression(601)
      ))

    /** Generates a 2D barcode pointing to the url using Microsoft Tag. url must be less than 1000 character long and size must be between 0.75 and 5 inches. */
    case "tag url" =>
      val List(url, size, bw) = parameters // String,Number,Boolean
      // TODO: Add check for text size
      if (TouchAnalysisParameters.get.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](size, 0.75, 5, "tag url", "size")
      }
      New[S](TPicture, Map(
        TPicture.field_width -> toRichExpression(601),
        TPicture.field_height -> toRichExpression(601)
      ))

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
