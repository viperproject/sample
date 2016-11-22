/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TopInitializer, ApiField, RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_SLanguages
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of languages
 *
 * Translation, speech to text, ...
 *
 * @author Lucas Brutschy
 */

object SLanguages extends Default_SLanguages {

  /** Gets the current language code, to be used in the 'translate' method. */
  lazy val field_current_language = ApiField("current language", TString, TopInitializer)

  override def possibleFields = super.possibleFields ++ List(field_current_language)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Automatically detects the language of a given text using Bing. */
    case "detect language" =>
      val List(text) = parameters // String
      if (TouchAnalysisParameters.get.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "detect language",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TString, "language detection service may not be reachable")

    /** Extracts text in the picture using Project Hawaii from Microsoft Research. */
    case "picture to text" =>
      val List(lang, pic) = parameters // String,Picture
      if (TouchAnalysisParameters.get.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "picture to text",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TString, "text recognition service may not be reachable")

    /** Converts the microphone dictation to text using Project Hawaii from Microsoft Research. */
    case "record text" =>
      if (TouchAnalysisParameters.get.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "record text",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TString, "speech recognition service may not be reachable")


    /** Speaks the text immediately using the text-to-speech engine on the device. */
    case "speak text" =>
      val List(voice_language, voice_gender, text) = parameters // String,String,String
      TopWithInvalid[S](TSound, "speech synthesis service may not be available")

    /** This api was renamed. Use `speak_text` instead. */
    case "speak" =>
      val List(lang, text) = parameters // String,String
      TopWithInvalid[S](TSound, "speech synthesis service may not be reachable")

    /** Converts a sound to a text using Project Hawaii from Microsoft Research. */
    case "speech to text" =>
      val List(lang, speech) = parameters // String,Sound
      if (TouchAnalysisParameters.get.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "speech to text",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TString, "speech recognition service may not be reachable")

    /** Translates some text between two languages using Bing. Empty source language to auto-detect. */
    case "translate" =>
      val List(source_lang, target_lang, text) = parameters // String,String,String
      if (TouchAnalysisParameters.get.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "translate",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TString, "translation service may not be reachable")

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
