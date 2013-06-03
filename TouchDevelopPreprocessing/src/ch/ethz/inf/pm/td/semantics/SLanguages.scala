
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of languages
 *
 * Translation, speech to text, ...
 *
 * @author Lucas Brutschy
 */ 

object SLanguages {

  /** Gets the current language code, to be used in the 'translate' method. */
  val field_current_language = new TouchField("current language",TString.typName)

  val typName = "Languages"
  val typ = new TouchType(typName,isSingleton = true, fields = List(field_current_language))

}

class SLanguages extends AAny {

  def getTyp = SLanguages.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Automatically detects the language of a given text using Bing. */
    case "detect language" =>
      val List(text) = parameters // String
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"detect language",
        "Check if the device is connected to the internet before using the connection")
      Top[S](TString.typ)

    /** Extracts text in the picture using Project Hawaii from Microsoft Research. */
    case "picture to text" =>
      val List(lang,pic) = parameters // String,Picture
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"picture to text",
        "Check if the device is connected to the internet before using the connection")
      Top[S](TString.typ)

    /** Converts the microphone dictation to text using Project Hawaii from Microsoft Research. */
    case "record text" =>
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"record text",
        "Check if the device is connected to the internet before using the connection")
      Top[S](TString.typ)

    /** Speaks the text in the specified language using Bing. */
    case "speak" =>
      val List(lang,text) = parameters // String,String
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"speak",
        "Check if the device is connected to the internet before using the connection")
      Top[S](TSound.typ)

    /** Converts a sound to a text using Project Hawaii from Microsoft Research. */
    case "speech to text" =>
      val List(lang,speech) = parameters // String,Sound
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"speech to text",
        "Check if the device is connected to the internet before using the connection")
      Top[S](TString.typ)

    /** Translates some text between two languages using Bing. Empty source language to auto-detect. */
    case "translate" =>
      val List(source_lang,target_lang,text) = parameters // String,String,String
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"translate",
        "Check if the device is connected to the internet before using the connection")
      Top[S](TString.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
