
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

  val typName = "languages"
  val typ = new TouchType(typName,isSingleton = true,List())

}

class SLanguages extends AAny {

  def getTyp = SLanguages.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the current language code, to be used in the 'translate' method. */
    // case "current_language" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the current language code, to be used in the 'translate' method. */
    //   val field_current_language = new TouchField("current_language",TString.typ)

    /** Automatically detects the language of a given text using Bing. */
    // case "detect_language" => 
    //   val List(text) = parameters // String
    //   Return[S](Valid(TString.typ))

    /** Extracts text in the picture using Project Hawaii from Microsoft Research. */
    // case "picture_to_text" => 
    //   val List(lang,pic) = parameters // String,Picture
    //   Return[S](Valid(TString.typ))

    /** Converts the microphone dictation to text using Project Hawaii from Microsoft Research. */
    // case "record_text" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Converts the microphone dictation to text using Project Hawaii from Microsoft Research. */
    //   val field_record_text = new TouchField("record_text",TString.typ)

    /** Speaks the text in the specified language using Bing. */
    // case "speak" => 
    //   val List(lang,text) = parameters // String,String
    //   Return[S](Valid(TSound.typ))

    /** Converts a sound to a text using Project Hawaii from Microsoft Research. */
    // case "speech_to_text" => 
    //   val List(lang,speech) = parameters // String,Sound
    //   Return[S](Valid(TString.typ))

    /** Translates some text between two languages using Bing. Empty source language to auto-detect. */
    // case "translate" => 
    //   val List(source_lang,target_lang,text) = parameters // String,String,String
    //   Return[S](Valid(TString.typ))

    // FIELDS: , field_current_language, field_record_text

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
