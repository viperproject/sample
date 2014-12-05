
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Languages
 *
 * Translation, speech to text, ...
 *
 * @author Lucas Brutschy
 */

trait Default_SLanguages extends ASingleton {

  lazy val typeName = TypeName("Languages")
          
  /** Sometimes used: Gets the current language code, to be used in the 'translate' method. */
  def member_current_language = ApiMember(
    name = "current language",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Automatically detects the language of a given text using Bing. */
  def member_detect_language = ApiMember(
    name = "detect language",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Extracts text in the picture using Project Hawaii from Microsoft Research. */
  def member_picture_to_text = ApiMember(
    name = "picture to text",
    paramTypes = List(ApiParam(TString), ApiParam(TPicture)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Converts the microphone dictation to text. */
  def member_record_text = ApiMember(
    name = "record text",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Speaks the SSML markup immediately using the text-to-speech engine on the device. */
  def member_speak_ssml = ApiMember(
    name = "speak ssml",
    paramTypes = List(ApiParam(TXml_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Speaks the text immediately using the text-to-speech engine on the device. */
  def member_speak_text = ApiMember(
    name = "speak text",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] This api was renamed. Use `speak_text` instead. */
  def member_speak = ApiMember(
    name = "speak",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TSound,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**not implemented**] [**obsolete**] Converts a sound to a text using Project Hawaii from Microsoft Research. */
  def member_speech_to_text = ApiMember(
    name = "speech to text",
    paramTypes = List(ApiParam(TString), ApiParam(TSound)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Translates some text between two languages using Bing. Empty source language to auto-detect. */
  def member_translate = ApiMember(
    name = "translate",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "current language" -> member_current_language,
    "detect language" -> member_detect_language,
    "picture to text" -> member_picture_to_text,
    "record text" -> member_record_text,
    "speak ssml" -> member_speak_ssml,
    "speak text" -> member_speak_text,
    "speak" -> member_speak,
    "speech to text" -> member_speech_to_text,
    "translate" -> member_translate
  )
            

}
          
