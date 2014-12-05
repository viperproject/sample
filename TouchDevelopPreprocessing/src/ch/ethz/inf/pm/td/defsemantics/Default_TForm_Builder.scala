
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Form Builder
 *
 * A builder to create HTML Form data
 *
 * @author Lucas Brutschy
 */

trait Default_TForm_Builder extends AAny {

  lazy val typeName = TypeName("Form Builder")
          
  /** Never used: Adds a boolean value */
  def member_add_boolean = ApiMember(
    name = "add boolean",
    paramTypes = List(ApiParam(TString), ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a buffer as an attached file */
  def member_add_buffer = ApiMember(
    name = "add buffer",
    paramTypes = List(ApiParam(TString), ApiParam(TBuffer), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a number value */
  def member_add_number = ApiMember(
    name = "add number",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a picture */
  def member_add_picture = ApiMember(
    name = "add picture",
    paramTypes = List(ApiParam(TString), ApiParam(TPicture), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a buffer as an attached file */
  def member_add_string_as_file = ApiMember(
    name = "add string as file",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a string value */
  def member_add_string = ApiMember(
    name = "add string",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a piece of text to the form using a custom type and/or file name. */
  def member_add_text = ApiMember(
    name = "add text",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add boolean" -> member_add_boolean,
    "add buffer" -> member_add_buffer,
    "add number" -> member_add_number,
    "add picture" -> member_add_picture,
    "add string as file" -> member_add_string_as_file,
    "add string" -> member_add_string,
    "add text" -> member_add_text
  )
            

}
          
