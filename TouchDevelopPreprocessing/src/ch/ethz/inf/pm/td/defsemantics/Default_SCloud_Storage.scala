/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Cloud Storage
 *
 * OneDrive, OneNote operations
 *
 * @author Lucas Brutschy
 */

trait Default_SCloud_Storage extends ASingleton {

  lazy val typeName = TypeName("Cloud Storage", isSingleton = true)
          
  /** Never used: Creates a OneNote page from the given HTML fragment. The page can be uploaded using `cloud storage->upload note'. */
  def member_create_note = ApiMember(
    name = "create note",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TForm_Builder,
    semantics = DefaultSemantics
  )

  /** Never used: Uploads a new OneNote page and returns the web client url if successful. The 'Presentation' field must contain the well-formed HTML. Additional pictures can be stored in other fields. */
  def member_upload_note = ApiMember(
    name = "upload note",
    paramTypes = List(ApiParam(TForm_Builder)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Prompts the user to upload a picture to OneDrive. If the filename is empty, a default filename gets generated. */
  def member_upload_picture = ApiMember(
    name = "upload picture",
    paramTypes = List(ApiParam(TPicture), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TCloud_Picture,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "create note" -> member_create_note,
    "upload note" -> member_upload_note,
    "upload picture" -> member_upload_picture
  )
            

}
          
