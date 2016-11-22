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
 * Specifies the abstract semantics of Picture Album
 *
 * A picture album
 *
 * @author Lucas Brutschy
 */

trait Default_TPicture_Album extends AAny {

  lazy val typeName = TypeName("Picture Album")
          
  /** Never used: Gets the children albums */
  def member_albums = ApiMember(
    name = "albums",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture_Albums,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the name of the album */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the pictures */
  def member_pictures = ApiMember(
    name = "pictures",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPictures,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "albums" -> member_albums,
    "name" -> member_name,
    "pictures" -> member_pictures
  )
            

}
          
