/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TPicture_Album
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Picture Album
 *
 * A picture album
 *
 * @author Lucas Brutschy
 */ 

object TPicture_Album extends Default_TPicture_Album {

  /** Gets the children albums */
  lazy val field_albums = ApiField("albums", TPicture_Albums)

  /** Gets the name of the album */
  lazy val field_name = ApiField("name", TString)

  /** Gets the pictures */
  lazy val field_pictures = ApiField("pictures", TPictures)

  override def possibleFields = super.possibleFields ++ List(field_albums, field_name, field_pictures)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
