
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_TUser
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of User
 *
 * A user account
 *
 * @author Lucas Brutschy
 */ 

object TUser extends Default_TUser {

  /** Gets the about-me text of the user */
  lazy val field_about = ApiField("about", TString)

  /** Indicates if the user has a picture */
  lazy val field_has_picture = ApiField("has picture", TBoolean)

  /** Gets the id */
  lazy val field_id = ApiField("id", TString)

  /** Gets the name of the user */
  lazy val field_name = ApiField("name", TString)

  /** Gets the url of the user picture where original is the unmodified user picture, square is 50x50, small has 50px
    * width, normal has 100px width, large has roughly 200px width */
  lazy val field_picture_address = ApiField("picture address", TString)

  /** Gets the user picture where original is the unmodified user picture, square is 50x50, small has 50px width,
    * normal has 100px width, large has roughly 200px width */
  lazy val field_picture = ApiField("picture", TPicture)

  override def possibleFields = super.possibleFields ++ List(field_about, field_has_picture, field_id,
    field_name, field_picture_address, field_picture)


  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet],
                                               returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
