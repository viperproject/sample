/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TPlace
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Place
 *
 * A named location
 *
 * @author Lucas Brutschy
 */ 

object TPlace extends Default_TPlace {

  /** Gets the category of the place */
  lazy val field_category = ApiField("category", TString)

  /** Gets the link associated to the message */
  lazy val field_link = ApiField("link", TString)

  /** Gets the location of the place */
  lazy val field_location = ApiField("location", TLocation)

  /** Gets the name of the place */
  lazy val field_name = ApiField("name", TString)

  /** Gets a url to the picture */
  lazy val field_picture_link = ApiField("picture link", TString)

  /** Gets the source of this place (facebook, touchdevelop) */
  lazy val field_source = ApiField("source", TString)

  override def possibleFields = super.possibleFields ++ List(field_category, field_link, field_location,
    field_name, field_picture_link, field_source)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Checks into the place (supported for Facebook) */
    case "check in" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
