/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TPage_Button
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Page Button
 *
 * A page button on the wall
 *
 * @author Lucas Brutschy
 */

object TPage_Button extends Default_TPage_Button {

  /** Gets the text */
  lazy val field_text = ApiField("text", TString)

  /** Gets the page hosting this button */
  lazy val field_page = ApiField("page", TPage)

  /** Gets the icon name */
  lazy val field_icon = ApiField("icon", TString)

  override def possibleFields = super.possibleFields ++ List(field_text,field_page,field_icon)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets a value indicating if both instances are equal */
    // case "equals" =>
    //   val List(page_button) = parameters // Page_Button
    //   Top[S](TBoolean)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}