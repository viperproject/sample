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
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_SCloud_Storage
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Cloud Storage
 *
 * OneDrive, OneNote operations
 *
 * @author Lucas Brutschy
 */ 

object SCloud_Storage extends Default_SCloud_Storage {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Uploads a new OneNote page and returns the web client url if successful. The 'Presentation' field must contain the well-formed HTML. Additional pictures can be stored in other fields. */
    case "upload note" =>
    //   val List(form) = parameters // Form_Builder
      TopWithInvalid[S](TString,"upload may fail")

    /** Prompts the user to upload a picture to OneDrive. If the filename is empty, a default filename gets generated. */
    case "upload picture" =>
    //   val List(pic,folder,filename) = parameters // Picture,String,String
      TopWithInvalid[S](TCloud_Picture,"upload may fail")

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
