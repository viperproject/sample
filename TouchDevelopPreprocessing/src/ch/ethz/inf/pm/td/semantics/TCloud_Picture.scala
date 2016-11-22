/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TCloud_Picture
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Cloud Picture
 *
 * A picture hosted on OneDrive.
 *
 * @author Lucas Brutschy
 */ 

object TCloud_Picture extends Default_TCloud_Picture {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Downloads the picture with a particular size. */
    case "download picture" =>
    //  val List(media) = parameters // String
      TopWithInvalid[S](TPicture,"download may fail")

    /** Gets the picture with a particular size. */
    case "to picture" =>
    //   val List(media) = parameters // String
       TopWithInvalid[S](TPicture,"conversion may fail")

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
