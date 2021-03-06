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
import ch.ethz.inf.pm.td.defsemantics.Default_TPictures
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Pictures
 *
 * A collection of pictures
 *
 * @author Lucas Brutschy
 */ 

object TPictures extends Default_TPictures {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Finds a picture by name and returns the index. Returns -1 if not found. */
    case "find" =>
      val List(name) = parameters // String
      Return[S](-1 ndToIncl (Count[S](this0) - 1))

    /** Gets the thumbnail of i-th picture. */
    case "thumbnail" =>
      val List(index) = parameters // Number
      New[S](TPicture)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
