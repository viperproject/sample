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
import ch.ethz.inf.pm.td.defsemantics.Default_TNumber_Map
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Number Map
 *
 * A map of numbers to numbers
 *
 * TODO: These implementations are only valid for (size,elem) abstractions
 *
 * @author Lucas Brutschy
 */

object TNumber_Map extends Default_TNumber_Map {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {


    /** === NUMBER MAPS RETURN 0 FOR UNITIALIZED FIELDS! === */
    case "at" =>
      val List(key) = parameters // Key_Type
      If[S](ContainsKey[S](this0, key), Then={
        Return[S](At[S](this0, key))(_, pp)
      }, Else={
        Return[S](0)(_, pp)
      })

    /** Computes the average of the values */
    case "avg" =>
      Return[S](AllValues[S](this0))

    /** Computes the maximum of the values */
    case "max" =>
      Return[S](AllValues[S](this0))

    /** Computes the minimum of the values */
    case "min" =>
      Return[S](AllValues[S](this0))

    /** Extracts the elements at indices between start (inclusive) and end (non-inclusive). */
    case "slice" =>
      val List(start,end) = parameters // Number,Number
      Return[S](this0)
      //Clone[S](this0)

    /** Computes the sum of the values */
    case "sum" =>
      Return[S]( Count[S](this0) * AllValues[S](this0))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}

