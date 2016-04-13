
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
import ch.ethz.inf.pm.td.defsemantics.Default_TBuffer
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Buffer
 *
 * Contains binary data
 *
 * @author Lucas Brutschy
 */

object TBuffer extends Default_TBuffer {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Copies all bytes from `source` to current buffer at `offset` */
    case "clone" =>
      // TODO CHECK RANGE
      Assign[S](AllValues[S](this0), Top[S](TNumber).expr)

    /** Copies all bytes from `source` to current buffer at `offset` */
    case "copy from" =>
      val List(target_offset, source) = parameters // Number,Buffer
      // TODO CHECK RANGE
      Assign[S](AllValues[S](this0), Top[S](TNumber).expr)

    /** Fills the buffer with random values */
    case "fill random" =>
      val List() = parameters //
      Assign[S](AllValues[S](this0), Top[S](TNumber).expr)

    /** Sets all bytes in buffer to `value` */
    case "fill" =>
      val List(value) = parameters // Number
      Assign[S](AllValues[S](this0), value)

    /** Set byte at `index` to `value` */
    case "set" =>
      val List(index, value) = parameters // Number,Number
      If[S](IndexInRange[S](this0, index), Then = (state) => {
        val newState = RemoveAt[S](this0, index)(state, pp)
        Insert[S](this0, index, value)(newState, pp)
      }, Else = (state) => {
        state
      })

    /** Creates a read-write view of the current buffer. */
    case "sub buffer" =>
      val List(start, length) = parameters // Number,Number
      // TODO CHECK RANGE
      Top[S](TBuffer)

    /** Convert the buffer to a string */
    case "to string" =>
      val List(encoding) = parameters // String
      Top[S](TString)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
