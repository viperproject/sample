/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.td.compiler.{TouchType, CFGGenerator, TouchCompiler}
import ch.ethz.inf.pm.td.libcontracts.LibraryContract
import ch.ethz.inf.pm.td.semantics.AAny


/**
 * Implements user-defined libraries
 *
 * User: Lucas
 * Date: 17.02.13
 * Time: 18:54
 */
object Dispatcher extends NativeMethodSemantics {

  /**
   * Backward semantics are empty for all native function for now
   */
  def applyBackwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                  parameters : List[ExpressionSet], typeparameters : List[Type],
                                                  returnedtype : Type, pp : ProgramPoint, state : S) : Option[S] = None

  /**
   * Delegates forward semantics to concrete classes.
   *
   * Checks if the object or any other
   */
  def applyForwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                 parameters : List[ExpressionSet], typeparameters : List[Type],
                                                 returnedtype : Type, pp : ProgramPoint, state : S) : Option[S] = {

    val typ = thisExpr.typ.asInstanceOf[AAny]
    if (CFGGenerator.isLibraryIdent(typ.name)) {
      LibraryContract.applyForwardNativeSemantics(thisExpr,operator,parameters,typeparameters,returnedtype,pp,state)
    } else {
      typ.applyForwardNativeSemantics[S](thisExpr,operator,parameters,Nil,returnedtype.asInstanceOf[TouchType],pp,state)
    }

  }

}
