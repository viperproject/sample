/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.libcontracts

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{Lattice, VariableIdentifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{MethodDeclaration, ProgramPoint, Type, ForwardNativeMethodSemantics}
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, RichNativeSemantics, MethodSummaries}
import ch.ethz.inf.pm.td.compiler.{TouchCompiler, CFGGenerator, TouchType}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics.SData

object LibraryContract extends ForwardNativeMethodSemantics {

  /**
   * A list of all defined contracts
   */
  def contracts = List(
    GameLib,
    GamepadLib,
    TurtleLib
  )

  /**
   * Dispatches to the corresponding libraries
   */
  override def applyForwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet, operator: String,
                                                          parameters: List[ExpressionSet], typeparameters: List[Type],
                                                          returnedtype: Type, programpoint: ProgramPoint, state: S) = {

    if (TouchAnalysisParameters.get.useLibraryContracts) {

      val results = (for (c <- contracts) yield {
        c.applyForwardNativeSemantics(thisExpr,operator,parameters,typeparameters,returnedtype,programpoint,state)
      }).flatten

      results match {
        case Nil =>
          Some(analyzeImplementation[S](thisExpr,operator,parameters,returnedtype.asInstanceOf[TouchType])(programpoint,state))
        case List(x) =>
          Some(x)
        case xs =>
          Some(Lattice.bigLub(xs))
      }

    } else {

      Some(analyzeImplementation[S](thisExpr,operator,parameters,returnedtype.asInstanceOf[TouchType])(programpoint,state))

    }
  }

  /**
   * Implements forward semantics of a library
   * Here, we look at the actual implementation of the library
   */
  def analyzeImplementation[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                     (implicit pp: ProgramPoint, state: S): S = {

    val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]
    compiler.getMethodWithClassDefinition(method,this0.getType(),parameters map (_.getType())) match {
      case Some(mdecl: MethodDeclaration) =>

        // We may access a library function
        MethodSummaries.collect(pp, mdecl, state, parameters)

      case _ =>

        // Curiously, this is also used to access global data from libraries.
        if(parameters.isEmpty && returnedType.toString != "Nothing") {
          SData.forwardSemantics[S](SData.reference,method,parameters,returnedType)
        } else if (parameters.size == 1 && returnedType.toString == "Nothing" && method.startsWith("set ")) {
          // And we have also automatically generated setters. What the...
          SData.forwardSemantics[S](SData.reference,method,parameters,returnedType)
        } else {
          RichNativeSemantics.Unimplemented[S](method, returnedType)
        }

    }
  }

}

trait LibraryContract extends ForwardNativeMethodSemantics {

  def name:String

  /**
   * Delegates forward semantics to concrete classes.
   *
   * Checks if the object or any other
   */
  def applyForwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet, operator: String,
                                                 parameters: List[ExpressionSet], typeparameters: List[Type],
                                                 returnedtype: Type, pp: ProgramPoint, state: S): Option[S] = {

    if (thisExpr.getType().asInstanceOf[TouchType].typeName == TypeName("♻"+name)) {

      Some(forwardSemantics(thisExpr, operator, parameters, returnedtype.asInstanceOf[TouchType])(pp, state))

    } else None

  }

  /**
   * Implements forward semantics of a library. Overwrite this with custom contracts
   */
  def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                     (implicit pp: ProgramPoint, state: S): S = {

    LibraryContract.analyzeImplementation[S](this0,method,parameters,returnedType)(pp,state)

  }

}
