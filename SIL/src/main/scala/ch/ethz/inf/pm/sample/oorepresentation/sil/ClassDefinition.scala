/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.{oorepresentation => rep}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.State

/**
 * Method declaration that supports out-parameters.
 * The only difference is that it uses `createVariable` instead of
 * `createVariableForArgument` for the out-parameters.
 *
 * @todo Integrate it into the official Sample base class.
 */
class MethodDeclWithOutParams(
    override val programpoint: rep.ProgramPoint,
    override val ownerType: rep.Type,
    override val modifiers: List[rep.Modifier],
    override val name: MethodIdentifier,
    override val parametricType: List[Type],
    override val arguments: List[List[VariableDeclaration]],
    override val returnType: Type,
    override val body: ControlFlowGraph,
    override val precond: Statement,
    override val postcond: Statement,
    override val classDef: ClassDefinition)
  extends sample.MethodDeclaration(
    programpoint,
    ownerType,
    modifiers,
    name,
    parametricType,
    arguments,
    returnType,
    body,
    precond,
    postcond,
    classDef) {

  override def initializeArgument[S <: State[S]](state: S): S = {
    var result = state
    // Create a variable for each formal in-parameter
    for (variable <- arguments.head) {
      result = variable.variable.forwardSemantics[S](result)
      val varExpr = result.expr
      result = result.removeExpression()
      result = result.createVariableForArgument(varExpr, variable.typ)
    }
    // Create a variable for each formal out-parameter
    for (variable <- arguments(1)) {
      result = variable.variable.forwardSemantics[S](result)
      val varExpr = result.expr
      result = result.removeExpression()
      result = result.createVariable(varExpr, variable.typ, programpoint)
    }

    // Assume the precondition
    result = precond.forwardSemantics(result).testTrue()
    result
  }
}