/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}

/**
  * A quantified expression.
  *
  * @author Jerome Dohrau
  */
trait QuantifiedExpression
  extends Expression {

  /**
    * Returns the bound variables.
    *
    * @return The bound variables.
    */
  def variables: Seq[VariableIdentifier]

  /**
    * Returns the body in which the variables are bound.
    *
    * @return The body.
    */
  def body: Expression

  override def ids: IdentifierSet = body.ids -- variables.toSet[Identifier]

  override def pp: ProgramPoint = variables
    .headOption.map(_.pp)
    .getOrElse(body.pp)

  override def typ: Type = body.typ

  override def contains(f: (Expression) => Boolean): Boolean =
    f(this) || body.contains(f)
}

/**
  * Represents an existentially quantified expression.
  *
  * @param variables The quantified variables.
  * @param body      The body of the quantifier.
  * @author Jerome Dohrau
  */
case class Exists(variables: Seq[VariableIdentifier], body: Expression)
  extends QuantifiedExpression {

  override def transform(f: (Expression) => Expression): Expression =
    f(Exists(variables, body.transform(f)))

  override def toString: String = s"exists ${variables.mkString(", ")} :: $body"
}

object Exists {
  def apply(variable: VariableIdentifier, body: Expression): Exists =
    Exists(Seq(variable), body)
}

/**
  * Represents a universally quantified expression.
  *
  * @param variables The quantified variables.
  * @param body      The body of the quantifier.
  * @author Jerome Dohrau
  */
case class ForAll(variables: Seq[VariableIdentifier], body: Expression)
  extends QuantifiedExpression {

  override def transform(f: (Expression) => Expression): Expression =
    f(ForAll(variables, body.transform(f)))

  override def toString: String = s"forall ${variables.mkString(", ")} :: $body"
}

object ForAll {
  def apply(variable: VariableIdentifier, body: Expression): ForAll =
    ForAll(Seq(variable), body)
}
