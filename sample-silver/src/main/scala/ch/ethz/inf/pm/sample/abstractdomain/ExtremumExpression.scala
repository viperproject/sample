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
  * An expression representing an extremum, i.e., either a minimum or maximum.
  *
  * @author Jerome Dohrau
  */
trait ExtremumExpression
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

case class BigMin(variables: Seq[VariableIdentifier], body: Expression)
  extends ExtremumExpression {

  override def transform(f: (Expression) => Expression): Expression =
    f(BigMin(variables, body.transform(f)))

  override def toString: String = s"min ${variables.mkString(",")}:: $body"
}

object BigMin {
  def apply(variable: VariableIdentifier, body: Expression): BigMin =
    BigMin(Seq(variable), body)
}

case class BigMax(variables: Seq[VariableIdentifier], body: Expression)
  extends ExtremumExpression {

  override def transform(f: (Expression) => Expression): Expression =
    f(BigMax(variables, body.transform(f)))

  override def toString: String = s"max ${variables.mkString(",")}:: $body"
}

object BigMax {
  def apply(variable: VariableIdentifier, body: Expression): BigMax =
    BigMax(Seq(variable), body)
}

case class Min(left: Expression, right: Expression)
  extends BinaryExpression {

  override def typ: Type = left.typ lub right.typ

  override def transform(f: (Expression) => Expression): Expression =
    f(Min(left.transform(f), right.transform(f)))

  override def toString: String = s"min($left, $right)"
}

object MinList {
  def apply(expressions: Iterable[Expression]): Expression =
    expressions.reduce((left, right) => Min(left, right))

  def unapply(argument: Expression): Option[List[Expression]] = argument match {
    case Min(MinList(left), MinList(right)) => Some(left ++ right)
    case _ if argument.typ.isNumericalType => Some(List(argument))
    case _ => None
  }
}

case class Max(left: Expression, right: Expression)
  extends BinaryExpression {

  override def typ: Type = left.typ lub right.typ

  override def transform(f: (Expression) => Expression): Expression =
    f(Max(left.transform(f), right.transform(f)))

  override def toString: String = s"max($left, $right)"
}

object MaxList {
  def apply(expressions: Iterable[Expression]): Expression =
    expressions.reduce((left, right) => Max(left, right))

  def unapply(argument: Expression): Option[List[Expression]] = argument match {
    case Max(MaxList(left), MaxList(right)) => Some(left ++ right)
    case _ if argument.typ.isNumericalType => Some(List(argument))
    case _ => None
  }
}