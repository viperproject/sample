/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis.util

import viper.silver.{ast => sil}

/**
  * An object that provides helper methods to generate a silver AST.
  *
  * @author Jerome Dohrau
  */
object ExpressionGenerator {
  /**
    * Computes and returns an expression that is semantically equivalent to the
    * conjunction of the two given expressions.
    *
    * @param first  The first expression.
    * @param second The second expression.
    * @return The expression equivalent to the conjunction.
    */
  def and(first: sil.Exp, second: sil.Exp): sil.Exp = (first, second) match {
    case (sil.TrueLit(), _) => second
    case (_, sil.TrueLit()) => first
    case (sil.FalseLit(), _) => sil.FalseLit()()
    case (_, sil.FalseLit()) => sil.FalseLit()()
    case _ =>
      if (equivalent(first, second)) first
      else sil.And(first, second)()
  }

  /**
    * Computes and returns an expression that is semantically equivalent to the
    * conjunction of the given expressions.
    *
    * @param expressions The expressions.
    * @return The expression equivalent to the conjunction.
    */
  def and(expressions: Iterable[sil.Exp]): sil.Exp =
    if (expressions.isEmpty) sil.TrueLit()()
    else expressions.reduce(and)


  /**
    * Computes and returns an expression that is semantically equivalent to the
    * disjunction of the two given expressions.
    *
    * @param first  The first expression.
    * @param second The second expression.
    * @return The expression equivalent to the disjunction.
    */
  def or(first: sil.Exp, second: sil.Exp): sil.Exp = (first, second) match {
    case (sil.TrueLit(), _) => sil.TrueLit()()
    case (_, sil.TrueLit()) => sil.TrueLit()()
    case (sil.FalseLit(), _) => second
    case (_, sil.FalseLit()) => first
    case _ =>
      if (equivalent(first, second)) first
      else sil.Or(first, second)()
  }

  /**
    * Computes and returns an expression that is semantically equivalent to the
    * disjunction of the two given expressions.
    *
    * @param expressions The expressions.
    * @return The expression equivalent to the disjunction.
    */
  def or(expressions: Iterable[sil.Exp]): sil.Exp =
    if (expressions.isEmpty) sil.FalseLit()()
    else expressions.reduce(or)

  /**
    * Computes and returns an expression that is semantically equivalent to the
    * negation of the given expression.
    *
    * @param expression The expression.
    * @return The expression equivalent to the negation.
    */
  def not(expression: sil.Exp): sil.Exp = expression match {
    case sil.TrueLit() => sil.FalseLit()()
    case sil.FalseLit() => sil.TrueLit()()
    case sil.Not(negated) => negated
    case sil.EqCmp(l, r) => sil.NeCmp(l, r)()
    case sil.NeCmp(l, r) => sil.EqCmp(l, r)()
    case _ => sil.Not(expression)()
  }

  /**
    * Computes and returns an expression that is semantically equivalent to the
    * equality of the given expressions.
    *
    * @param first  The first expression.
    * @param second The second expression.
    * @return The expression equivalent to the equality.
    */
  def equal(first: sil.Exp, second: sil.Exp): sil.Exp =
    if (equivalent(first, second)) sil.TrueLit()()
    else sil.EqCmp(first, second)()

  /**
    * Computes and returns an expression that is semantically equivalent to the
    * inequality of the given expressions.
    *
    * @param first  The first expression.
    * @param second The second expression.
    * @return The expression equivalent to the inequality.
    */
  def notEqual(first: sil.Exp, second: sil.Exp): sil.Exp =
    if (equivalent(first, second)) sil.FalseLit()()
    else sil.NeCmp(first, second)()

  def field(receiver: sil.Exp, name: String, typ: sil.Type): sil.FieldAccess =
    sil.FieldAccess(receiver, sil.Field(name, typ)())()

  def access(name: String, typ: sil.Type): sil.LocalVar = {
    sil.LocalVar(name)(typ)
  }

  def access(path: Iterable[String], typ: sil.Type): sil.Exp =
    if (path.isEmpty) throw new IllegalArgumentException("Path must be non-empty.")
    else if (path.size == 1) access(path.last, typ)
    else sil.FieldAccess(access(path.init, sil.Ref), sil.Field(path.last, typ)())()

  def old(expression: sil.Exp): sil.Exp =
    sil.Old(expression)()

  /**
    * Returns true if the two given expressions are equivalent.
    *
    * @param first  The first expression.
    * @param second The second expression.
    * @return True if the expressions are equivalent.
    */
  private def equivalent(first: sil.Exp, second: sil.Exp): Boolean = (first, second) match {
    case (sil.And(l1, r1), sil.And(l2, r2)) => equivalent(l1, r1) && equivalent(l2, r2)
    case (sil.Or(l1, r1), sil.Or(l2, r2)) => equivalent(l1, r1) && equivalent(l2, r2)
    case (sil.Not(a1), sil.Not(a2)) => equivalent(a1, a2)
    case (sil.LocalVar(n1), sil.LocalVar(n2)) => n1 == n2
    case (sil.Old(e1), sil.Old(e2)) => equivalent(e1, e2)
    case _ => false
  }
}


