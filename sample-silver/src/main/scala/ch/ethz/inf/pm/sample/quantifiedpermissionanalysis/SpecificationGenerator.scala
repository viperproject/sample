/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, FunctionCallExpression}
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Permission.{Fractional, Read}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.PermissionTree._
import viper.silver.{ast => sil}

/**
  * An object providing methods to generate specifications for the quantified
  * permission inference.
  *
  * @author Jerome Dohrau
  */
object SpecificationGenerator {
  /**
    * Generates the specifications corresponding to the given permission
    * records.
    *
    * @param records The permission records.
    * @return The generated specifications.
    */
  def generate(records: PermissionRecords): Seq[sil.Exp] = {
    records.fields.map { field =>
      // get tree and plug in zero as the initial permissions
      val tree = records(field).transform {
        case Initial => Empty
        case tree => tree
      }

      val quantified = sil.LocalVar("q")(sil.Ref)
      val permission = convert(tree, quantified)
      val location = sil.FieldAccess(quantified, sil.Field(field.getName, convert(field.typ))())()

      val variables = Seq(sil.LocalVarDecl("q", sil.Ref)())
      val triggers = Seq.empty
      val expression = sil.FieldAccessPredicate(location, permission)()
      sil.Forall(variables, triggers, expression)()
    }
  }

  private def convert(tree: PermissionTree, quantified: sil.Exp): sil.Exp = tree.simplify match {
    case Empty => convert(Fractional(0, 1))
    case Leaf(receiver, permission) =>
      val condition = receiver match {
        case FunctionCallExpression(name, arguments, _, _) =>
          val function = getFunction(name)
          val converted = arguments.map(convert)
          val application = sil.FuncLikeApp(function, converted, Map.empty[sil.TypeVar, sil.Type])
          sil.EqCmp(application, quantified)()
        case expression =>
          val converted = convert(expression)
          sil.EqCmp(converted, quantified)()
      }
      val leftExpression = convert(permission)
      val rightExpression = convert(Fractional(0, 1))
      conditional(condition, leftExpression, rightExpression)
    case Addition(left, right) =>
      val leftExpression = convert(left, quantified)
      val rightExpression = convert(right, quantified)
      addition(leftExpression, rightExpression)
    case Subtraction(left, right) =>
      val leftExpression = convert(left, quantified)
      val rightExpression = convert(right, quantified)
      val zero = sil.IntLit(0)()
      max(subtraction(leftExpression, rightExpression), zero)
    case Maximum(left, right) =>
      val leftExpression = convert(left, quantified)
      val rightExpression = convert(right, quantified)
      max(leftExpression, rightExpression)
    case Conditional(condition, left, right) =>
      val conditionExpression = convert(condition)
      val leftExpression = convert(left, quantified)
      val rightExpression = convert(right, quantified)
      conditional(conditionExpression, leftExpression, rightExpression)
  }

  private def convert(permission: Permission): sil.Exp = permission match {
    case Read =>
      // TODO: Replace with read permission.
      convert(Fractional(1, 100))
    case Fractional(numerator, denominator) =>
      if (numerator == 0) sil.NoPerm()()
      else if (numerator == denominator) sil.FullPerm()()
      else {
        val left = sil.IntLit(numerator)()
        val right = sil.IntLit(denominator)()
        sil.FractionalPerm(left, right)()
      }
  }

  private def convert(expression: Expression): sil.Exp =
    DefaultSampleConverter.convert(expression)

  private def convert(typ: Type): sil.Type =
    DefaultSampleConverter.convert(typ)

  private def addition(left: sil.Exp, right: sil.Exp): sil.Exp =
    sil.Add(left, right)()

  private def subtraction(left: sil.Exp, right: sil.Exp): sil.Exp = {
    val zero = sil.IntLit(0)()
    val difference = sil.Sub(left, right)()
    max(zero, difference)
  }

  private def max(left: sil.Exp, right: sil.Exp): sil.Exp = {
    val function = Context.getMaxFunction
    val arguments = Seq(left, right)
    sil.FuncApp(function, arguments)()
  }

  private def conditional(condition: sil.Exp, left: sil.Exp, right: sil.Exp): sil.Exp =
    sil.CondExp(condition, left, right)()

  private def getFunction(name: String): sil.FuncLike =
    Context.getFunction(name)
}