/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, FunctionCallExpression}
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.PermissionTree._
import ch.ethz.inf.pm.sample.util.SampleExpressions
import viper.silver.{ast => sil}

/**
  * An object providing methods to generate specifications for the quantified
  * permission inference.
  *
  * @author Jerome Dohrau
  */
object SpecificationGenerator {

  import SampleExpressions._

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
        case other => other
      }

      val receiver = Context.getReceiver
      val quantified = Context.getQuantified(receiver.name)

      val variables = for ((x, y) <- quantified zip receiver.formalArgs)
        yield sil.LocalVarDecl(x.name, y.typ)()
      val arguments = variables.map { variable => sil.LocalVar(variable.name)(variable.typ) }

      val permission = convert(tree)
      val application = sil.FuncLikeApp(receiver, arguments, Map.empty)
      val location = sil.FieldAccess(application, sil.Field(field.getName, convert(field.typ))())()

      val triggers = Seq.empty
      val body = sil.FieldAccessPredicate(location, permission)()

      sil.Forall(variables, triggers, body)()
    }
  }

  /**
    * @param tree
    * @return
    */
  private def convert(tree: PermissionTree): sil.Exp = tree.simplify match {
    case Empty => convert(No)
    case Leaf(condition, permission) =>
      val conditionExpression = convert(condition)
      val leftExpression = convert(permission)
      val rightExpression = convert(No)
      conditional(conditionExpression, leftExpression, rightExpression)
    case Addition(left, right) =>
      val leftExpression = convert(left)
      val rightExpression = convert(right)
      addition(leftExpression, rightExpression)
    case Subtraction(left, right) =>
      val leftExpression = convert(left)
      val rightExpression = convert(right)
      val zero = sil.IntLit(0)()
      max(subtraction(leftExpression, rightExpression), zero)
    case Maximum(left, right) =>
      val leftExpression = convert(left)
      val rightExpression = convert(right)
      max(leftExpression, rightExpression)
    case Conditional(condition, left, right) =>
      val conditionExpression = convert(condition)
      val leftExpression = convert(left)
      val rightExpression = convert(right)
      conditional(conditionExpression, leftExpression, rightExpression)
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
