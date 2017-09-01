/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, FunctionCallExpression, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Permission.{Read, Write, Zero}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.PermissionTree.{Empty, Initial, Leaf, Maximum}
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

  private def convert(tree: PermissionTree, quantified: sil.Exp): sil.Exp = tree match {
    case Empty => convert(Zero)
    case Leaf(receiver, permission) =>
      val condition = convert(receiver, quantified)
      val leftExpression = convert(permission)
      val rightExpression = convert(Zero)
      conditional(condition, leftExpression, rightExpression)
    case Maximum(left, right) =>
      val leftExpression = convert(left, quantified)
      val rightExpression = convert(right, quantified)
      max(leftExpression, rightExpression)
    case _ => ???
  }

  private def convert(receiver: Receiver, quantified: sil.Exp): sil.Exp = receiver.expression match {
    case variable: VariableIdentifier =>
      val converted = convert(variable)
      sil.EqCmp(converted, quantified)()
    case FunctionCallExpression(name, arguments, _, _) =>
      val function = getFunction(name)
      val converted = arguments.map(convert)
      val application = sil.FuncApp(function, converted)()
      sil.EqCmp(application, quantified)()
  }

  private def convert(permission: Permission): sil.Exp = permission match {
    case Zero => sil.NoPerm()()
    case Read =>
      // TODO: Replace with read permission.
      sil.FullPerm()()
    case Write => sil.FullPerm()()
  }

  private def convert(expression: Expression): sil.Exp =
    DefaultSampleConverter.convert(expression)

  private def convert(typ: Type): sil.Type =
    DefaultSampleConverter.convert(typ)

  private def max(left: sil.Exp, right: sil.Exp): sil.Exp = {
    val function = Context.getMaxFunction
    val arguments = Seq(left, right)
    sil.FuncApp(function, arguments)()
  }

  private def conditional(condition: sil.Exp, left: sil.Exp, right: sil.Exp): sil.Exp =
    sil.CondExp(condition, left, right)()

  private def getFunction(name: String): sil.Function = {
    // TODO: Get the actual function from the context.
    val parameters = Seq.empty
    val typ = sil.Ref
    val preconditions = Seq.empty
    val postconditions = Seq.empty
    val body = None
    sil.Function(name, parameters, typ, preconditions, postconditions, body)()
  }
}
