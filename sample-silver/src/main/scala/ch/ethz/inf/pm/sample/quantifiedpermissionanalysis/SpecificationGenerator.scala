/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.elimination.MaximumElimination
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, IntType, PermType}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.PermissionTree._
import ch.ethz.inf.pm.sample.util.SampleExpressions
import viper.silver.{ast => sil}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * An object providing methods to generate specifications for the quantified
  * permission inference.
  *
  * @author Jerome Dohrau
  */
object SpecificationGenerator {

  import SampleExpressions._

  def slice(variable: VariableIdentifier, expression: Expression): Set[(Expression, Expression)] = {
    var values = Set.empty[Expression]
    expression.foreach {
      case Equal(`variable`, value) => values = values + value
      case Comparison(left, right, _) if left.contains(_ == variable) || right.contains(_ == variable) => ??? // should not occur
      case _ => // do nothing
    }
    for (value <- values) yield {
      val transformed = expression.transform {
        case Equal(`variable`, term) => Literal(term == value)
        case other => other
      }
      (value, simplify(transformed))
    }
  }

  /**
    * Generates the specifications corresponding to the given permission
    * records.
    *
    * @param records The permission records.
    * @return The generated specifications.
    */
  def generate(records: PermissionRecords): Seq[sil.Exp] =
    records.fields.flatMap { field =>
      // get tree and plug in zero as the initial permissions
      val tree = records(field).transform {
        case Initial => Empty
        case other => other
      }

      val rewritten = for ((cond, exp) <- tree.simplify.rewrite) yield ConditionalExpression(cond, exp, No)
      val expression = MaxList(rewritten)

      Context.getReceiver match {
        case None =>
          Seq.empty
        case Some(receiver) =>
          val variables = Context.getVariables(receiver.name)


          val blubb = variables.foldRight(Set((List.empty[Expression], expression))) {
            case (variable, set) =>
              if (variable.typ == IntType) {
                set.map { case (arguments, current) => (variable :: arguments, current)}
              } else {
                // do some slicing
                set.flatMap { case (arguments, current) =>
                  val sliced = slice(variable, current)
                  sliced.map { case (argument, updated) => (argument :: arguments, updated) }
                }
              }
          }

          val simplified = blubb.map { case (arguments, updated) =>
            arguments.foldRight((List.empty[Expression], updated)) { case (argument, (list, current)) =>
              if (variables.contains(argument)) {
                // collect terms
                var collected = Option(Set.empty[Expression])
                current.foreach {
                  case Equal(`argument`, term) =>
                    collected match {
                      case Some(terms) => collected = Some(terms + term)
                      case None => // do nothing
                    }
                  case Comparison(left, right, _) if left.contains(_ == argument) || right.contains(_ == argument) =>
                    collected = None
                  case _ => // do nothing
                }
                collected match {
                  case Some(terms) if terms.size == 1 =>
                    val term = terms.head
                    val transformed = current.transform {
                      case `argument` => term
                      case other => other
                    }
                    val simplified = simplify(transformed)
                    (term :: list, simplified)
                  case _ => (argument :: list, current)
                }
              }else (argument :: list, current)
            }
          }

          for ((arguments, updated) <- simplified) yield {
            val application = sil.FuncLikeApp(receiver, arguments.map(convert), Map.empty)
            val location = sil.FieldAccess(application, sil.Field(field.getName, convert(field.typ))())()

            val prettified = simplify(pretty(simplify(updated), variables))
            val permission = convert(prettified)

            val quantified = arguments.filter(variables.contains(_)).map { variable => sil.LocalVarDecl(variable.toString, sil.Int)() }
            val body = sil.FieldAccessPredicate(location, permission)()
            if (quantified.isEmpty) body
            else sil.Forall(quantified, Seq.empty, body)()
          }
      }
    }

  private def frame(expression: Expression): (Expression, Seq[Expression]) = {
    val constraints = mutable.ListBuffer[Expression]()

    val transformed = expression.transform {
      case access@FieldAccessExpression(receiver, field) => receiver match {
        case FunctionCallExpression(name, arguments, _) =>
          val name2 = Context.getUninterpreted(name, field.name)
          val uninterpreted = FunctionCallExpression(name2, arguments, field.typ)()
          val constraint = Equal(access, uninterpreted)
          constraints.append(constraint)
          uninterpreted
      }
      case other => other
    }

    (transformed, constraints)
  }

  private def convert(expression: Expression): sil.Exp =
    Converter.convert(expression)

  private def convert(typ: Type): sil.Type =
    Converter.convert(typ)
}

object Converter
  extends DefaultSampleConverter {

  override def convert(expression: Expression): sil.Exp = expression match {
    case BoundedPermissionExpression(permission) => max(convert(permission), sil.NoPerm()())
    case Max(left, right) => max(convert(left), convert(right))
    case FunctionCallExpression(name, parameters, typ) =>
      val function = Context.getFunction(name)
      sil.FuncLikeApp(function, parameters.map(convert), Map.empty[sil.TypeVar, sil.Type])
    case _ => super.convert(expression)
  }

  private def max(left: sil.Exp, right: sil.Exp): sil.Exp = {
    val function = Context.getMaxFunction
    val arguments = Seq(left, right)
    sil.FuncApp(function, arguments)()
  }
}
