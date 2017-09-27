/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.PermissionTree._
import ch.ethz.inf.pm.sample.util.SampleExpressions
import viper.silver.{ast => sil}

import scala.collection.mutable.ListBuffer

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
  def generate(records: PermissionRecords): Seq[sil.Exp] =
    records.fields.map { field =>
      // get tree and plug in zero as the initial permissions
      val tree = records(field).transform {
        case Initial => Empty
        case other => other
      }
      val expression = tree.simplify.toExpression

      val receiver = Context.getReceiver
      val variables = Context.getVariables(receiver.name)

      val quantified = ListBuffer[sil.LocalVarDecl]()
      val arguments = ListBuffer[sil.Exp]()

      val zipped = variables zip receiver.formalArgs
      val simplified = zipped.foldLeft(expression) {
        case (current, (variable, parameter)) =>
          // collect terms
          var terms = Option(Set.empty[Expression])
          current.foreach {
            case Equal(`variable`, term) =>
              terms match {
                case Some(set) => terms = Some(set + term)
                case None => // do nothing
              }
            case Comparison(left, right, _) if left.contains(_ == variable) || right.contains(_ == variable) =>
              terms = None
            case _ => // do nothing
          }

          terms match {
            case Some(set) if set.size == 1 =>
              val term = set.head
              // add argument
              val argument = convert(term)
              arguments.append(argument)
              // return
              val transformed = current.transform {
                case `variable` => term
                case other => other
              }
              simplify(transformed)
            case _ =>
              // add quantified variable
              val name = variable.name
              val typ = parameter.typ
              val declaration = sil.LocalVarDecl(name, typ)()
              quantified.append(declaration)
              // add argument
              val argument = sil.LocalVar(name)(typ)
              arguments.append(argument)
              // return
              current
          }
      }

      val application = sil.FuncLikeApp(receiver, arguments, Map.empty)
      val location = sil.FieldAccess(application, sil.Field(field.getName, convert(field.typ))())()
      val permission = convert(simplify(simplified))
      val body = sil.FieldAccessPredicate(location, permission)()

      if (quantified.isEmpty) body
      else {
        val triggers = Seq.empty
        sil.Forall(quantified, triggers, body)()
      }
    }

  private def convert(expression: Expression): sil.Exp =
    Converter.convert(expression)

  private def convert(typ: Type): sil.Type =
    Converter.convert(typ)
}

object Converter
  extends DefaultSampleConverter {

  override def convert(expression: Expression): sil.Exp = expression match {
    case Max(left, right) => max(convert(left), convert(right))
    case FunctionCallExpression(name, parameters, typ, pp) =>
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
