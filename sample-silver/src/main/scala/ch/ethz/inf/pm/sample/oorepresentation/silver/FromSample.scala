/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.util.SampleExpressions
import viper.silver.ast.LocationAccess
import viper.silver.{ast => sil}

trait SampleConverter {
  /** Converts a Sample expression to a SIL expression. */
  def convert(e: sample.Expression): sil.Exp

  /** Converts a Sample program point to a SIL position. */
  def convert(pp: sample.ProgramPoint): sil.Position

  /** Converts a Sample type to a SIL type. */
  def convert(typ: sample.Type): sil.Type
}

trait DefaultSampleConverter
  extends SampleConverter {

  import SampleExpressions._

  override def convert(expression: Expression): sil.Exp = expression match {
    // boolean expressions
    case True => sil.TrueLit()(convert(expression.pp))
    case False => sil.FalseLit()(convert(expression.pp))
    case Not(argument) => sil.Not(convert(argument))()
    case And(left, right) => sil.And(convert(left), convert(right))()
    case Or(left, right) => sil.Or(convert(left), convert(right))()
    // arithmetic constants
    case Literal(value: Int) => sil.IntLit(value)(convert(expression.pp))
    case No => sil.NoPerm()(convert(expression.pp))
    case Full => sil.FullPerm()(convert(expression.pp))
    case FractionalPermissionExpression(left, right) =>
      sil.FractionalPerm(convert(left), convert(right))()
    // unary arithmetic operations
    case sample.UnaryArithmeticExpression(inner, op, typ) => op match {
      case ArithmeticOperator.`+` => convert(inner)
      case ArithmeticOperator.`-` => sil.Minus(convert(inner))()
    }
    // binary arithmetic operations
    case Operation(left, right, operator) => operator match {
      case ArithmeticOperator.+ => sil.Add(convert(left), convert(right))()
      case ArithmeticOperator.- => sil.Sub(convert(left), convert(right))()
      case ArithmeticOperator.* => sil.Mul(convert(left), convert(right))()
      case ArithmeticOperator./ => sil.Div(convert(left), convert(right))()
      case ArithmeticOperator.% => sil.Mod(convert(left), convert(right))()
    }
    // binary arithmetic comparisons
    case Comparison(left, right, operator) => operator match {
      case ArithmeticOperator.== => sil.EqCmp(convert(left), convert(right))()
      case ArithmeticOperator.!= => sil.NeCmp(convert(left), convert(right))()
      case ArithmeticOperator.< => sil.LtCmp(convert(left), convert(right))()
      case ArithmeticOperator.<= => sil.LeCmp(convert(left), convert(right))()
      case ArithmeticOperator.> => sil.GtCmp(convert(left), convert(right))()
      case ArithmeticOperator.>= => sil.GeCmp(convert(left), convert(right))()
    }
    // reference expressions
    // TODO: Null
    case sample.ReferenceComparisonExpression(left, right, op) => op match {
      case ReferenceOperator.`==` => sil.EqCmp(convert(left), convert(right))()
      case ReferenceOperator.`!=` => sil.NeCmp(convert(left), convert(right))()
    }
    // variables
    case VariableIdentifier(name, _) => name match {
      case Constants.ResultVariableName =>
        sil.Result()(convert(expression.typ), convert(expression.pp))
      case _ =>
        sil.LocalVar(name)(convert(expression.typ), convert(expression.pp))
    }
    // fields
    case AccessPathIdentifier(path) =>
      val variable = convert(path.head)
      path.tail.foldLeft(variable) { case (receiver, identifier) =>
        val field = sil.Field(identifier.getName, convert(identifier.typ))()
        sil.FieldAccess(receiver, field)()
      }
    case FieldAccessExpression(receiver, identifier) =>
      val field = sil.Field(identifier.name, convert(identifier.typ))()
      sil.FieldAccess(convert(receiver), field)()
    // other expressions
    case ConditionalExpression(condition, left, right) =>
      sil.CondExp(convert(condition), convert(left), convert(right))()
    case CurrentPermission(location, _) =>
      val loc = convert(location) match {
        case access: LocationAccess => access
        case _ => ???
      }
      sil.CurrentPerm(loc)()
    case _ => ???
  }

  override def convert(pp: ProgramPoint): sil.Position = pp match {
    case DummyProgramPoint => sil.NoPosition
    case sample.WrappedProgramPoint(pos) => pos.asInstanceOf[sil.SourcePosition]
  }

  override def convert(typ: Type): sil.Type = typ match {
    case IntType => sil.Int
    case BoolType => sil.Bool
    case RefType(_) => sil.Ref
    case PermType => sil.Perm
    case DomType(name) => sil.DomainType(name, Map.empty[sil.TypeVar, sil.Type])(Seq.empty)
  }

}

object DefaultSampleConverter
  extends DefaultSampleConverter
