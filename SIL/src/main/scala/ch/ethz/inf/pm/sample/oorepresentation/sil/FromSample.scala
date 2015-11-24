package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.{BooleanOperator, ArithmeticOperator}
import viper.silver.{ast => sil}
import viper.silver.ast.SourcePosition

trait SampleConverter {
  /** Converts a Sample expression to a SIL expression. */
  def convert(e: sample.Expression): sil.Exp

  /** Converts a Sample program point to a SIL position. */
  def convert(pp: sample.ProgramPoint): sil.Position

  /** Converts a Sample type to a SIL type. */
  def convert(typ: sample.Type): sil.Type
}

object DefaultSampleConverter extends SampleConverter {

  def convert(e: sample.Expression): sil.Exp = e match {
    case sample.NegatedBooleanExpression(inner) => sil.Not(go(inner))()
    case sample.BinaryBooleanExpression(left, right, op, typ) => op match {
      case BooleanOperator.`&&` => sil.And(go(left), go(right))()
      case BooleanOperator.`||` => sil.Or(go(left), go(right))()
    }
    case sample.ReferenceComparisonExpression(left, right, op, typ) => op match {
      case ArithmeticOperator.`==` => sil.EqCmp(go(left), go(right))()
      case ArithmeticOperator.`!=` => sil.NeCmp(go(left), go(right))()
    }
    case sample.BinaryArithmeticExpression(left, right, op, typ) => op match {
      case ArithmeticOperator.`+` => sil.Add(go(left), go(right))()
      case ArithmeticOperator.`-` => sil.Sub(go(left), go(right))()
      case ArithmeticOperator.`*` => sil.Mul(go(left), go(right))()
      case ArithmeticOperator.`/` => sil.Div(go(left), go(right))()
      case ArithmeticOperator.`%` => sil.Mod(go(left), go(right))()
      case ArithmeticOperator.`>=` => sil.GeCmp(go(left), go(right))()
      case ArithmeticOperator.`<=` => sil.LeCmp(go(left), go(right))()
      case ArithmeticOperator.`==` => sil.EqCmp(go(left), go(right))()
      case ArithmeticOperator.`!=` => sil.NeCmp(go(left), go(right))()
      case ArithmeticOperator.`<` => sil.LtCmp(go(left), go(right))()
      case ArithmeticOperator.`>` => sil.GtCmp(go(left), go(right))()
    }
    case sample.UnaryArithmeticExpression(inner, op, typ) => op match {
      case ArithmeticOperator.`+` => go(inner)
      case ArithmeticOperator.`-` => sil.Minus(go(inner))()
    }
    case sample.Constant(c, typ, pp) => typ match {
      case sample.BoolType => c match {
        case "0" | "false" => sil.FalseLit()(go(pp))
        case "1" | "true" => sil.TrueLit()(go(pp))
        case _ => sys.error(s"unexpected boolean constant '$c'")
      }
      case sample.IntType =>
        // Apron only uses floating point values
        sil.IntLit(c.toFloat.toInt)()
      case t: sample.RefType => c match {
        case "null" => sil.NullLit()(go(pp))
        case _ => sys.error(s"unexpected reference constant '$c'")
      }
      case _ => sys.error(s"unexpected constant type $typ")
    }
    case id @ sample.VariableIdentifier(name, scope) =>
      name match {
        case Constants.ResultVariableName =>
          sil.Result()(go(id.typ), go(id.pp))
        case _ =>
          sil.LocalVar(name)(go(id.typ), go(id.pp))
      }
    case id @ sample.AccessPathIdentifier(path) =>
      val localVar = go(path.head)

      path.tail.foldLeft[sil.Exp](localVar)((exp, field) => {
        sil.FieldAccess(exp, sil.Field(field.getName, go(field.typ))())()
      })
  }

  def convert(pp: sample.ProgramPoint): sil.Position = pp match {
    case sample.DummyProgramPoint => sil.NoPosition
    case sample.WrappedProgramPoint(pos) => pos.asInstanceOf[SourcePosition]
  }

  def convert(typ: sample.Type): sil.Type = typ match {
    case sample.IntType => sil.Int
    case sample.BoolType => sil.Bool
    case sample.RefType(_) => sil.Ref
  }

  // Convenience aliases
  protected def go(e: sample.Expression) = convert(e)

  protected def go(pp: sample.ProgramPoint) = convert(pp)

  protected def go(typ: sample.Type) = convert(typ)
}