/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{BinaryArithmeticExpression, _}
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.Type
import viper.silicon.Silicon
import viper.silver.verifier.Success
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 16/11/16.
  */
object Utils {

  def lcm(a: Int, b: Int): Int = (a * b / gcd(a, b)).abs

  def lcmOption(numbers: TraversableOnce[Int]): Option[Int] = numbers.reduceOption(lcm)

  def lcm(numbers: TraversableOnce[Int]): Int = lcmOption(numbers) match {
    case Some(lcm) => lcm
    case None => 1
  }

  def gcd(a: Int, b: Int): Int = b match {
    case 0 => a.abs
    case _ => gcd(b, a % b)
  }

  def gcdOption(numbers: TraversableOnce[Int]): Option[Int] = numbers.reduceOption(gcd)

  def gcd(numbers: TraversableOnce[Int]): Int = gcdOption(numbers) match {
    case Some(gcd) => gcd
    case None => 0
  }

  def mergeElements[T](seq: Seq[T], mergeFun: (T, T) => Option[T]): Seq[T] = seq.foldLeft[Seq[T]](Seq()) {
    case (existing, nextElement) => existing.find(mergeFun(nextElement, _).isDefined) match {
      case Some(result) => (existing.takeWhile(_ != result) :+ mergeFun(nextElement, result).get) ++ existing.dropWhile(_ != result).tail
      case None => existing :+ nextElement
    }
  }

  def getLiterals(expr: Expression): Set[Expression] = expr match {
    case BinaryBooleanExpression(left, right, _) => getLiterals(left) ++ getLiterals(right)
    case other => Set(other)
  }

  def isFunctionInjective(function: sil.FuncLike, expr: Expression, constraints: Expression, program: sil.Program = Context.program): Boolean = {
    val intDecls = Context.getQuantifiedVarDeclsForType(sil.Int, 2)
    val (i1, i2) = (intDecls.head, intDecls.last)
    val formalArgs = function.formalArgs.filter(formalArg => formalArg.typ != sil.Int) ++ intDecls
    val i1Id = VariableIdentifier(i1.name)(IntType)
    val i2Id = VariableIdentifier(i2.name)(IntType)
    val constraints1 = QuantifierElimination.eliminate(expr.ids.toSetOrFail, and(equ(i1Id, expr), constraints))
    val constraints2 = QuantifierElimination.eliminate(expr.ids.toSetOrFail, and(equ(i2Id, expr), constraints))
    val precondition = sil.And(sil.NeCmp(i1.localVar, i2.localVar)(), sil.And(DefaultSampleConverter.convert(constraints1), DefaultSampleConverter.convert(constraints2))())()
    val postcondition = sil.NeCmp(
      sil.FuncLikeApp(function, function.formalArgs.map(formalArg => if (formalArg.typ == sil.Int) i1.localVar else formalArg.localVar), Map()),
      sil.FuncLikeApp(function, function.formalArgs.map(formalArg => if (formalArg.typ == sil.Int) i2.localVar else formalArg.localVar), Map()))()
    val methodToCheck = sil.Method(Context.createNewUniqueFunctionIdentifier("injectivity_test"), formalArgs, Seq(), Seq(precondition), Seq(postcondition), Seq(), sil.Seqn(Seq())())()
    val newProgram: sil.Program = sil.Program(program.domains, program.fields, program.functions, program.predicates, Seq(methodToCheck))()
    val silicon = new Silicon(Seq(("startedBy", "viper.silicon.SiliconTests")))
    silicon.parseCommandLine(Seq("dummy.sil"))
    silicon.start()
    val result = silicon.verify(newProgram) match {
      case Success => true
      case _ => false
    }
    silicon.stop()
    result
  }

  def toNNF(expr: Expression): Expression = expr.transform {
    case NegatedBooleanExpression(BinaryBooleanExpression(left, right, op)) =>
      BinaryBooleanExpression(NegatedBooleanExpression(left), NegatedBooleanExpression(right), BooleanOperator.negate(op))
    case NegatedBooleanExpression(NegatedBooleanExpression(arg)) => arg
    case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op)) if ArithmeticOperator.isComparison(op) => BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op))
    case other => other
  }.transform {
    case BinaryBooleanExpression(left, right, _) if left == right => left
    case other => other
  } match {
    case transformed if transformed.equals(expr) => transformed
    case transformed => toNNF(transformed)
  }

  def toCNF(expr: Expression): Expression = toNNF(expr).transform {
    case BinaryBooleanExpression(left, BinaryBooleanExpression(innerLeft, innerRight, BooleanOperator.&&), BooleanOperator.||) =>
      BinaryBooleanExpression(BinaryBooleanExpression(left, innerLeft, BooleanOperator.||), BinaryBooleanExpression(left, innerRight, BooleanOperator.||), BooleanOperator.&&)
    case BinaryBooleanExpression(BinaryBooleanExpression(innerLeft, innerRight, BooleanOperator.&&), right, BooleanOperator.||) =>
      BinaryBooleanExpression(BinaryBooleanExpression(innerLeft, right, BooleanOperator.||), BinaryBooleanExpression(innerRight, right, BooleanOperator.||), BooleanOperator.&&)
    case other => other
  }.transform {
    case BinaryBooleanExpression(left, right, _) if left == right => left
    case other => other
  } match {
    case transformed if transformed.equals(expr) => transformed
    case transformed => toCNF(transformed)
  }

  def splitToConjuncts(expr: Expression): Set[Expression] = expr match {
    case BinaryBooleanExpression(left, right, BooleanOperator.&&) => splitToConjuncts(left) ++ splitToConjuncts(right)
    case cnfExpr => Set(cnfExpr)
  }

  def splitToDisjuncts(expr: Expression): Set[Expression] = expr match {
    case BinaryBooleanExpression(left, right, BooleanOperator.||) => splitToDisjuncts(left) ++ splitToDisjuncts(right)
    case dnfExpr => Set(dnfExpr)
  }

  def toCNFConjuncts(expr: Expression): Set[Expression] = splitToConjuncts(toCNF(expr))

  def collect(expr: Expression): Map[Any, Int] = expr match {
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.+) => binOp(collect(left), collect(right), _ + _)
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.-) => binOp(collect(left), collect(right), _ - _)
    case BinaryArithmeticExpression(Constant(const, _, _), other, ArithmeticOperator.*) => unOp(collect(other), const.toInt * _)
    case BinaryArithmeticExpression(other, Constant(const, _, _), ArithmeticOperator.*) => unOp(collect(other), const.toInt * _)
    case UnaryArithmeticExpression(left, ArithmeticOperator.-, _) => unOp(collect(left), - _)
    case UnaryArithmeticExpression(left, ArithmeticOperator.+, _) => collect(left)
    case Constant(const, _, _) => Map((ConstPlaceholder, const.toInt))
    case FractionalPermissionExpression(numerator, denominator) => Map((ConstPlaceholder, numerator / denominator))
    case other => Map((other, 1))
  }

  private def collectedToExpr(collected: Map[Any, Int], typ: Type): Expression = {
    val collectedNotNull = collected.filter {
      case (_, 0) => false
      case _ => true
    }
    collectedNotNull.size match {
      case 0 => intToConst(0, typ)
      case 1 => collectedNotNull.head match {
        case (ConstPlaceholder, n) => intToConst(n, typ)
        case (other: VariableIdentifier, n) => mult(intToConst(n, typ), other)
      }
      case _ =>
        collectedNotNull.map {
          case (key: VariableIdentifier, value) => mult(intToConst(value, typ), key)
          case (ConstPlaceholder, value) => intToConst(value, typ)
        }.reduce(plus)
    }
  }

  private def collectAndToExpr(expr: Expression): Expression = collectedToExpr(collect(expr), expr.typ)

  def binOp(a: Map[Any, Int], b: Map[Any, Int], op: (Int, Int) => Int): Map[Any, Int] = a ++ b.transform((key, value) => op(a.getOrElse(key, 0), value))

  def unOp(a: Map[Any, Int], op: (Int) => Int): Map[Any, Int] = a.transform((_, value) => op(value))

  case object ConstPlaceholder

  private def containsModuloOrDivision(expr: Expression): Boolean = expr.contains {
    case BinaryArithmeticExpression(_, _, ArithmeticOperator.% | ArithmeticOperator./) => true
    case _ => false
  }

  def getCollected(expr: Expression): Expression = expr.transform {
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.%) => BinaryArithmeticExpression(collectAndToExpr(left), collectAndToExpr(right), ArithmeticOperator.%)
    case BinaryArithmeticExpression(left, right, op: ArithmeticOperator.Value) if ArithmeticOperator.isComparison(op) && !containsModuloOrDivision(left) && !containsModuloOrDivision(right) =>
      val typ = left.typ
      def divider(g: Int): ((Any, Int)) => Expression = {
        case (ConstPlaceholder, value) => intToConst(value / g, typ)
        case (key: Expression, value) => mult(intToConst(value / g, key.typ), key)
      }
      binOp(collect(left), collect(right), _ - _).filter {
        case (_, 0) => false
        case _ => true
      }.toList match {
        case Nil => toComparisonOp(op)(0, 0)
        case singleElement :: Nil => singleElement match {
          case (ConstPlaceholder, n) => toComparisonOp(op)(n, 0)
          case (other: Expression, n) if n >= 0 => comp(other, intToConst(0, typ), op)
          case (other: Expression, n) if n < 0 => comp(other, intToConst(0, typ), ArithmeticOperator.flip(op))
        }
        case (first@(key1, value1)) :: (second@(key2, value2)) :: Nil =>
          val greatestCommonDivisor = gcd(value1, value2)
          val mapping = divider(greatestCommonDivisor)
          (value1 > 0, value2 > 0) match {
            case (true, false) => comp(mapping(first), mapping((key2, -value2)), op)
            case (false, true) => comp(mapping(second), mapping((key1, -value1)), op)
            case (true, true) | (false, false) => comp(mapping(first), mapping((key2, -value2)), op)
          }
        case other =>
          val greatestCommonDivisor = gcd(other.unzip._2)
          val mapping = divider(greatestCommonDivisor)
          BinaryArithmeticExpression(other.filter {
            case (_, n) if n > 0 => true
            case (_, n) if n < 0 => false
          }.map(mapping).reduceOption(plus) match {
            case Some(e) => e
            case None => intToConst(0, typ)
          }, unOp(Map(other.filter {
            case (_, n) if n > 0 => false
            case (_, n) if n < 0 => true
          }: _*), -_).map(mapping).reduceOption(plus) match {
            case Some(e) => e
            case None => intToConst(0, typ)
          }, op)
      }
    case other => other
  }

  /**
    * Produces an equivalent expression that is potentially simplified. E.g. this method simplifies constant expressions
    * like '1 > 0' to 'true' and it also eliminates identity operations. e.g 'x + 0' will be replaced by 'x'.
    *
    * @param expr The expression to simplify.
    * @return An equivalent expression that is potentially simpler than the original one.
    */
  def simplifyExpression(expr: Expression): Expression = {
    val rdAmountName = Context.getRdAmountVariable.name
    getCollected(toNNF(expr)).transform {
      // Simplify binary boolean expressions, e.g. true && a = a
      case b@BinaryBooleanExpression(left, right, op) => (left, right, op) match {
        case (`trueConst`, other, BooleanOperator.&&) => other
        case (other, `trueConst`, BooleanOperator.&&) => other
        case (`falseConst`, _, BooleanOperator.&&) | (_, `falseConst`, BooleanOperator.&&) => falseConst
        case (`falseConst`, other, BooleanOperator.||) => other
        case (other, `falseConst`, BooleanOperator.||) => other
        case (`trueConst`, _, BooleanOperator.||) | (_, `trueConst`, BooleanOperator.||) => trueConst
        case _ if left == right => left
        case _ => b
      }
      // Eliminate doubly negated boolean expressions and negations of constants
      case n@NegatedBooleanExpression(arg) => arg match {
        case NegatedBooleanExpression(nestedArg) => nestedArg
        case `trueConst` => falseConst
        case `falseConst` => trueConst
        case _ => n
      }
      // Simplify constant conditions in ternary expressions
      case ConditionalExpression(`trueConst`, left, _, _) => left
      case ConditionalExpression(`falseConst`, _, right, _) => right
      // Comparing two syntactically equal elements is trivially true
      case ReferenceComparisonExpression(left, right, ReferenceOperator.==) if left == right => trueConst
      // Simplify binary arithmetic expressions, e.g. 1 * a = a
      case b@BinaryArithmeticExpression(left, right, op) => (left, right, op) match {
        case (Constant("1", PermType, _), VariableIdentifier(`rdAmountName`, _), _) if ArithmeticOperator.isComparison(op) => toComparisonOp(op)(1, 0)
        case (VariableIdentifier(`rdAmountName`, _), Constant("1", PermType, _), _) if ArithmeticOperator.isComparison(op) => toComparisonOp(op)(0, 1)
        case (Constant("0", PermType, _), VariableIdentifier(`rdAmountName`, _), _) if ArithmeticOperator.isComparison(op) => toComparisonOp(op)(0, 1)
        case (VariableIdentifier(`rdAmountName`, _), Constant("0", PermType, _), _) if ArithmeticOperator.isComparison(op) => toComparisonOp(op)(1, 0)
        case (Constant(leftConst, _, _), Constant(rightConst, _, _), _) if ArithmeticOperator.isComparison(op) => Constant(toComparisonOp(op)(leftConst.toInt, rightConst.toInt).toString, BoolType)
        case (Constant(leftConst, leftTyp, _), Constant(rightConst, _, _), _) if ArithmeticOperator.isArithmetic(op) => Constant(toArithmeticOp(op)(leftConst.toInt, rightConst.toInt).toString, leftTyp)
        case (Constant("1", _, _), other, ArithmeticOperator.*) => other
        case (Constant("-1", _, _), other, ArithmeticOperator.*) => neg(other)
        case (other, Constant("1", _, _), ArithmeticOperator.* | ArithmeticOperator./) => other
        case (other, Constant("-1", _, _), ArithmeticOperator.* | ArithmeticOperator./) => neg(other)
        case (Constant("0", _, _), other, ArithmeticOperator.+ | ArithmeticOperator.-) => other
        case (other, Constant("0", _, _), ArithmeticOperator.+ | ArithmeticOperator.-) => other
        case (_, Constant("1" | "-1", typ, _), ArithmeticOperator.%) => intToConst(0, typ)
        case (_, _, ArithmeticOperator.%) if left == right => intToConst(0, left.typ)
        case (_, _, ArithmeticOperator.<= | ArithmeticOperator.== | ArithmeticOperator.>=) if left == right => trueConst
        case (_, _, ArithmeticOperator.< | ArithmeticOperator.!= | ArithmeticOperator.>) if left == right => falseConst
        case (_, Constant(const, _, _), ArithmeticOperator.%) =>
          val collectLeft = collect(left)
          if (collectLeft.forall(_._2 % const.toInt == 0)) intToConst(0, left.typ)
          else b
        case _ => b
      }
      case other => other
    } match {
      case transformed if transformed == expr => transformed
      case transformed => simplifyExpression(transformed) // Recursively apply simplifications until no more simplification can be applied
    }
  }

  def toArithmeticOp(op: ArithmeticOperator.Value): (Int, Int) => Int = op match {
    case ArithmeticOperator.+ => _ + _
    case ArithmeticOperator.- => _ - _
    case ArithmeticOperator.* => _ * _
    case ArithmeticOperator./ => _ / _
    case ArithmeticOperator.% => _ % _
  }

  def toComparisonOp(op: ArithmeticOperator.Value): (Int, Int) => Boolean = op match {
    case ArithmeticOperator.< => _ < _
    case ArithmeticOperator.> => _ > _
    case ArithmeticOperator.<= => _ <= _
    case ArithmeticOperator.>= => _ >= _
    case ArithmeticOperator.== => _ == _
    case ArithmeticOperator.!= => _ != _
  }

  val trueConst = Constant("true", BoolType)

  val falseConst = Constant("false", BoolType)

  val nullConst = Constant("null", RefType())

  val noneConst = Constant("0", PermType)

  val writeConst = Constant("1", PermType)

  def intToConst(c: Int, typ: Type): Constant = Constant(c.toString, typ)

  implicit def boolToConst(c: Boolean): Constant = Constant(c.toString, BoolType)

  def countLiterals(expr: Expression): Int = {
    var count = 1
    expr.foreach {
      case _: BinaryBooleanExpression => count += 1
      case _ =>
    }
    count
  }

  def plus(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.+)

  def neg(arg: Expression): UnaryArithmeticExpression = UnaryArithmeticExpression(arg, ArithmeticOperator.-, IntType)

  def minus(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.-)

  def mult(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.*)

  def div(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator./)

  def modulo(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.%)

  def comp(left: Expression, right: Expression, op: ArithmeticOperator.Value): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, op)

  def lt(left: Expression, right: Expression): BinaryArithmeticExpression = comp(left, right, ArithmeticOperator.<)

  def leq(left: Expression, right: Expression): BinaryArithmeticExpression = comp(left, right, ArithmeticOperator.<=)

  def gt(left: Expression, right: Expression): BinaryArithmeticExpression = comp(left, right, ArithmeticOperator.>)

  def geq(left: Expression, right: Expression): BinaryArithmeticExpression = comp(left, right, ArithmeticOperator.>=)

  def equ(left: Expression, right: Expression): BinaryArithmeticExpression = comp(left, right, ArithmeticOperator.==)

  def neq(left: Expression, right: Expression): BinaryArithmeticExpression = comp(left, right, ArithmeticOperator.!=)

  def and(left: Expression, right: Expression): BinaryBooleanExpression = BinaryBooleanExpression(left, right, BooleanOperator.&&)

  def or(left: Expression, right: Expression): BinaryBooleanExpression = BinaryBooleanExpression(left, right, BooleanOperator.||)

  def not(arg: Expression): NegatedBooleanExpression = NegatedBooleanExpression(arg)

  def implies(left: Expression, right: Expression): BinaryBooleanExpression = or(not(left), right)

  def iff(left: Expression, right: Expression): BinaryBooleanExpression = or(and(left, right), and(not(left), not(right)))
}
