/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{BinaryArithmeticExpression, _}
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.Type
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.QuantifiedPermissionsParameters._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils.ExpressionBuilder._
import viper.silicon.Silicon
//import viper.carbon.CarbonVerifier
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

  def isFunctionInjective(function: sil.FuncLike, expr: Expression, numericalInfo: NumericalDomainType, program: sil.Program = Context.program): Boolean = {
    val intDecls = Context.getQuantifiedVarDeclsForType(sil.Int, 2)
    val (i1, i2) = (intDecls.head, intDecls.last)
    val formalArgs = function.formalArgs.filter(formalArg => formalArg.typ != sil.Int) ++ intDecls
    val i1Id = VariableIdentifier(i1.name)(IntType)
    val i2Id = VariableIdentifier(i2.name)(IntType)
    val expressionToAssume1 = BinaryArithmeticExpression(i1Id, expr, ArithmeticOperator.==)
    val expressionToAssume2 = BinaryArithmeticExpression(i2Id, expr, ArithmeticOperator.==)
    val constraints1 = numericalInfo.createVariable(i1Id).assume(expressionToAssume1).removeVariables(numericalInfo.ids.getNonTop).getConstraints(Set(i1Id))
    val constraints2 = numericalInfo.createVariable(i2Id).assume(expressionToAssume2).removeVariables(numericalInfo.ids.getNonTop).getConstraints(Set(i2Id))
    val precondition =
      (Seq(sil.NeCmp(i1.localVar, i2.localVar)()) ++
      constraints1.map(DefaultSampleConverter.convert) ++
      constraints2.map(DefaultSampleConverter.convert)).reduceLeft(sil.And(_, _)())
    val postcondition = sil.NeCmp(
      sil.FuncLikeApp(function, function.formalArgs.map(formalArg => if (formalArg.typ == sil.Int) i1.localVar else formalArg.localVar), Map()),
      sil.FuncLikeApp(function, function.formalArgs.map(formalArg => if (formalArg.typ == sil.Int) i2.localVar else formalArg.localVar), Map()))()
    val methodToCheck = sil.Method(Context.createNewUniqueFunctionIdentifier("injectivity_test"), formalArgs, Seq(), Seq(precondition), Seq(postcondition), Seq(), sil.Seqn(Seq())())()
    val newProgram: sil.Program = sil.Program(program.domains, program.fields, program.functions, program.predicates, Seq(methodToCheck))()
    val silicon = new Silicon(Seq(("startedBy", "viper.silicon.SiliconTests")))
    silicon.parseCommandLine(Seq("dummy.sil"))
    silicon.config.initialize { case _ => silicon.config.initialized = true }
    silicon.start()
    silicon.verify(newProgram) match {
      case Success => true
      case _ => false
    }
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

  private def toCNF(expr: Expression): Expression = toNNF(expr).transform {
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

  def toCNFConjuncts(expr: Expression): Set[Expression] = splitToConjuncts(toCNF(expr))

  def collect(expr: Expression): Map[Any, Int] = expr match {
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.+) => binOp(collect(left), collect(right), _ + _)
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.-) => binOp(collect(left), collect(right), _ - _)
    case BinaryArithmeticExpression(Constant(const, _, _), other, ArithmeticOperator.*) => unOp(collect(other), const.toInt * _)
    case BinaryArithmeticExpression(other, Constant(const, _, _), ArithmeticOperator.*) => unOp(collect(other), const.toInt * _)
    case UnaryArithmeticExpression(left, ArithmeticOperator.-, _) => unOp(collect(left), - _)
    case UnaryArithmeticExpression(left, ArithmeticOperator.+, _) => collect(left)
    case Constant(const, _, _) => Map((ConstPlaceholder, const.toInt))
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
      val collectedNonZero = binOp(collect(left), collect(right), _ - _).filter {
        case (_, 0) => false
        case _ => true
      }
      collectedNonZero.size match {
        case 0 => toComparisonOp(op)(0, 0)
        case 1 => collectedNonZero.head match {
          case (ConstPlaceholder, n) => toComparisonOp(op)(n, 0)
          case (other: VariableIdentifier, n) if n >= 0 => comp(other, intToConst(0, typ), op)
          case (other: VariableIdentifier, n) if n < 0 => comp(intToConst(0, typ), UnaryArithmeticExpression(other, ArithmeticOperator.-, typ), op)
        }
        case _ =>
          val greatestCommonDivisor = gcd(collectedNonZero.values)
          val mapping: ((Any, Int)) => Expression = {
            case (ConstPlaceholder, value) => intToConst(value / greatestCommonDivisor, typ)
            case (key: Expression, value) => mult(intToConst(value / greatestCommonDivisor, key.typ), key)
          }
          BinaryArithmeticExpression(collectedNonZero.filter {
            case (_, n) if n > 0 => true
            case (_, n) if n < 0 => false
          }.map(mapping).reduceOption(plus) match {
            case Some(e) => e
            case None => intToConst(0, typ)
          }, unOp(collectedNonZero.filter {
            case (_, n) if n > 0 => false
            case (_, n) if n < 0 => true
          }, - _).map(mapping).reduceOption(plus) match {
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
  def simplifyExpression(expr: Expression): Expression = getCollected(expr).transform {
    case BinaryBooleanExpression(`trueConst`, other, BooleanOperator.&&) => other
    case BinaryBooleanExpression(other, `trueConst`, BooleanOperator.&&) => other
    case BinaryBooleanExpression(`falseConst`, _, BooleanOperator.&&) | BinaryBooleanExpression(_, `falseConst`, BooleanOperator.&&) => falseConst
    case BinaryBooleanExpression(`falseConst`, other, BooleanOperator.||) => other
    case BinaryBooleanExpression(other, `falseConst`, BooleanOperator.||) => other
    case BinaryBooleanExpression(`trueConst`, _, BooleanOperator.||) | BinaryBooleanExpression(_, `trueConst`, BooleanOperator.||) => trueConst
    case BinaryBooleanExpression(left, right, _) if left == right => left
    case NegatedBooleanExpression(NegatedBooleanExpression(arg)) => arg
    case ConditionalExpression(`trueConst`, left, _, _) => left
    case ConditionalExpression(`falseConst`, _, right, _) => right
    case ReferenceComparisonExpression(left, right, ReferenceOperator.==) if left == right => trueConst
    case BinaryArithmeticExpression(Constant(left, _, _), Constant(right, _, _), op) if ArithmeticOperator.isComparison(op) => Constant(toComparisonOp(op)(left.toInt, right.toInt).toString, BoolType)
    case BinaryArithmeticExpression(Constant(left, leftTyp, _), Constant(right, _, _), op) if ArithmeticOperator.isArithmetic(op) => Constant(toArithmeticOp(op)(left.toInt, right.toInt).toString, leftTyp)
    case BinaryArithmeticExpression(Constant("1", _, _), other, ArithmeticOperator.*) => other
    case BinaryArithmeticExpression(other, Constant("1", _, _), ArithmeticOperator.* | ArithmeticOperator./) => other
    case BinaryArithmeticExpression(Constant("0", _, _), other, ArithmeticOperator.+ | ArithmeticOperator.-) => other
    case BinaryArithmeticExpression(other, Constant("0", _, _), ArithmeticOperator.+ | ArithmeticOperator.-) => other
    case BinaryArithmeticExpression(_, Constant("1", typ, _), ArithmeticOperator.%) => intToConst(0, typ)
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.%) if left == right => intToConst(0, left.typ)
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.<= | ArithmeticOperator.== | ArithmeticOperator.>=) if left == right => trueConst
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.< | ArithmeticOperator.!= | ArithmeticOperator.>) if left == right => falseConst
    case b@BinaryArithmeticExpression(left, Constant(const, _, _), ArithmeticOperator.%) =>
      val collectLeft = collect(left)
      if (collectLeft.forall(_._2 % const.toInt == 0)) intToConst(0, left.typ)
      else b
    case other => other
  } match {
    case transformed if transformed == expr => transformed
    case transformed => simplifyExpression(transformed)
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

  def intToConst(c: Int, typ: Type): Constant = Constant(c.toString, typ)

  implicit def boolToConst(c: Boolean): Constant = Constant(c.toString, BoolType)

  object ExpressionBuilder {

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
  }
}

object Main2 {
  def main(args: Array[String]): Unit = {
    val a = VariableIdentifier("A")(BoolType)
    val b = VariableIdentifier("B")(BoolType)
    val c = VariableIdentifier("C")(BoolType)
    val d = VariableIdentifier("D")(BoolType)
    val e = VariableIdentifier("E")(BoolType)
    println(NegatedBooleanExpression(BinaryBooleanExpression(a, BinaryBooleanExpression(b, BinaryBooleanExpression(c, BinaryBooleanExpression(d, e, BooleanOperator.||), BooleanOperator.&&), BooleanOperator.||), BooleanOperator.&&)))
//    println(Utils.toCNF(NegatedBooleanExpression(BinaryBooleanExpression(a, BinaryBooleanExpression(b, BinaryBooleanExpression(c, BinaryBooleanExpression(d, e, BooleanOperator.||), BooleanOperator.&&), BooleanOperator.||), BooleanOperator.&&))))
    println(Utils.toCNFConjuncts(NegatedBooleanExpression(BinaryBooleanExpression(a, BinaryBooleanExpression(b, BinaryBooleanExpression(c, BinaryBooleanExpression(d, e, BooleanOperator.||), BooleanOperator.&&), BooleanOperator.||), BooleanOperator.&&))))
    println(NegatedBooleanExpression(BinaryBooleanExpression(a, BinaryBooleanExpression(b, c, BooleanOperator.&&), BooleanOperator.||)))
//    println(Utils.toCNF(NegatedBooleanExpression(BinaryBooleanExpression(a, BinaryBooleanExpression(b, c, BooleanOperator.&&), BooleanOperator.||))))
  }
}
