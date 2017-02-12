/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{BinaryArithmeticExpression, _}
import ch.ethz.inf.pm.sample.oorepresentation.silver.{BoolType, DefaultSampleConverter, IntType, RefType}
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

  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

  def lcm(numbers: TraversableOnce[Int]): Int = numbers.reduce(lcm)

  def gcd(a: Int, b: Int): Int = b match {
    case 0 => a.abs
    case _ => gcd(b, a % b)
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
    case BinaryArithmeticExpression(Constant(const, IntType, _), other, ArithmeticOperator.*) => unOp(collect(other), const.toInt * _)
    case BinaryArithmeticExpression(other, Constant(const, IntType, _), ArithmeticOperator.*) => unOp(collect(other), const.toInt * _)
    case UnaryArithmeticExpression(left, ArithmeticOperator.-, _) => unOp(collect(left), - _)
    case UnaryArithmeticExpression(left, ArithmeticOperator.+, _) => collect(left)
    case Constant(const, IntType, _) => Map((ConstPlaceholder, const.toInt))
    case v: VariableIdentifier => Map((v, 1))
    case VariableIdentifierWithFactor(factor, variableIdentifier) => Map((variableIdentifier, factor))
  }

  private def collectedToExpr(collected: Map[Any, Int]): Expression = {
    val collectedNotNull = collected.filter {
      case (_, 0) => false
      case _ => true
    }
    collectedNotNull.size match {
      case 0 => zeroConst
      case 1 => collectedNotNull.head match {
        case (ConstPlaceholder, n) => const(n)
        case (other: VariableIdentifier, n) => mult(const(n), other)
      }
      case _ =>
        collectedNotNull.map {
          case (key: VariableIdentifier, value) => mult(const(value), key)
          case (ConstPlaceholder, value) => const(value)
        }.reduce(plus)
    }
  }

  private def collectAndToExpr(expr: Expression): Expression = collectedToExpr(collect(expr))

  def binOp(a: Map[Any, Int], b: Map[Any, Int], op: (Int, Int) => Int): Map[Any, Int] = a ++ b.transform((key, value) => op(a.getOrElse(key, 0), value))

  def unOp(a: Map[Any, Int], op: (Int) => Int): Map[Any, Int] = a.transform((_, value) => op(value))

  case object ConstPlaceholder

  private def containsModuloOrDivision(expr: Expression): Boolean = expr.contains {
    case BinaryArithmeticExpression(_, _, ArithmeticOperator.% | ArithmeticOperator./) => true
    case _ => false
  }

  def getCollected(expr: Expression): Expression = expr.transform {
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.%) => BinaryArithmeticExpression(collectAndToExpr(left), collectAndToExpr(right), ArithmeticOperator.%)
    case BinaryArithmeticExpression(left, right, op: ArithmeticOperator.Value) if left.typ == IntType && right.typ == IntType && ArithmeticOperator.isComparison(op) && !containsModuloOrDivision(left) && !containsModuloOrDivision(right) =>
      val collectedNotNull = binOp(collect(left), collect(right), _ - _).filter {
        case (_, 0) => false
        case _ => true
      }
      collectedNotNull.size match {
        case 0 => Constant(toComparisonOp(op)(0, 0).toString, BoolType)
        case 1 => collectedNotNull.head match {
          case (ConstPlaceholder, n) => Constant(toComparisonOp(op)(n, 0).toString, BoolType)
          case (other: VariableIdentifier, n) if n >= 0 => BinaryArithmeticExpression(BinaryArithmeticExpression(const(n), other, ArithmeticOperator.*), zeroConst, op)
          case (other: VariableIdentifier, n) if n < 0 => BinaryArithmeticExpression(zeroConst, BinaryArithmeticExpression(const(-n), other, ArithmeticOperator.*), op)
        }
        case _ =>
          val mapping: ((Any, Int)) => Expression = {
            case (key: VariableIdentifier, value) => mult(const(value), key)
            case (ConstPlaceholder, value) => const(value)
          }
          BinaryArithmeticExpression(collectedNotNull.filter {
            case (_, n) if n > 0 => true
            case (_, n) if n < 0 => false
          }.map(mapping).reduceOption(plus) match {
            case Some(e) => e
            case None => zeroConst
          }, unOp(collectedNotNull.filter {
            case (_, n) if n > 0 => false
            case (_, n) if n < 0 => true
          }, - _).map(mapping).reduceOption(plus) match {
            case Some(e) => e
            case None => zeroConst
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
    case BinaryArithmeticExpression(Constant(left, IntType, _), Constant(right, IntType, _), op) if ArithmeticOperator.isComparison(op) => const(toComparisonOp(op)(left.toInt, right.toInt))
    case BinaryArithmeticExpression(Constant(left, IntType, _), Constant(right, IntType, _), op) if ArithmeticOperator.isArithmetic(op) => const(toArithmeticOp(op)(left.toInt, right.toInt))
    case BinaryArithmeticExpression(`oneConst`, other, ArithmeticOperator.*) => other
    case BinaryArithmeticExpression(other, `oneConst`, ArithmeticOperator.* | ArithmeticOperator./) => other
    case BinaryArithmeticExpression(`zeroConst`, other, ArithmeticOperator.+ | ArithmeticOperator.-) => other
    case BinaryArithmeticExpression(other, `zeroConst`, ArithmeticOperator.+ | ArithmeticOperator.-) => other
    case BinaryArithmeticExpression(_, `oneConst`, ArithmeticOperator.%) => zeroConst
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.%) if left == right => zeroConst
    case b@BinaryArithmeticExpression(left, Constant(const, IntType, _), ArithmeticOperator.%) =>
      val collectLeft = collect(left)
      if (collectLeft.forall(collected => collected._2 % const.toInt == 0)) zeroConst
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

  val zeroConst = Constant("0", IntType)

  val oneConst = Constant("1", IntType)

  val trueConst = Constant("true", BoolType)

  val falseConst = Constant("false", BoolType)

  val nullConst = Constant("null", RefType())

  object ExpressionBuilder {

    def const(c: Int): Constant = Constant(c.toString, IntType)

    def const(c: Boolean): Constant = Constant(c.toString, BoolType)

    def plus(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.+)

    def neg(arg: Expression): UnaryArithmeticExpression = UnaryArithmeticExpression(arg, ArithmeticOperator.-, IntType)

    def minus(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.-)

    def mult(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.*)

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
