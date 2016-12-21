/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.td.cloud

import ch.ethz.inf.pm.sample.abstractdomain._

object Z3Sample {

  def withZ3[A](f: Z3Prover[Expression,VariableIdentifier] => A,
      converter: Z3SampleExpressionConverter = Z3SampleExpressionConverter(),
      config: Z3Prover.Config = Z3Prover.Config(),
      bookkeeper: Z3Prover.Bookkeeper = Z3Prover.Bookkeeper()): A = {
    val z3 = Z3Prover[Expression,VariableIdentifier](converter,config,bookkeeper)
    try {
      val res = f(z3)
      z3.stop()
      res
    } catch {
      case t: Throwable =>
        z3.stop()
        throw t
    }
  }

}

case class Z3SampleExpressionConverter() extends Z3Prover.ExpressionConverter[Expression,VariableIdentifier] {

  override def name(v: VariableIdentifier): String = v.name

  override def sort(v: VariableIdentifier): String = {
    if (v.typ.isStringType) {
      "String"
    } else if (v.typ.isBooleanType) {
      "Bool"
    } else if (v.typ.isNumericalType && !v.typ.isFloatingPointType) {
      "Int"
    } else if (v.typ.isNumericalType && v.typ.isFloatingPointType) {
      "Real"
    } else {
      throw new UnsupportedOperationException("Cannot encode non-primitive types into SMT")
    }
  }

  override def convert(expr: Expression): String = {
    expr match {
      case NegatedBooleanExpression(exp) =>
        "(not " + convert(exp) + ")"
      case BinaryBooleanExpression(left, right, op) =>
        "(" + convertBO(op) + " " + convert(left) + " " + convert(right) + ")"
      case BinaryArithmeticExpression(left, right, op) =>
        "(" + convertAO(op) + " " + convert(left) + " " + convert(right) + ")"
      case UnaryArithmeticExpression(left, op, _) =>
        "(" + convertAO(op) + " " + convert(left) + ")"
      case Constant(constant, typ, _) if typ.isStringType =>
        "\"" + constant + "\""
      case Constant(constant, typ, _) if !typ.isStringType =>
        constant
      case VariableIdentifier(name, _) =>
        name
      case _ =>
        throw new UnsupportedOperationException("This expression cannot be converted into SMT syntax")
    }
  }

  private def convertBO(op: BooleanOperator.Value): String = {
    op match {
      case BooleanOperator.&& => "and"
      case BooleanOperator.|| => "or"
    }
  }

  private def convertAO(op: ArithmeticOperator.Value): String = {
    op match {
      case ArithmeticOperator.+ => "+"
      case ArithmeticOperator.- => "-"
      case ArithmeticOperator.* => "*"
      case ArithmeticOperator./ => "/"
      case ArithmeticOperator.% => "%"
      case ArithmeticOperator.>= => ">="
      case ArithmeticOperator.<= => "<="
      case ArithmeticOperator.== => "="
      case ArithmeticOperator.!= => "!="
      case ArithmeticOperator.> => ">"
      case ArithmeticOperator.< => "<"
    }
  }

  override def vars(goal: Expression): Set[VariableIdentifier] =
    goal.ids.getNonTopUnsafe.collect { case x:VariableIdentifier => x }
}
