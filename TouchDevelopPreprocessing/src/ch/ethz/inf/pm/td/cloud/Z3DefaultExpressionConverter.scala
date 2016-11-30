/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.td.cloud

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type

case class Z3DefaultExpressionConverter() extends Z3Prover.ExpressionConverter {

  override def declare(expr: VariableIdentifier): String = {
    "(declare-const " + expr.name + " " + convertSort(expr.typ) + ")"
  }

  def convertSort(typ: Type): String = {
    if (typ.isStringType) {
      "String"
    } else if (typ.isBooleanType) {
      "Bool"
    } else if (typ.isNumericalType && !typ.isFloatingPointType) {
      "Int"
    } else if (typ.isNumericalType && typ.isFloatingPointType) {
      "Real"
    } else {
      throw new UnsupportedOperationException("Cannot encode non-primitive types into SMT")
    }
  }

  override def convert(expr: Expression): String = {
    expr match {
      case NegatedBooleanExpression(exp) =>
        "(not " + convert(exp) + ")"
      case BinaryBooleanExpression(left, right, op, _) =>
        "(" + convertBO(op) + " " + convert(left) + " " + convert(right) + ")"
      case BinaryArithmeticExpression(left, right, op, _) =>
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

  override def fresh(str: String): String = ???

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
  
}
