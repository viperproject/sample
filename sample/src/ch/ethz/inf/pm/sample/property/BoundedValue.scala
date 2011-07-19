package ch.ethz.inf.pm.sample.property

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._

/**
 * Created by IntelliJ IDEA.
 * User: dominik
 * Date: 7/10/11
 * Time: 1:25 PM
 * To change this template use File | Settings | File Templates.
 */

class BoundedValue(val variable: String, val lower: Int, val upper: Int) extends Visitor {

	def this(id: String) = this(id, Int.MinValue+1, Int.MaxValue-1)

	override def getLabel: String = {
		"Bounded variable: " + stringRepresentation(lower) + " <= " + variable + " <= " + stringRepresentation(upper)
	}

	private def stringRepresentation(i: Int): String = {
		if (i == Int.MinValue) {
			"-inf"
		} else if (i == Int.MaxValue) {
			"inf"
		} else {
			i.toString
		}
	}

	override def checkSingleStatement[S <: State[S]](s: S, stmt: Statement, out: OutputCollector) {
		val lr = stmt match {
			case VariableDeclaration(_, l, _, r) => Some((l, r))
			case Assignment(_, l, r) => Some((l, r))
			case _ => None
		}

		lr match {
			case Some((l, r)) => {
				val sl = l.forwardSemantics(s)
				val sa = stmt.forwardSemantics(s)
				for (pl <- sl.getExpression.getExpressions) {
					pl match {
						case VariableIdentifier(v, _, pp) => {
							if (v == variable) {
								val expr = BinaryBooleanExpression(
									BinaryArithmeticExpression(pl, new Constant(lower.toString, null, null), ArithmeticOperator.>=, null),
									BinaryArithmeticExpression(pl, new Constant(upper.toString, null, null), ArithmeticOperator.<=, null),
									BooleanOperator.&&,
									null
								)
								if (!sa.lessEqual(sa.assume(new SymbolicAbstractValue(expr, sa)))) {
									out.add(new WarningProgramPoint(pp, "Possible unbounded assignment to " + v))
								} else {
									out.add(new ValidatedProgramPoint(pp, "Bounded assignment to " + v))
								}
							}
						}
						case _ => ()
					}
				}
			}
			case _ => ()
		}
	}

}


class LowerBoundedValue(variable: String, lower: Int) extends BoundedValue(variable, lower, Int.MaxValue)


class UpperBoundedValue(variable: String, upper: Int) extends BoundedValue(variable, Int.MinValue, upper)

