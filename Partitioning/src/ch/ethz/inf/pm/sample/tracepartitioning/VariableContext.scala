/**
 * This file is part of the thesis "Disjunction on Demand". The accompanying
 * report can be found at [1].
 *
 * References
 * [1] http://www.pm.inf.ethz.ch/education/theses/student_docs/Dominik_Gabi/dominik_gabi_MA_report
 * [2] Laurent Mauborgne and Xavier Rival, The Trace Partitioning
 * Abstract Domain, ACM Transactions on Programming Languages and Systems
 * (TOPLAS), vol. 29 (5), ACM, 2007
 */


package ch.ethz.inf.pm.sample.tracepartitioning

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.SystemParameters

/**
 * Provides the necessary context for a partitioning of a variable.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
trait VariableContext {

	/**
	 * The identifier for the variable in question.
	 *
	 * @return The identifier
	 */
	def identifier: VariableIdentifier

	/**
	 * Generates a constant of the type of the variable from a string.
	 *
	 * @param value The string representing the value
	 * @return The constant
	 */
	def constant(value: String): Constant

	/**
	 * The restrictions imposed on the variable by this context.
	 *
	 * @return A list of restrictions
	 */
	def restrictions: List[Restriction]

	/**
	 * Represents a restriction on a variable.
	 *
	 * @author Dominik Gabi
	 * @version 0.1
	 */
	abstract class Restriction(val context: VariableContext) {
		def expression: Expression
	}

	/**
	 * Represents a restriction on a values range. This range can be inclusive or
	 * exclusive on both sides.
	 *
	 * @author Dominik Gabi
	 * @version 0.1
	 */
	class Range (
								context: VariableContext,
								val lower: (String, Boolean),
								val upper: (String, Boolean)
								) extends Restriction(context) {

		override def expression: Expression = {
			val lop = if (lower._2) ArithmeticOperator.>= else ArithmeticOperator.>
			val rop = if (upper._2) ArithmeticOperator.<= else ArithmeticOperator.<

			BinaryBooleanExpression(
				BinaryArithmeticExpression(context.identifier, context.constant(lower._1), lop, null),
				BinaryArithmeticExpression(context.identifier, context.constant(upper._1), rop, null),
				BooleanOperator.&&,
				null
			)
		}
	}


	/**
	 * Represents an exact restriction on a value (i.e. an equality).
	 *
	 * @author Dominik Gabi
	 * @version 0.1
	 */
	class Value (
								context: VariableContext,
								val value: String
								) extends Restriction(context) {

		override def expression: Expression = {
			BinaryArithmeticExpression(context.identifier, context.constant(value), ArithmeticOperator.==, null)
		}

	}

}


/**
 * A minimal variable context that does not do any verification of its
 * parameters.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
class UncheckedVariableContext(val name: String, ranges: List[(Any, Any)]) extends VariableContext {
	 require(ranges.length > 0)

	val top = SystemParameters.getType().top()

	override def identifier: VariableIdentifier = VariableIdentifier(name, top, null, EmptyScopeIdentifier())

	override def constant(value: String): Constant = Constant(value, top, null)

	override def restrictions: List[Restriction] = {
		ranges.map(_ match {
			case (l, r) => if (l == r) new Value(this, l.toString) else new Range(this, (l.toString, true), (r.toString, true))
		})
	}

}



