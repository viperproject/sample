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

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import scala.util.parsing.combinator.JavaTokenParsers


/**
 * Companion object facilitates creation and parsing of directives.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
object Directive {

	/**
	 * An error if one occurred during parsing, None otherwise.
	 */
	var error: Option[String] = None

	/**
	 * Attempts to parse a directive from a given string. The supported directives
	 * are listed in the DirectiveParser object.
	 *
	 * @param input The string to parse
	 * @return The parsed directive, or None if it could not be parsed
	 */
	def parse[D <: State[D]](input: String): Option[Directive[D]] = {
		parseUntyped(input) match {
			case Some(directive) => Some(directive.asInstanceOf[Directive[D]])
			case None => None
		}
	}

	/**
	 * Attempts to parse a directive from a given string. The supported directives
	 * are listed in the DirectiveParser object.
	 *
	 * @param input The string to parse
	 * @return The parsed directive, or None if it could not be parsed
	 */
	def parseUntyped(input: String): Option[Directive[_]] = {
		import DirectiveParser._
		try {
			parseAll(directive, input) match {
				case DirectiveParser.Success(directive, _) => {
					error = None
					Some(directive)
				}
				case r => {
					error = Some(r.toString)
					None
				}
			}
		} catch {
			case _ => None
		}
	}

	object DirectiveParser extends JavaTokenParsers {

		def programPoint: Parser[ProgramPoint] = {
			"("~>decimalNumber~","~decimalNumber<~")" ^^ {
				case l~","~c => ProgramPointConversions.programPoint(l.toInt, c.toInt)
			}
		}

		def directive: Parser[Directive[Nothing]] = {
			merge | partitionIf | partitionWhile | partitionValue | partitionSign
		}

		def merge: Parser[Merge[Nothing]] = {
			"Merge("~>programPoint~","~programPoint<~")" ^^ {
				case p~","~s => {
					TracePartitioning.get(s) match {
						case ds @ (_::_) => new Merge[Nothing](p, ds.head)
						case _ => throw ParserException("Source for merge at (" + p + ") not found")
					}
				}
			}
		}

		def partitionIf: Parser[PartitionIf[Nothing]] = {
			"PartitionIf("~>programPoint<~")" ^^ { p => PartitionIf(p) }
		}

		def partitionWhile: Parser[PartitionWhile[Nothing]] = {
			"PartitionWhile("~>programPoint~","~decimalNumber<~")" ^^ {
				case p~","~n => PartitionWhile(p, n.toInt)
			}
		}

		def partitionValue: Parser[PartitionValue[Nothing]] = {
			"PartitionValue("~>programPoint~","~stringLiteral~","~repsep(restriction, ",")<~")" ^^ {
				case p~","~v~","~r => {
					new PartitionValue(p, new UncheckedVariableContext(v, r))
				}
			}
		}

		def partitionSign: Parser[PartitionSign[Nothing]] = {
			"PartitionSign("~>programPoint~","~stringLiteral<~")" ^^ {
				case p~","~v => new PartitionSign(p, v)
			}
		}

		def restriction: Parser[(Int, Int)] = {
			(range | value) ^^ {
				case v: Int => (v, v)
				case r: (Int, Int) => r
			}
		}

		override def stringLiteral: Parser[String] = """[\w\-]+""".r

		def value: Parser[Int] = {
			stringLiteral ^^ {
				case "inf" => Int.MaxValue
				case "-inf" => Int.MinValue
				case l => l.toInt
			}
		}

		def range: Parser[(Int, Int)] = {
			"("~>value~","~value<~")" ^^ {
				case l~","~u => (l, u)
			}
		}

		case class ParserException(msg: String) extends Exception(msg)

	}
}


/**
 * This class represents the directives responsible for manipulating 
 * partitionings.
 *
 * @param <D> is the type of the leaf values.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
abstract class Directive[D <: State[D]](val programPoint: ProgramPoint) extends PartitionedStateObserver[D] {
	
	/**
	 * Apply the directive to a partitioning.
	 *
	 * @param p A partitioning.
	 * @return The transformed partitioning.
	 */
	def apply(p: Partitioning[D]): Partitioning[D] 
	
	/**
	 * The tokens that are generated by the directive.
	 *
	 * @return A list of tokens.
	 */
	def tokens: List[Token]

	/**
	 * Two directives are compatible if the least upper bound etc. are defined.
	 *
	 * @param o Some other directive
	 * @return Whether or not this node is compatible with another
	 */
	def compatible(o: Any): Boolean = equals(o)
}


/**
 * Represents a basic token of a <code>Partitioning</code>.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
abstract class Token(pp: ProgramPoint)


/**
 * The <code>Merge</code> is the inverse of a normal partitioning. It results 
 * in a coarser partitioning (i.e. <code>merge(p).lessEqual(p)</code>).
 * 
 * @param <D>
 *
 * @author Dominik Gabi
 * @version 0.1
 */
case class Merge[D <: State[D]](pp: ProgramPoint, source: Directive[D]) extends Directive[D](pp) {
	
	/**
	 * Merge the partitioning created by the <code>source</code> directive as 
	 * described in [1].
	 *
	 * @param p A partitioning
	 * @return The partitioning without any occurrences of the <code>source
	 * </code> directive. 
	 */
	override def apply(p: Partitioning[D]): Partitioning[D] = p match {
		case Node(d, c) => if (source.compatible(d)) {
				Node(PartitionNone(), c.reduceLeft((c1, c2) => c1.lub(c1, c2)) :: Nil)
			} else {
				Node(d, c.map(apply(_)))
			}
		case _ => p
	}
	
	override def tokens: List[Token] = Nil
	
}


/**
 * An empty partition that does nothing. Currently unused.
 * 
 * @param <D>
 * 
 * @author Dominik Gabi
 * @version 0.1
 */
case class PartitionNone[D <: State[D]]() extends Directive[D](null) {
	
	override def apply(p: Partitioning[D]): Partitioning[D] = p
	
	override def tokens: List[Token] = List(Void())

	/**
	 * The empty token.
	 */
	case class Void() extends Token(null)

}


/**
 * Partitions the state along a conditional in the control flow.
 * 
 * @param <D>
 *
 * @author Dominik Gabi
 * @version 0.1
 */
case class PartitionIf[D <: State[D]](pp: ProgramPoint) extends Directive[D](pp) {
	
	/**
	 * Partitions each leaf into a <code>Node</code> that contains the leaves
	 * representing an execution for either choice in the control flow.
	 *
	 * @param p The initial partitioning.
	 * @return The refined partitioning.
	 */
	override def apply(p: Partitioning[D]): Partitioning[D] = p match {
		case Node(d, c) => Node(d, c.map(apply(_)))
		case Leaf(v) => Node(this, List.fill(2)(Leaf(v)))
		case _ => p
	}

	/**
	 * Tokens representing both branches.
	 *
	 * @return The list of tokens
	 */
	override def tokens: List[Token] = If(pp, true) :: If(pp, false) :: Nil
	
	/**
	 * Called when true branch is taken. This results in discarding the states 
	 * along the false branch.
	 *
	 * @param p The initial partitioning.
	 * @return The partitioning with the subtrees along the false branch
	 * replaced by <code>Bottom()</code>.
	 */
	override def testTrue(p: Partitioning[D]): Partitioning[D] = {
		partition(p, true)
	}
	
	/**
	 * Called when false branch is taken. This results in discarding the states 
	 * along the true branch.
	 *
	 * @param p The initial partitioning.
	 * @return The partitioning with the subtrees along the true branch
	 * replaced by <code>Bottom()</code>.
	 */
	override def testFalse(p: Partitioning[D]): Partitioning[D] = {
		partition(p, false)
	}
	
	private def partition(p: Partitioning[D], branch: Boolean): Partitioning[D] = p match {
		case Node(d, c) => if (compatible(d)) {
				if (branch == true) {
					Node(d, c(0) :: Bottom[D]() :: Nil)
				} else {
					Node(d, Bottom[D]() :: c(1) :: Nil)
				}
			} else {
				Node(d, c.map(partition(_, branch)))
			}
		case _ => p
	}

	/**
	 * The token representing a choice in the control flow.
	 */
	case class If(pp: ProgramPoint, branch: Boolean) extends Token(pp)

}


/**
 * Partitions the state along different possible executions of a while loop.
 *
 * @param <D>
 *
 * @author Dominik Gabi
 * @version 0.1
 */
sealed abstract class PartitionWhileBase[D <: State[D]](pp: ProgramPoint, n: Int) extends Directive[D](pp) {
	require(n > 0)

	/**
	 * Partitions each leaf into <code>n+2</code> children representing <code>
	 * infinite, 0 and 1...n  loop iterations respectively. The partitioning is
	 * only applied once.
	 *
	 * @param p The initial partitioning
	 * @return The partitioning where each leaf is replaced by the <code>n+2</code>
	 * considered loop iteration patterns.
	 */
	override def apply(p: Partitioning[D]): Partitioning[D] = p match {
		case Node(d, c) => if (compatible(d)) {
			p
		} else {
			Node(d, c.map(apply(_)))
		}
		case Leaf(v) => {
			Node(this, Bottom[D]() :: Leaf[D](v) :: List.fill(n)(Bottom[D]()))
		}
		case _ => p
	}

	override def compatible(o: Any): Boolean = o match {
		case PartitionWhileComputing(p, c) => pp == p && n == c
		case PartitionWhile(p, c) => pp == p && n == c
		case _ => false
	}

	override def testTrue(p: Partitioning[D]): Partitioning[D] = p match {
		case Node(d, c) => if (compatible(d)) {
			val ci = if (c(0) != Bottom()) c(0) else c.last
			Node(PartitionWhileComputing(pp, n), ci :: Bottom[D]() :: c.tail.take(n))
		} else {
			Node(d, c.map(testTrue(_)))
		}
		case _ => p
	}

	override def testFalse(p: Partitioning[D]): Partitioning[D] = p match {
		case Node(d, c) => if (compatible(d)) {
			Node(PartitionWhile(pp, n), c)
		} else {
			Node(d, c.map(testFalse(_)))
		}
		case _ => p
	}

	/**
	 * Tokens representing the <code>n+2</code> possible loop iteration patterns.
	 *
	 * @return The list of tokens
	 */
	override def tokens: List[Token] = {
		(Int.MaxValue :: (0 to n).toList).map(While(pp, _))
	}

	/**
	 * The token representing the number of loop executions.
	 */
	case class While(pp: ProgramPoint, iterations: Int) extends Token(pp)

}

case class PartitionWhile[D <: State[D]](pp: ProgramPoint, n: Int) extends PartitionWhileBase[D](pp, n)

case class PartitionWhileComputing[D <: State[D]](pp: ProgramPoint, n: Int) extends PartitionWhileBase[D](pp, n)


/**
 * Partitions the state along different possible conditions.
 *
 * @param <D>
 *
 * @author Dominik Gabi
 * @version 0.1
 */
case class PartitionCondition[D <: State[D]](pp: ProgramPoint, conditions: List[Expression]) extends Directive[D](pp) {

	override def apply(p: Partitioning[D]): Partitioning[D] = p match {
		case Node(d, c) => Node(d, c.map(apply(_)))
		case Leaf(v) => {
			val c = conditions.map(c => Leaf(v.assume(new SymbolicAbstractValue(c, v))))
			Node(this, c)
		}
		case _ => p
	}

	override def tokens: List[Token] = conditions.map(Condition(pp, _))

	/**
	 * The token representing a choice in the control flow.
	 */
	case class Condition(pp: ProgramPoint, condition: Expression) extends Token(pp)

}


/**
 * Partitions the state along different possible values.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
class PartitionValue[D <: State[D]](pp: ProgramPoint, context: VariableContext) extends PartitionCondition[D](pp, context.restrictions.map(_.expression))


/**
 * Partitions the state along the possible signs of a variable
 *
 * @author Dominik Gabi
 * @version 0.1
 */
class PartitionSign[D <: State[D]](
																pp: ProgramPoint,
																name: String
																) extends PartitionValue[D](pp, new UncheckedVariableContext(name, List(
		(Int.MinValue, -1), (0, 0), (1, Int.MaxValue)
	)))

