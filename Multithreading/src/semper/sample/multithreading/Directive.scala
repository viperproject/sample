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


package semper.sample.multithreading

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.tracepartitioning._

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
