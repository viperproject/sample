/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

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

import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaProgramPoint
import scala.collection.mutable
import ch.ethz.inf.pm.sample.abstractdomain.State
import scala.reflect.internal.util.Position
import scala.language.implicitConversions

/**
 * Global object responsible for settings etc.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
object TracePartitioning {
	/**
	 * Adds automatic conversion from program points to pairs.
	 */
	import ProgramPointConversions._

	/**
	 * A map containing program points and their associated directives.
	 */
	private val directives = new mutable.HashMap[(Int, Int), List[Directive[_]]]

  def getDirectives(): Iterable[Directive[_]] = directives.values.flatten

	/**
	 * Get a list of  directives for a program point
	 *
	 * @param p The program point
	 * @return The list of directives
	 */
	def get[D <: State[D]](p: ProgramPoint): List[Directive[D]] = {
		directives.get(p.asInstanceOf[ScalaProgramPoint]) match {
			case Some(d) => d.asInstanceOf[List[Directive[D]]]
			case _ => Nil
		}
	}

	/**
	 * Add a directive
	 *
	 * @param d The directive
	 */
	def add[D <: State[D]](d: Directive[D]) {
		val pp = d.programPoint.asInstanceOf[ScalaProgramPoint]
		directives(pp) = d::get(pp)
	}

	/**
	 * Remove all directives at a program point
	 *
	 * @param p The program point
	 */
	def remove(p: ProgramPoint) {
		directives -= p.asInstanceOf[ScalaProgramPoint]
	}

	/**
	 * Remove a directive
	 *
	 * @param d The directive
	 */
	def remove(d: Directive[_]) {
		val pp = d.programPoint
		directives(pp.asInstanceOf[ScalaProgramPoint]) = get(pp).filter((x: Directive[_]) => x != d)
	}

	/**
	 * Resets to a default state
	 */
	def reset() {
		directives.clear()
		maxPartitioningDepth = defaultMaxPartitioningDepth
		maxPartitioningWidth = defaultMaxPartitioningWidth
	}

	/**
	 * The maximum depth a partitioning can have during the analysis.
	 */
	var maxPartitioningDepth: Int = 5

	/**
	 * The maximum depth a partitioning can have during the analysis.
	 */
	val defaultMaxPartitioningDepth: Int = 5


	/**
	 * The maximum width a partitioning can have during analysis
	 */
	var maxPartitioningWidth: Int = 25

	/**
	 * The maximum width a partitioning can have during analysis
	 */
	val defaultMaxPartitioningWidth: Int = 25

}


/**
 * This object facilitates handling program points.
 *
 * @author Dominik Gabi
 * @version 0.1
 */

object ProgramPointConversions {

	/**
	 * Creates a program point.
	 *
	 * @param l The line of the program point
	 * @param c The column of the program point
	 * @return The program point
	 */
	def programPoint(l: Int, c: Int): ScalaProgramPoint = new ScalaProgramPoint(new Position {
		override val line = l
		override val column = c
	}) /*new ProgramPoint {
		def getLine: Int = l
		def getColumn: Int = c

		override def equals(other: Any): Boolean = other match {
			case pp: ProgramPoint => pp.getLine == l && pp.getColumn == c
			case _ => false
		}

		override def hashCode: Int = l.hashCode + c.hashCode
	}*/

	/**
	 * Implicit conversion from pairs to program points.
	 *
	 * @param p A pair representing the row and the column of the program point
	 * @return The program point
	 */
	implicit def pairToProgramPoint(p: (Int, Int)) = programPoint(p._1, p._2)

	/**
	 * Implicit conversion from program points to pairs.
	 *
	 * @param p The program point
	 * @return A pair of the line and the column of the program point
	 */
	implicit def programPointToPair(p: ScalaProgramPoint): (Int, Int) = (p.getLine, p.getColumn)

}


