/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
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

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.util.StringBlock


/**
 * Represents a partitioning, the tree of tokens leading to a certain state.
 * 
 * @tparam D The leaf type
 *
 * @author Dominik Gabi
 * @version 0.1
 */
sealed abstract class Partitioning[D <: State[D]] extends Lattice[Partitioning[D]] {
	
	/**
	 * Generates a partitioning.
	 *
	 * @return The partitioning
	 */
	override def factory: Partitioning[D] = bottom
	
	/**
	 * The bottom element of the lattice.
	 *
	 * @return The partitioning
	 */
	override def bottom: Partitioning[D] = Bottom()
	
	/**
	 * The top element of the lattice.
	 *
	 * @return The partitioning
	 */
	override def top: Partitioning[D] = Top()

	/**
	 * Whether element is a supremum
	 *
	 * @return true iff element is Top or Bottom
	 */
	def isSupremum: Boolean = {
		this == Top() || this == Bottom()
	}

	/**
	 * Computes a canonical representation for the current partitioning.
	 *
	 * @return The canonical representation
	 */
	def canonical: Partitioning[D]

	/**
	 * The depth of the partitioning.
	 *
	 * @return The depth
	 */
	def depth: Int

	/**
	 * The width of the partitioning.
	 *
	 * @return The width
	 */
	def width: Int

	/**
	 * Maps a function on every leaf state of the partitioning.
	 *
	 * @param f A function transforming a state
	 * @return The partitioning with all leaves <code>Leaf(v)</code> replaced
	 * by <code>Leaf(f(v))</code>
	 */
	def map(f: D => D): Partitioning[D]

  /**
   * Takes a partitioning <code>p</code> with the same structure as <code>this
   * </code> and zips together all corresponding leaves using a specified
   * function.
	 *
   * @param p The other partitioning
   * @param f The function zipping together states
   * @return The modified partitioning
   */
  def zipmap(p: Partitioning[D], f: (D, D) => D): Partitioning[D] = {
    zipmap(List(p), (s: D, ss: List[D]) => f(s, ss.head))
  }

  /**
   * This is a generalized version of zipmap that zips together an
   * arbitrary number of trees of the same structure.
	 *
   * @param ps The other partitionings
   * @param f A function zipping together states
   * @return The modified partitioning
   */
  def zipmap(ps: List[Partitioning[D]], f: (D, List[D]) => D): Partitioning[D]

	/**
	 * The bottom state of the leaf type.
	 *
	 * @return The state
	 */
	def bottomState: D
	
	/**
	 * The top state of the leaf type
	 *
	 * @return The state
	 */
	def topState: D
	
	/**
	 * The greatest lower bound over all leaf states. This assumes that <code>
	 * glb</code> is commutative.
	 *
	 * @return The state
	 */
	def glbState: D
	
	/**
	 * The least upper bound over all leaf states. This assumes that <code>
	 * lub</code> is commutative.
	 *
	 * @return The state
	 */
	def lubState: D
	
	/**
	 * Computes a list of all states in the leaves of the partitioning.
	 *
	 * @return The list of states
	 */
	def states: List[D]
	
	/**
	 * Computes all directives used in the partitioning.
	 *
	 * @return The list of directives
	 */
	def directives: List[Directive[D]]
	
}


/**
 * Represents an inner node of the <code>Partitioning</code>.
 *  
 * @tparam D
 *
 * @author Dominik Gabi
 * @version 0.1
 */
case class Node[D <: State[D]](directive: Directive[D], children: List[Partitioning[D]]) extends Partitioning[D] {
	require(children.nonEmpty)
	require(children.exists(!_.isSupremum))
	require(children.length == tokens.length)

	val isTop = children.foldLeft(false)(_ && _.isTop)
	
	override def lessEqual(p: Partitioning[D]): Boolean = p match {
		case Top() => true
		case Bottom() => children.forall(_.lessEqual(Bottom()))
		case Node(d, cs) => directive.compatible(d) && children.indices.forall(i => children(i).lessEqual(cs(i)))
		case Leaf(v) => false
	}
	
	override def glb(p: Partitioning[D]): Partitioning[D] = p match {
		case Top() => this
		case Bottom() => Bottom()
		case Node(d, cs) => if (directive.compatible(d)) {
      Node(directive, for ((c1, c2) <- children.zip(cs)) yield c1.glb(c2))
    } else {
      Bottom()
    }
		case Leaf(_) => p.glb(this)
	}
	
	override def lub(p: Partitioning[D]): Partitioning[D] = p match {
		case Top() => Top()
		case Bottom() => this
		case Node(d, cs) => if (directive.compatible(d)) {
      Node(directive, for ((c1, c2) <- children.zip(cs)) yield c1.lub(c2))
    } else {
			(directive, d) match {
				case (PartitionWhileComputing(_, _), _) => Node(directive, children.patch(1, List(p.lub(children(1))), 1))
				case (_, PartitionWhileComputing(_, _)) => Node(d, cs.patch(1, List(this.lub(cs(1))), 1))
				case _ => Top()
			}
    }
		case Leaf(v) => directive match {
			case PartitionWhileComputing(_, n) => Node(directive, children.patch(1, List(p.lub(children(1))), 1))
			case _ => Node(directive, children.map(_.lub(p)))
		}
	}

	override def widening(p: Partitioning[D]): Partitioning[D] = p match {
		case Top() => Top()
		case Bottom() => this
		case Node(d, cs) => if (directive.compatible(d)) {
			Node(directive, for ((c1, c2) <- children.zip(cs)) yield c1.widening(c2))
		} else {
			(directive, d) match {
				case (PartitionWhileComputing(_, _), _) => Node(directive, children.patch(1, List(p.widening(children(1))), 1))
				case (_, PartitionWhileComputing(_, _)) => Node(d, cs.patch(1, List(this.widening(cs(1))), 1))
				case _ => Top()
			}
		}
		case Leaf(v) => Node(directive, children.map(_.widening(p)))
	}

	override def canonical: Partitioning[D] = directive match {
		case PartitionNone() => children.head
		case _ => Node(directive, children.map(_.canonical))
	}

	override def depth: Int = 1 + children.map(_.depth).max

	override def width: Int = children.map(_.width).sum
	
	override def map(f: D => D): Partitioning[D] = Node(directive, children.map(_.map(f)))

  override def zipmap(ps: List[Partitioning[D]], f: (D, List[D]) => D): Partitioning[D] = {
		val cs: List[List[Partitioning[D]]] = for (p <- ps) yield p match {
				case Node(d, cs) => if (directive.compatible(d)) {
					cs
				} else {
					throw CompatibilityException("zipmap: Incompatible partitioning")
				}
				case _ => throw CompatibilityException("zipmap: Incompatible partitioning")
			}

    Node(directive, for {
			(c, i) <- children.zipWithIndex
		} yield c.zipmap(for (css <- cs) yield css(i), f))
  }

	override def glbState: D = {
		val glb: Partitioning[D] = (top() /: children)((c1, c2) => c1.glb(c2))
		glb match {
			case Top() => topState
			case Bottom() => bottomState
			case Node(d, c) => (topState /: c.map(_.glbState))((c1, c2) => c1.glb(c2))
			case Leaf(v) => v
		}
	}

  override def bottomState: D = {
    (children.find(!_.isSupremum): @unchecked) match {
      // class invariant
      case Some(c) => c.bottomState
    }
  }

  override def topState: D = {
    (children.find(!_.isSupremum): @unchecked) match {
      case Some(c) => c.topState
    }
  }

	override def lubState: D = {
		val lub: Partitioning[D] = (bottom() /: children)((c1, c2) => c1.lub(c2))
		lub match {
			case Top() => topState
			case Bottom() => bottomState
			case Node(d, c) => (bottomState /: c.map(_.lubState))((c1, c2) => c1.lub(c2))
			case Leaf(v) => v
		}
	}
	
	override def states: List[D] = {
		children.map(_.states).flatten
	}
	
	override def directives: List[Directive[D]] = {
		directive :: children.map(_.directives).flatten
	}
	
	def tokens = directive.tokens

	override def toString: String = {
		val s = StringBlock.empty(5)
		val c = children.map(c => StringBlock(c.toString))

		val b = c.reduceLeft(_ + s + _)
		val t = StringBlock.border(directive.toString)
		StringBlock.border((s \\ t \\ s \\ b).toString).toString
	}

	override def isBottom = children.forall(_.isBottom)
}


/**
 * Represents a leaf of the <code>Partitioning</code>.
 *  
 * @tparam D
 *
 * @author Dominik Gabi
 * @version 0.1
 */
case class Leaf[D <: State[D]](value: D) extends Partitioning[D] {

	def isTop = value.isTop

	override def lessEqual(p: Partitioning[D]): Boolean = p match {
		case Top() => true
		case Bottom() => value.lessEqual(value.bottom())
		case Node(_, _) => value.lessEqual(p.lubState)
		case Leaf(v) => value.lessEqual(v)
	}
	
	override def glb(p: Partitioning[D]): Partitioning[D] = p match {
		case Top() => this
		case Bottom() => Bottom()
		case Node(_, _) => Leaf(value.glb(p.glbState))
		case Leaf(v) => Leaf(value.glb(v))
	}
	
	override def lub(p: Partitioning[D]): Partitioning[D] = p match {
		case Top() => Top()
		case Bottom() => this
		case Node(_, _) => p.lub(this)
		case Leaf(v) => Leaf(value.lub(v))
	}

	override def widening(p: Partitioning[D]): Partitioning[D] = p match {
		case Top() => Top()
		case Bottom() => this
		case Node(_, _) => p.widening(this)
		case Leaf(v) => Leaf(value.widening(v))
	}

	override def canonical: Partitioning[D] = this
	
	override def depth: Int = 0

	override def width: Int = 1
	
	override def map(f: D => D): Partitioning[D] = Leaf(f(value))

  override def zipmap(ps: List[Partitioning[D]], f: (D, List[D]) => D): Partitioning[D] = {
    Leaf(f(value, for (l <- ps) yield l match {
      case Leaf(v) => v
      case _ => throw CompatibilityException("zipmap: Incompatible partitionings")
    }))
  }

	override def bottomState: D = value.bottom
	
	override def topState: D = value.top
	
	override def glbState: D = value
	
	override def lubState: D = value
	
	override def states: List[D] = List(value)
	
  override def directives: List[Directive[D]] = Nil

  override def toString: String = StringBlock.border(value.toString).toString

	override def isBottom = value.isBottom
}


/**
 * Common operations on both Top() and Bottom()
 *
 * @author Dominik Gabi
 * @version 0.1
 */
sealed trait Supremum[D <: State[D]] extends Partitioning[D] {

	override def canonical: Partitioning[D] = this

	override def depth: Int = 0

	override def width: Int = 0

  override def map(f: D => D): Partitioning[D] = this

  override def zipmap(ps: List[Partitioning[D]], f: (D, List[D]) => D): Partitioning[D] = {
    if (ps.forall(_ == this)) {
      this
    } else {
      throw CompatibilityException("zipmap: Incompatible partitionings")
    }
  }

  override def bottomState: D = throw CompatibilityException("bottomState: supremum")

  override def topState: D = throw CompatibilityException("topState: on supremum")

  override def glbState: D = throw CompatibilityException("glbState: on supremum")

  override def lubState: D = throw CompatibilityException("lubState: on supremum")

  override def states: List[D] = Nil

  override def directives: List[Directive[D]] = Nil

}

/**
 * The bottom element of the lattice.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
case class Bottom[D <: State[D]]() extends Partitioning[D] with Supremum[D] {

	def isTop = false

	override def lessEqual(p: Partitioning[D]): Boolean = true
	
	override def glb(p: Partitioning[D]): Partitioning[D] = Bottom()
	
	override def lub(p: Partitioning[D]): Partitioning[D] = p

	override def widening(p: Partitioning[D]): Partitioning[D] = p

  override def toString: String = StringBlock.border(" B ").toString

	override def isBottom = true
}


/**
 * The top element of the lattice.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
case class Top[D <: State[D]]() extends Partitioning[D] with Supremum[D] {

	def isTop = true

	override def lessEqual(p: Partitioning[D]): Boolean = p match {
		case Top() => true
		case _ => false
	}
	
	override def glb(p: Partitioning[D]): Partitioning[D] = p
	
	override def lub(p: Partitioning[D]): Partitioning[D] = this

	override def widening(p: Partitioning[D]): Partitioning[D] = this

	override def toString: String = StringBlock.border(" T ").toString

	override def isBottom = false
}


/**
 * Exception thrown by <code>Partitioning</code>.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
class PartitioningException(msg: String) extends Exception(msg)


/**
 * Exception thrown on unsupported operations.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
case class UnsupportedException(msg: String) extends PartitioningException(msg)


/**
 * Exception thrown when attempting to operate with incompatible <code>
 * Partitioning</code>.
 *
 * @author Dominik Gabi
 * @version 0.1
 */
case class CompatibilityException(msg: String) extends PartitioningException(msg)
