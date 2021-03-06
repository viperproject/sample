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
import ch.ethz.inf.pm.sample.oorepresentation._


/**
 * Represents a partitioned state.
 *
 * @tparam D The leaf type
 *
 * @author Dominik Gabi
 * @version 0.1
 */
class PartitionedState[D <: State[D]] (val partitioning: Partitioning[D])
  extends State[PartitionedState[D]] {

  require(partitioning != null)

  def isTop = partitioning.isTop

  /**
   * Auxiliary constructor for a single state.
    *
    * @param s The state
   */
  def this(s: D) = this(Leaf[D](s))

  /**
   * The depth of the current partitioning.
   */
  lazy val depth = partitioning.depth

  /**
   * The width of the current partitioning.
   */
  lazy val width = partitioning.width

  /**
   * All directives used in the current partitioning.
   */
  lazy val directives: List[Directive[D]] = {
    partitioning.directives
  }

  /**
   * Directives that are associated with expressions currently present in the
   * leaves of the partitioning.
   */
  lazy val activeDirectives: List[Directive[D]] = {
    directives.filter(d => programPoints.contains(d.programPoint))
  }

  /**
   * Program points that are associated with the expressions currently present
   * in the leaf states.
   */
  lazy val programPoints: List[ProgramPoint] = {
    expr.toSetOrFail.map(_.pp).toList
  }

  /**
   * Looks if there are directives available for the given statement and
   * applies them.
   *
   * @param p The program point
   * @return This state or, if a directive is available, this state after
   * applying said directive
   */
  def before(p: ProgramPoint): PartitionedState[D] = {
    (this /: TracePartitioning.get[D](p))((s, d) => s.apply(d))
  }

  /**
   * Applies a directive if it is either a merging or the if the current depth
   * does allow further partitionings.
   *
   * @param d The directive
   * @return The state after applying the directive
   */
  def apply(d: Directive[D]): PartitionedState[D] = d match {
    case Merge(pp, dd) => new PartitionedState(d.apply(partitioning).canonical)
    case _ => if (depth < TracePartitioning.maxPartitioningDepth && width < TracePartitioning.maxPartitioningWidth) {
      new PartitionedState(d.apply(partitioning))
    } else {
      this
    }
  }

  /**
   * A string representation of the partitioned state
   *
   * @return The string
   */
  override def toString = partitioning.toString

  /**
   * Generates a partitioned state. The default value is the bottom of the lattice.
   *
   * @return The generated state
   */
  override def factory: PartitionedState[D] = bottom

  /**
   * The top element of the lattice contains the top element of the partitioning.
   *
   * @return The partitioned state
   */
  override def top: PartitionedState[D] = {
    new PartitionedState(partitioning.top)
  }

  /**
   * The top element of the lattice contains the top element of the partitioning.
   *
   * @return The partitioned state
   */
  override def bottom: PartitionedState[D] = {
    new PartitionedState(partitioning.bottom)
  }

  /**
   * The least upper bound of two partitioned states operates on the pair-wise
   * on the structure of the partitioning and the leaf states.
   *
   * @param other Another partitioned state
   * @return The least upper bound of the two arguments
   */
  override def lub(other: PartitionedState[D]): PartitionedState[D] = {
    new PartitionedState(partitioning.lub(other.partitioning))
  }

  /**
   * The greatest lower bound of two partitioned states operates on the pair-wise
   * on the structure of the partitioning and the leaf states.
   *
   * @param other Another partitioned state
   * @return The greatest lower bound of the two arguments
   */
  override def glb(other: PartitionedState[D]) : PartitionedState[D] = {
    new PartitionedState(partitioning.glb(other.partitioning))
  }

  /**
   * The actual widening is implemented in {@link #apply}. The directives
   * themselves are responsible for a more fine grained widening.
   * The widening here is just applied to the leaf states.
   *
   * @param other Another partitioned state
   * @return The least upper bound of the two arguments
   */
  override def widening(other: PartitionedState[D]): PartitionedState[D] = {
    new PartitionedState[D](partitioning.widening(other.partitioning))
  }

  /**
   * The order on the partitioned states is defined pair-wise on the structure
   * of the partitioning as well as the leaf states.
   *
   * @param r The other partitioned state
   * @return <code>true</code> iff <code>this</code> is less equal than the
   * argument
   */
  override def lessEqual(r: PartitionedState[D]): Boolean = {
    partitioning.lessEqual(r.partitioning)
  }

  override def isBottom = partitioning.isBottom

  /**
   * Creates an object in all leaf states.
   *
   * @param t
   * @param pp
   * @return The state after creating the object in all leaves
    * @see #map
   */
  override def createObject(t: Type, pp: ProgramPoint): PartitionedState[D] = {
    map(_.createObject(t, pp))
  }

  /**
   * Creates the variable in all leaf states.
   *
   * @param x
   * @param t
   * @param pp
   * @return The lub of all partitioned states after creating the variable for
   * each expression in x in all leaves
    * @see #mapValue
   */
  override def createVariable(x: ExpressionSet, t: Type, pp: ProgramPoint): PartitionedState[D] = {
    mapValue(x, (s, v) => s.createVariable(v, t, pp))
  }

  /**
   * Creates a variable for parameters in all leaf states.
   *
   * @param x
   * @param t
   * @return The lub of all partitioned states after creating the variable for
   * each expression of the argument in all leaves
    * @see #mapValue
   */
  override def createVariableForArgument(x: ExpressionSet, t: Type): PartitionedState[D] = {
    mapValue(x, (s, v) => s.createVariableForArgument(v, t))
  }

  /**
   * Assigns an expression to a variable in all leaf states.
   *
   * @param x
   * @param r
   * @return The lub of all partitioned states after assigning each expression
   * in <code>r</code> to each variable in <code>x</code> in all leaves
    * @see #mapValues
   */
  override def assignVariable(x: ExpressionSet, r: ExpressionSet): PartitionedState[D] = {
    mapValues(x, r, (s, ex, er) => s.assignVariable(ex, er))
  }

  /**
   * Assigns a field to a value in all leaf states.
   *
   * @return The lub of all partitioned states after assigning all possible
   * expressions to all possible combinations of fields in all leaves
    * @see #mapValues
   */
  override def assignField(o: ExpressionSet, f: String, r: ExpressionSet) = {
    mapValues(o, r, (s, ex, er) => s.assignField(ex, f, er))
  }

  /**
   * Assigns an expression to an initial parameter in all leaf states.
   *
   * @param x
   * @param r
   * @return The lub of all partitioned states after assigning each parameter
   * in <code>r</code> to each variable in <code>x</code> in all leaves
    * @see #mapValues
   */
  override def setArgument(x: ExpressionSet, r: ExpressionSet): PartitionedState[D] = {
    mapValues(x, r, (s, ex, er) => s.setArgument(ex, er))
  }

  /**
   * Sets a variable to top in all leaf states.
   *
   * @param x
   * @return The lub of all partitioned states after setting the parameter for
   * each expression in <code>x</code> in all leaves
    * @see #mapValue
   */
  override def setVariableToTop(x: ExpressionSet): PartitionedState[D] = {
    mapValue(x, (s, v) => s.setVariableToTop(v))
  }

  /**
   * Removes a variable from all leaf states.
   *
   * @param x
   * @return The lub of all partitioned states after removing each expression in
   * <code>x</code> in all leaves
    * @see #mapValue
   */
  override def removeVariable(x: ExpressionSet): PartitionedState[D] = {
    mapValue(x, (s, v) => s.removeVariable(v))
  }

  /**
   * Throws an exception in all leaf states.
   *
   * @param x
   * @return The lub of all partitioned states after throwing each exception in
   * <code>x</code> in all leaves
    * @see #mapValue
   */
  override def throws(x: ExpressionSet): PartitionedState[D] = {
    mapValue(x, (s, v) => s.throws(v))
  }

  /**
   * Gets the value of a variable in each leaf state.
   *
   * @param i
   * @return The state after getting the variable in each leaf
    * @see #map
   */
  override def getVariableValue(i: Identifier): PartitionedState[D] = {
    map(_.getVariableValue(i))
  }

  /**
   * Gets the value of a field in each leaf state.
   *
   * @param o
   * @param f
   * @param t
   * @return The lub of all partitioned states after mapping all combinations
   * on all leaves
    * @see #mapValueList
   */
  override def getFieldValue(o: ExpressionSet, f: String, t: Type): PartitionedState[D] = {
    mapValue(o, (s, v) => s.getFieldValue(v, f, t))
  }

  /**
   * Backward semantics of variable access in all leaf states.
   *
   * @param i
   * @return The state before accessing the variables in all leaves
   * @see #map
   */
  override def refiningGetVariableValue(i: Identifier): PartitionedState[D] = {
    map(_.refiningGetVariableValue(i))
  }

  /**
   * Backward semantics of field access in all leaf states.
   *
   * @param o
   * @param f
   * @param t
   * @return The state before accessing the field in all leaves
    * @see #mapValue
   */
  override def refiningGetFieldValue(o: ExpressionSet, f: String, t: Type): PartitionedState[D] = {
    mapValue(o, (s, v) => s.refiningGetFieldValue(v, f, t))
  }

  /**
   * Backward semantics of variable assignment in all leaf states.
   *
   * @param x
   * @param r
   * @return The state before the assignment in all leaves
    * @see #mapValues
   */
  override def refiningAssignVariable(oldPreState: PartitionedState[D], x: ExpressionSet, r: ExpressionSet): PartitionedState[D] = ???

  /**
   * Evaluates the numerical constant in all leaf states.
   *
   * @param v
   * @param t
   * @param pp
   * @return The state after evaluating the constant in all leaves
    * @see #map
   */
  override def evalConstant(v: String, t: Type, pp: ProgramPoint): PartitionedState[D] = {
    map(_.evalConstant(v, t, pp))
  }

  /**
   * Assumes that expression holds in all leaf states.
   *
   * @param c
   * @return The state after assuming the condition in all leaves
    * @see #mapValue
   */
  override def assume(c: ExpressionSet): PartitionedState[D] = {
    mapValue(c, (s, v) => s.assume(v))
  }

  /**
   * Assumes that the current expression holds in all leaf states.
   *
   * @return The state after assuming the current expression in all leaves
    * @see #map
   */
  override def testTrue: PartitionedState[D] = {
    val result = map(_.testTrue)
    dispatch(result, (o, p) => o.testTrue(p))
  }

  /**
   * Assumes that the current expression does not hold in all leaf states.
   *
   * @return The state after assuming the inverse of the current expression in
   * all leaves
    * @see #map
   */
  override def testFalse: PartitionedState[D] = {
    val result = map(_.testFalse)
    dispatch(result, (o, p) => o.testFalse(p))
  }

  /**
   * Associates all expressions of the leaf states with the current partitioned
   * state.
   *
   * @return The symbolic abstract value
   */
  override def expr: ExpressionSet = {
    var expr = ExpressionSet() // TODO: Maybe I could be more precise
    for (s <- partitioning.states; e <- s.expr.toSetOrFail)
      expr = expr.add(e)
    expr
  }

  /**
   * Sets the expression to all leaf states.
   *
   * @return The modified state
   */
  override def setExpression(e: ExpressionSet): PartitionedState[D] = {
    mapValue(e, (s, v) => s.setExpression(v))
  }

  /**
   * Removes the expression from all leaf states.
   *
   * @return The modified state
   */
  override def removeExpression: PartitionedState[D] = {
    map(_.removeExpression)
  }

  def pruneVariables(filter:VariableIdentifier => Boolean) : PartitionedState[D] = {
    throw new NotImplementedError()
  }

  def pruneUnreachableHeap(): PartitionedState[D] = {
    throw new NotImplementedError()
  }

  def optimizeSummaryNodes(): PartitionedState[D] = {
    throw new NotImplementedError()
  }

  /**
   * Maps a function transforming a leaf state and applies it to all leaves of
   * the partitioning.
   *
   * @param f The function
   * @return The state after applying the function to all leaves
   */
  private[this] def map(f: D => D): PartitionedState[D] = {
    new PartitionedState[D](partitioning.map(f))
  }

  /**
   * Maps a function taking a leaf state and a symbolic abstract value as arguments
   * to all leaves of the partitioning. Since value is not necessarily deterministic,
   * the initial is the combination (least upper bound) of all possible deterministic
   * values.
   *
   * @param x The symbolic abstract value
   * @param f The function
   * @return The least upper bound after applying the function with all possible
   * deterministic arguments to all leaves
   */
  private[this] def mapValue(x: ExpressionSet, f: (D, ExpressionSet) => D): PartitionedState[D] = {
    val separate = for {
      ex <- x.toSetOrFail
      px = this.partitioning
      pc = partitioning.zipmap(px, (s1: D, s2: D) => f(s1, new ExpressionSet(x.typ).add(ex)))
    } yield new PartitionedState(pc)

    lub(separate)
  }

  /**
   * Maps a function taking a leaf state and two symbolic abstract values as
   * arguments to all leaves of the partitioning. As with the single argument
   * ({@link #mapValue}), the initial is a combination of all possible combinations
   * of deterministic values of the two argument values.
   *
   * @param x A symbolic abstract value
   * @param y An other symbolic abstract value
   * @param f The function
   * @return The least upper bound after applying the function with all possible
   * combinations of deterministic arguments to all leaves
    * @see #mapValue
   */
  private[this] def mapValues(x: ExpressionSet, y: ExpressionSet, f: (D, ExpressionSet, ExpressionSet) => D): PartitionedState[D] = {
    val separate = for {
      ex <- x.toSetOrFail
      ey <- y.toSetOrFail
      px = this.partitioning
      py = this.partitioning
      pc = partitioning.zipmap(List(px, py), (s: D, ss: List[D]) => f(s, new ExpressionSet(x.typ).add(ex), new ExpressionSet(y.typ).add(ey)))
    } yield new PartitionedState[D](pc)

    lub(separate)
  }


  /**
   * Maps a function taking a leaf state and two symbolic abstract values as
   * arguments to all leaves of the partitioning. As with the single argument
   * ({@link #mapValue}), the initial is a combination of all possible combinations
   * of deterministic values of the two argument values.
   *
   * @param x A symbolic abstract value
   * @param y An other symbolic abstract value
   * @param f The function
   * @return The least upper bound after applying the function with all possible
   * combinations of deterministic arguments to all leaves
    * @see #mapValue
   */
  private[this] def mapValues(x: ExpressionSet, y: ExpressionSet, z: ExpressionSet, f: (D, ExpressionSet, ExpressionSet, ExpressionSet) => D): PartitionedState[D] = {
    val separate = for {
      ex <- x.toSetOrFail
      ey <- y.toSetOrFail
      ez <- z.toSetOrFail
      px = this.partitioning
      py = this.partitioning
      pz = this.partitioning
      pc = partitioning.zipmap(List(px, py, pz), (s: D, ss: List[D]) => f(s, new ExpressionSet(x.typ).add(ex), new ExpressionSet(y.typ).add(ey), new ExpressionSet(z.typ).add(ez)))
    } yield new PartitionedState[D](pc)

    lub(separate)
  }

  /**
   * Maps a function taking a state and a list of symbolic abstract values to
   * the leaf states. This generalizes {@link #mapValue} by applying the function
   * with all possible deterministic argument lists and taking the least upper
   * bound.
   *
   * @param v The list of abstract values
   * @param f The function
   * @return The least upper bound after applying the function with all possible
   * deterministic argument lists to all leaves
   */
  private[this] def mapValueList(xs: List[ExpressionSet], f: (D, List[ExpressionSet]) => D): PartitionedState[D] = {
    val separate = for {
      cx <- combinations(xs)
      es = cx.map(_.toSetOrFail.head)
      ps = for ((e, v) <- es.zip(cx)) yield this.partitioning
      pc = partitioning.zipmap(ps, (s: D, ss: List[D]) => f(s, for ((e, t) <- es.zip(ss)) yield ExpressionSet(e)))
    } yield new PartitionedState(pc)

    lub(separate)
  }

  /**
   * Maps a function taking a state and two lists of symbolic abstract values to
   * all leaf states. This is a combination of {@link #mapValues} and {@link
   * #mapValueList} and applies the function with all possible combinations of
   * all possible deterministic lists to all leaves.
   *
   * @param xs A list of symbolic abstract values
   * @param ys Another list of symbolic abstract values
   * @param f The function
   * @return The least upper bound after applying the function with all possible
   * combinations of all possible deterministic argument lists to all leaves
   */
  private[this] def mapValueLists(xs: List[ExpressionSet], ys: List[ExpressionSet], f: (D, List[ExpressionSet], List[ExpressionSet]) => D): PartitionedState[D] = {
    val separate = for {
      cx <- combinations(xs)
      cy <- combinations(ys)
      n = cx.length
      es = cx.map(_.toSetOrFail.head):::cy.map(_.toSetOrFail.head)
      ps = for ((e, v) <- es.zip(cx:::cy)) yield this.partitioning
      pc = partitioning.zipmap(ps, (s: D, ss: List[D]) => {
        f(s,
          for ((e, t) <- es.zip(ss).take(n)) yield ExpressionSet(e),
          for ((e, t) <- es.zip(ss).drop(n)) yield ExpressionSet(e))
      })
    } yield new PartitionedState(pc)

    lub(separate)
  }

  /**
   * Generates a list of deterministic combinations of a list of possibly non-
   * deterministic symbolic abstract values. An element of the resulting list
   * is a list of symbolic abstract values which all contain a single expression
   * mapping to a single state.
   *
   * @param xs The list of symbolic abstract values
   * @return The list of deterministic combinations of the argument
   */
  private[this] def combinations(xs: List[ExpressionSet]): List[List[ExpressionSet]] = xs match {
    case x::xs => (for (ex <- x.toSetOrFail; ps <- combinations(xs)) yield new ExpressionSet(x.typ).add(ex) :: ps).toList
    case Nil => Nil
  }

  /**
   * The least upper bound of several partitioned states. Assumes that <code>
   * lub(a,b) == lub(b, a)</code>.
   *
   * @param l The list of states
   * @return The least upper bound of the arguments
   */
  private[this] def lub(l: Iterable[PartitionedState[D]]): PartitionedState[D] = {
    (bottom /: l)((p1, p2) => p1.lub(p2))
  }


  /**
   * Dispatches a function to all active directives and returns the least upper
   * bound of the initial, or the argument if no directive is active.
   *
   * @param p The default initial
   * @param f The function
   * @return The default initial or the least upper bound of f applied to all
   * active directives
   */
  private[this] def dispatch(p: PartitionedState[D], f: (PartitionedStateObserver[D], Partitioning[D]) => Partitioning[D]): PartitionedState[D] = {
    if (activeDirectives.nonEmpty) {
      (bottom /: activeDirectives)((l, r) => l.lub(new PartitionedState[D](f(r, p.partitioning))))
    } else {
      p
    }
  }

  def removeObject(oldPreState: PartitionedState[D], obj: ExpressionSet, fields: Option[Set[Identifier]]): PartitionedState[D] = ???

  def refiningAssignField(oldPreState: PartitionedState[D], obj: ExpressionSet, field: String, right: ExpressionSet): PartitionedState[D] = ???

  def undoPruneVariables(unprunedPreState: PartitionedState[D], filter: (VariableIdentifier) => Boolean): PartitionedState[D] = ???

  def undoPruneUnreachableHeap(preState: PartitionedState[D]): PartitionedState[D] = ???

  override def ids = partitioning.states.map(_.ids).reduce(_ lub _)

}


/**
 * The trait for clients interested in events of the partitioned state.
 *
 * @param <D> The leaf type of the partitioning
 *
 * @author Dominik Gabi
 * @version 0.1
 */
trait PartitionedStateObserver[D <: State[D]] {

  def testTrue(p: Partitioning[D]): Partitioning[D] = p

  def testFalse(p: Partitioning[D]): Partitioning[D] = p

}




