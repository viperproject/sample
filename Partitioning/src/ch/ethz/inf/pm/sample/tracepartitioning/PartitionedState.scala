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
import ch.ethz.inf.pm.sample.SystemParameters
import sun.reflect.generics.reflectiveObjects.NotImplementedException


/**
 * Represents a partitioned state.
 *
 * @param <D> The leaf type
 *
 * @author Dominik Gabi
 * @version 0.1
 */
class PartitionedState[D <: State[D]] (val partitioning: Partitioning[D]) extends State[PartitionedState[D]] {
  require(partitioning != null)

  /**
   * Auxiliary constructor for a single state.
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
    getExpression.getSetOfExpressions.map(_.p).toList
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
   * @param l A partitioned state
   * @param r Another partitioned state
   * @return The least upper bound of the two arguments
   */
  override def lub(l: PartitionedState[D], r: PartitionedState[D]): PartitionedState[D] = {
    new PartitionedState(l.partitioning.lub(l.partitioning, r.partitioning))
  }

  /**
   * The greatest lower bound of two partitioned states operates on the pair-wise
   * on the structure of the partitioning and the leaf states.
   *
   * @param l A partitioned state
   * @param r Another partitioned state
   * @return The greatest lower bound of the two arguments
   */
  override def glb(l: PartitionedState[D], r: PartitionedState[D]) : PartitionedState[D] = {
    new PartitionedState(l.partitioning.glb(l.partitioning, r.partitioning))
  }

  /**
   * The actual widening is implemented in {@link #apply}. The directives
   * themselves are responsible for a more fine grained widening.
   * The widening here is just applied to the leaf states.
   *
   * @param l A partitioned state
   * @param r Another partitioned state
   * @return The least upper bound of the two arguments
   */
  override def widening(l: PartitionedState[D], r: PartitionedState[D]): PartitionedState[D] = {
    new PartitionedState[D](l.partitioning.widening(l.partitioning, r.partitioning))
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

  /**
   * Creates an object in all leaf states.
   *
   * @param t
   * @param pp
   * @return The state after creating the object in all leaves
   *
   * @see #map
   */
  override def createObject(t: Type, pp: ProgramPoint, fields : Option[Set[Identifier]] = None): PartitionedState[D] = {
    map(_.createObject(t, pp, fields))
  }

  /**
   * Creates an array in all leaf states.
   *
   * @param length
   * @param t
   * @param pp
   * @return The state after creating the array in all leaves
   *
   * @see #map
   */
  override def createArray(length : ExpressionSet, typ : Type, pp : ProgramPoint) : PartitionedState[D] = {
    mapValue(length, (s, v) => s.createArray(v, typ, pp))
  }

  /**
   * Creates the variable in all leaf states.
   *
   * @param x
   * @param t
   * @param pp
   * @return The lub of all partitioned states after creating the variable for
   * each expression in x in all leaves
   *
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
   *
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
   *
   * @see #mapValues
   */
  override def assignVariable(x: ExpressionSet, r: ExpressionSet): PartitionedState[D] = {
    mapValues(x, r, (s, ex, er) => s.assignVariable(ex, er))
  }

  /**
   * Assigns a field to a value in all leaf states.
   *
   * @param o
   * @param r
   * @return The lub of all partitioned states after assigning all possible
   * expressions to all possible combinations of fields in all leaves
   *
   * @see #mapValueLists
   */
  override def assignField(o: List[ExpressionSet], f: String, r: ExpressionSet) = {
    mapValueLists(o, List(r), (s, xs, ys) => s.assignField(xs, f, ys.head))
  }

  /**
   * Assigns an expression to an initial parameter in all leaf states.
   *
   * @param x
   * @param r
   * @return The lub of all partitioned states after assigning each parameter
   * in <code>r</code> to each variable in <code>x</code> in all leaves
   *
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
   *
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
   *
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
   *
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
   *
   * @see #map
   */
  override def getVariableValue(i: Assignable): PartitionedState[D] = {
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
   *
   * @see #mapValueList
   */
  override def getFieldValue(o: List[ExpressionSet], f: String, t: Type): PartitionedState[D] = {
    mapValueList(o, (s, v) => s.getFieldValue(v, f, t))
  }

  /**
   * Gets the length of an array from all leaf states.
   *
   * @param o
   * @return The lub of all partitioned states after mapping all combinations
   * of <code>o</code> and <code>i</code> on all leaves
   *
   * @see #mapValueLists
   */
  override def getArrayLength(o: ExpressionSet): PartitionedState[D] = {
    mapValue(o, (s, v) => s.getArrayLength(v))
  }

  /**
   * Gets the cell of an array in all leaf states.
   *
   * @param o
   * @param i
   * @param t
   * @return The lub of all partitioned states after mapping all combinations
   * of <code>o</code> and <code>i</code> on all leaves
   *
   * @see #mapValueLists
   */
  override def getArrayCell(o: ExpressionSet, i: ExpressionSet, t: Type): PartitionedState[D] = {
    mapValues(o, i, (s, vx, vr) => s.getArrayCell(vx, vr, t))
  }

  /**
   * Assign the cell of an array in all leaf states.
   *
   * @param o
   * @param i
   * @param r
   * @param t
   * @return The lub of all partitioned states after mapping all combinations
   * of <code>o</code> and <code>i</code> on all leaves
   *
   * @see #mapValueLists
   */
  override def assignArrayCell(o: ExpressionSet, i: ExpressionSet, r: ExpressionSet, t: Type): PartitionedState[D] = {
    mapValues(o, i, r, (s, vx, vi, vr) => s.assignArrayCell(vx, vi, vr, t))
  }

  /**
   * Backward semantics of variable access in all leaf states.
   *
   * @param i
   * @return The state before accessing the variables in all leaves
   *
   * @see #map
   */
  override def backwardGetVariableValue(i: Assignable): PartitionedState[D] = {
    map(_.backwardGetVariableValue(i))
  }

  /**
   * Backward semantics of field access in all leaf states.
   *
   * @param o
   * @param f
   * @param t
   * @return The state before accessing the field in all leaves
   *
   * @see #mapValueList
   */
  override def backwardGetFieldValue(o: List[ExpressionSet], f: String, t: Type): PartitionedState[D] = {
    mapValueList(o, (s, v) => s.backwardGetFieldValue(v, f, t))
  }

  /**
   * Backward semantics of variable assignment in all leaf states.
   *
   * @param x
   * @param r
   * @return The state before the assignment in all leaves
   *
   * @see #mapValues
   */
  override def backwardAssignVariable(x: ExpressionSet, r: ExpressionSet): PartitionedState[D] = {
    mapValues(x, r, (s, vx, vr) => s.backwardAssignVariable(vx, vr))
  }

  /**
   * Evaluates the numerical constant in all leaf states.
   *
   * @param v
   * @param t
   * @param pp
   * @return The state after evaluating the constant in all leaves
   *
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
   *
   * @see #mapValue
   */
  override def assume(c: ExpressionSet): PartitionedState[D] = {
    mapValue(c, (s, v) => s.assume(v))
  }

  /**
   * Assumes that the current expression holds in all leaf states.
   *
   * @return The state after assuming the current expression in all leaves
   *
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
   *
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
  override def getExpression: ExpressionSet = {
    var expr = new ExpressionSet(SystemParameters.typ.top())//TODO:Maybe I could be more precise
    for (s <- partitioning.states; e <- s.getExpression.getSetOfExpressions)
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

  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, keyCollectionTyp:Option[Type], tpp: ProgramPoint, fields : Option[Set[Identifier]] = None) = {
    throw new NotImplementedException()
  }

  def getSummaryCollectionIfExists(collectionSet: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def getCollectionValue(valueIds: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def insertCollectionTopElement(collectionSet: ExpressionSet, keyTop: ExpressionSet, valueTop: ExpressionSet, pp: ProgramPoint) = {
    throw new NotImplementedException()
  }

  def collectionContainsKey(collectionSet: ExpressionSet, keySet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint) = {
    throw new NotImplementedException()
  }

  def collectionContainsValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint) = {
    throw new NotImplementedException()
  }

  def getCollectionKeyByKey(collectionSet: ExpressionSet, keySet: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def getCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def extractCollectionKeys(fromCollectionSet: ExpressionSet, newKeyValueSet: ExpressionSet, fromCollectionTyp:Type, collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint) = {
    throw new NotImplementedException()
  }

  def getOriginalCollection(collectionSet: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def getKeysCollection(collectionSet: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def removeCollectionKeyConnection(origCollectionSet: ExpressionSet, keyCollectionSet: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def copyCollection(fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def insertCollectionElement(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet, pp: ProgramPoint) = {
    throw new NotImplementedException()
  }

  def removeCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def removeFirstCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def assignAllCollectionKeys(collectionSet: ExpressionSet, valueSet: ExpressionSet) = {
    throw new NotImplementedException()
  }

  def clearCollection(collectionSet: ExpressionSet) : PartitionedState[D] = {
    throw new NotImplementedException()
  }

  def getCollectionLength(collectionSet: ExpressionSet): PartitionedState[D] = {
    throw new NotImplementedException()
  }

  def isSummaryCollection(collectionSet: ExpressionSet): Boolean = {
    throw new NotImplementedException()
  }

  def pruneVariables(filter:Identifier => Boolean) : PartitionedState[D] = {
    throw new NotImplementedException()
  }

  def pruneUnreachableHeap(): PartitionedState[D] = {
    throw new NotImplementedException()
  }

  def optimizeSummaryNodes(): PartitionedState[D] = {
    throw new NotImplementedException()
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
      ex <- x.getSetOfExpressions
      val px = this.partitioning
      val pc = partitioning.zipmap(px, (s1: D, s2: D) => f(s1, new ExpressionSet(x.getType()).add(ex)))
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
   *
   * @see #mapValue
   */
  private[this] def mapValues(x: ExpressionSet, y: ExpressionSet, f: (D, ExpressionSet, ExpressionSet) => D): PartitionedState[D] = {
    val separate = for {
      ex <- x.getSetOfExpressions
      ey <- y.getSetOfExpressions
      val px = this.partitioning
      val py = this.partitioning
      val pc = partitioning.zipmap(List(px, py), (s: D, ss: List[D]) => f(s, new ExpressionSet(x.getType()).add(ex), new ExpressionSet(y.getType()).add(ey)))
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
   *
   * @see #mapValue
   */
  private[this] def mapValues(x: ExpressionSet, y: ExpressionSet, z: ExpressionSet, f: (D, ExpressionSet, ExpressionSet, ExpressionSet) => D): PartitionedState[D] = {
    val separate = for {
      ex <- x.getSetOfExpressions
      ey <- y.getSetOfExpressions
      ez <- z.getSetOfExpressions
      val px = this.partitioning
      val py = this.partitioning
      val pz = this.partitioning
      val pc = partitioning.zipmap(List(px, py, pz), (s: D, ss: List[D]) => f(s, new ExpressionSet(x.getType()).add(ex), new ExpressionSet(y.getType()).add(ey), new ExpressionSet(z.getType()).add(ez)))
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
      val es = cx.map(_.getSetOfExpressions.head)
      val ps = for ((e, v) <- es.zip(cx)) yield this.partitioning
      val pc = partitioning.zipmap(ps, (s: D, ss: List[D]) => f(s, for ((e, t) <- es.zip(ss)) yield new ExpressionSet(e.getType()).add(e)))
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
      val n = cx.length
      val es = cx.map(_.getSetOfExpressions.head):::cy.map(_.getSetOfExpressions.head)
      val ps = for ((e, v) <- es.zip(cx:::cy)) yield this.partitioning
      val pc = partitioning.zipmap(ps, (s: D, ss: List[D]) => {
        f(s,
          for ((e, t) <- es.zip(ss).take(n)) yield new ExpressionSet(e.getType()).add(e),
          for ((e, t) <- es.zip(ss).drop(n)) yield new ExpressionSet(e.getType()).add(e))
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
    case x::xs => (for (ex <- x.getSetOfExpressions; ps <- combinations(xs)) yield new ExpressionSet(x.getType()).add(ex) :: ps).toList
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
    (bottom /: l)((p1, p2) => lub(p1, p2))
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
    if (!activeDirectives.isEmpty) {
      (bottom /: activeDirectives)((l, r) => lub(l, new PartitionedState[D](f(r, p.partitioning))))
    } else {
      p
    }
  }
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




