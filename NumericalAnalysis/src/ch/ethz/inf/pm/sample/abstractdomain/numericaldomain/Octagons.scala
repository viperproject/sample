/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Octagons.{DoubleDbm, Environment, IntegerDbm, Interval}
import ch.ethz.inf.pm.sample.oorepresentation.Type

/** The common super trait of integer octagons and double octagons.
  *
  * @author Jerome Dohrau
  */
sealed trait Octagons[S <: Octagons[S]]
  extends NumericalDomain.Relational[S]
    with SimplifiedSemanticDomain[S]
    with SimplifiedMergeDomain[S] {
  this: S =>

  override def factory(): S = factory(Set.empty[Identifier])

  def factory(ids: Set[Identifier]): S = factory(Octagons.Environment(IdentifierSet.Inner(ids)))

  def factory(env: Octagons.Environment): S
}

object Octagons {

  /** The super trait for integer octagons and double octagons that are the top
    * element.
    *
    * @author Jerome Dohrau
    */
  sealed trait Top[S <: Octagons[S]]
    extends Octagons[S]
      with NumericalDomain.Relational.Top[S]
      with SimplifiedMergeDomain.Top[S]
      with BooleanExpressionSimplifier[S] {
    this: S =>

    override def assign(variable: Identifier, expr: Expression): S = this

    override def assumeSimplified(expression: Expression): S = this
  }

  /**
    * The super trait for integer octagons and double octagons that are the
    * bottom element.
    *
    * @author Jerome Dohrau
    */
  sealed trait Bottom[S <: Octagons[S]]
    extends Octagons[S]
      with NumericalDomain.Relational.Bottom[S]
      with SimplifiedMergeDomain.Bottom[S] {
    this: S =>

    override def createVariable(variable: Identifier, typ: Type): S =
      add(Set(variable))

    override def add(ids: Set[Identifier]): S =
      factory(ids)
  }

  /** The super trait for integer octagons and double octagons that are neither
    * the top element nor the bottom element.
    *
    * @author Jerome Dohrau
    */
  sealed trait Inner[S <: Octagons[S], T <: Dbm[T]]
    extends Octagons[S]
      with NumericalDomain.Relational.Inner[S, Inner[S, T]]
      with BooleanExpressionSimplifier[S] {
    this: S =>

    /** Returns the environment that holds all identifiers that are represented
      * in the DBM
      *
      * @return The environment.
      */
    def env: Environment

    /** Returns the underlying DBM. When an open and a closed DBM are present,
      * the closed version is returned.
      *
      * @return The underlying DBM.
      */
    def getDbm: T

    /** Returns the underlying closed DBM. If closed DBM is present, it is
      * computed from the open DBM.
      *
      * @return The underlying closed DBM.
      */
    def closedDbm: T

    /** Returns the underlying DBM. When an open and a closed DBM are present,
      * the open version is returned.
      *
      * @return
      */
    def openDbm: T

    /** Returns true if there is a closed version of the underlying DBM.
      *
      * @return True if there is a closed version of the underlying DBM.
      */
    def isClosed: Boolean

    /** Creates an element of the octagon domain.
      *
      * @param env    The environment containing the identifiers.
      * @param closed The optional closed DBM.
      * @param open   The optional open DBM.
      * @return The newly created element of the octagon domain.
      */
    def factory(env: Environment, closed: Option[T], open: Option[T]): S

    override def lubSameEnvInner(other: Inner[S, T]): S = {
      val newClosed = Some(closedDbm.lub(other.closedDbm))
      val newOpen = None
      factory(env, newClosed, newOpen)
    }

    override def glbSameEnvInner(other: Inner[S, T]): S = {
      val newDbm = Some(getDbm.glb(other.getDbm))
      val (newClosed, newOpen) = if (isClosed && other.isClosed) (newDbm, None) else (None, newDbm)
      factory(env, newClosed, newOpen)
    }

    override def wideningSameEnvInner(other: Inner[S, T]): S = {
      val newClosed = None
      val newOpen = Some(openDbm.widening(other.closedDbm))
      factory(env, newClosed, newOpen)
    }

    override def lessEqualSameEnvInner(other: Inner[S, T]): Boolean =
      closedDbm.lessThan(other.closedDbm)

    def epsilon: Interval

    override def assumeSimplified(expression: Expression): S = expression match {
      case _: ReferenceComparisonExpression => this
      case _ =>
        val nonExisting = expression.ids.getNonTop.filterNot(exists)
        if (nonExisting.nonEmpty)
          createVariables(nonExisting).assume(expression)
        else expression match {
          case BinaryArithmeticExpression(lhs, rhs, op) =>
            val left = normalize(lhs)
            val right = normalize(rhs)
            op match {
              case ArithmeticOperator.== => assumeNormalized(left - right) glb assumeNormalized(right - left)
              case ArithmeticOperator.!= => assumeNormalized(left - right + epsilon) lub assumeNormalized(right - left + epsilon)
              case ArithmeticOperator.<= => assumeNormalized(left - right)
              case ArithmeticOperator.< => assumeNormalized(left - right + epsilon)
              case ArithmeticOperator.>= => assumeNormalized(right - left)
              case ArithmeticOperator.> => assumeNormalized(right - left + epsilon)
            }
          case _ => throw new IllegalArgumentException("The argument is expected to be a comparison.")
        }
    }

    /** Assumes that the value represented by the given normalized expression
      * is at most zero.
      *
      * @param normalized The expression assumed to be at most zero.
      * @return THe state updated with the assumption.
      */
    def assumeNormalized(normalized: Normalized): S = {
      val Normalized(literals, interval) = normalized
      if (literals.isEmpty)
        if (interval.low <= 0) this else bottom()
      else {
        val indices: List[Int] = literals.map(env.getIndex)
        val dbm = closedDbm.clone()

        indices match {
          case i :: Nil => dbm.assume(i ^ 1, i, -2 * interval.low)
          case i :: j :: Nil => dbm.assume(j ^ 1, i, -interval.low)
          case _ =>
            val evaluated = literals.map(evaluate)
            val totalSum = evaluated.fold(Interval.Zero)(_ + _)
            for (i <- indices.indices; j <- i + 1 until indices.length) {
              val partialSum = totalSum - evaluated(i) - evaluated(j)
              dbm.assume(j ^ 1, i, -partialSum.low)
            }
        }
        factory(env, Some(dbm), None)
      }
    }

    /** A helper function that constructs a constant from the given value.
      */
    def makeConstant(value: Double): Expression

    /**
      * Assumption: the environments of this and that are disjoint.
      */
    override def unifyInner(that: Inner[S, T]): S = {
      val newEnv = env ++ that.env
      val newClosed = Some(getDbm.factory(newEnv.size)
        .copy(getDbm, env.getIndices(env.ids), newEnv.getIndices(env.ids))
        .copy(that.getDbm, that.env.getIndices(that.env.ids), newEnv.getIndices(that.env.ids))
        .close())
      val newOpen = None
      factory(newEnv, newClosed, newOpen)
    }

    override def expand(idA: Identifier, idsB: Set[Identifier]): S = {
      if (numerical(idA)) {
        if (exists(idA)) {
          val newIds = idsB.filter(numerical).toList
          val oldIds = (env.set.toSet -- newIds).toList
          val newEnv = env - idA ++ newIds.toSet
          val newIndices = newEnv.getIndices(newIds)
          val from = env.getIndices(oldIds) ++ List.fill(newIds.size)(env.getPositive(idA))
          val to = newEnv.getIndices(oldIds) ++ newIndices
          val newDbm = Some {
            var res = getDbm.factory(newEnv.size).copy(getDbm, from, to)
            // make sure the newly created variables are independent
            for (i <- newIndices; j <- newIndices if i < j) {
              res = res.assignRelational(i, j, Interval.Top)
              if (isClosed) res.close(i, j)
            }
            res
          }
          val (newClosed, newOpen) = if (isClosed) (newDbm, None) else (None, newDbm)
          factory(newEnv, newClosed, newOpen)
        } else add(idsB)
      } else this
    }

    override def rename(idA: Identifier, idB: Identifier): S = {
      if (numerical(idA) && numerical(idB)) {
        if (exists(idA) && !exists(idB)) {
          val newEnv = env - idA + idB
          val commonIds = (env.set.getNonTop - idA).toList
          val from = env.getPositive(idA) :: env.getIndices(commonIds)
          val to = newEnv.getPositive(idB) :: newEnv.getIndices(commonIds)
          copy(newEnv, from, to)
        } else remove(Set(idA)).add(Set(idB))
      } else this
    }

    override def fold(idsA: Set[Identifier], idB: Identifier): S = {
      if (numerical(idB)) {
        val ids = idsA.filter(exists).filter(numerical)
        if (!exists(idB) && ids.nonEmpty) {
          val newEnv = env -- idsA + idB
          val commonIds = (env.set.getNonTop -- idsA).toList
          val from = env.getIndices(commonIds) ++ env.getIndices(idsA.toList)
          val to = newEnv.getIndices(commonIds) ++ List.fill(idsA.size)(newEnv.getPositive(idB))
          val newClosed = Some(getDbm.factory(newEnv.size).maxCopy(closedDbm, from, to))
          val newOpen = None
          factory(newEnv, newClosed, newOpen)
        } else remove(ids).add(Set(idB))
      } else this
    }

    override def removeVariable(id: Identifier): S =
      remove(Set(id))

    override def createVariable(variable: Identifier, typ: Type): S =
      add(Set(variable))

    override def add(ids: Set[Identifier]): S = {
      val diff = ids.filterNot(exists).filter(numerical)
      if (diff.nonEmpty) {
        val newEnv = Environment(env.set ++ ids)
        copy(newEnv, env.getIndices(env.ids), newEnv.getIndices(env.ids))
      } else this
    }

    override def remove(ids: IdentifierSet): S = ids match {
      case IdentifierSet.Bottom => this
      case IdentifierSet.Top => factory(Set.empty[Identifier])
      case IdentifierSet.Inner(value) => remove(value)
    }

    override def remove(ids: Set[Identifier]): S = {
      val diff = ids.filter(exists).filter(numerical)
      if (diff.nonEmpty) {
        val newEnv = Environment(env.set -- diff)
        copy(newEnv, env.getIndices(newEnv.ids), newEnv.getIndices(newEnv.ids))
      } else this
    }

    private def exists(id: Identifier): Boolean = env.set.contains(id)

    private def numerical(id: Identifier): Boolean = id.typ.isNumericalType

    override def setToTop(variable: Identifier): S = {
      if (numerical(variable)) {
        if (exists(variable)) {
          val newClosed = Some(closedDbm.clone().assignTop(env.getPositive(variable)))
          val newOpen = None
          factory(env, newClosed, newOpen)
        } else add(Set(variable)).setToTop(variable)
      } else this
    }

    override def assign(variable: Identifier, expr: Expression): S = {
      if (numerical(variable)) {
        if (expr.ids.getNonTop.exists(!numerical(_))) {
          setToTop(variable)
        } else {
          val nonExisting = (expr.ids.getNonTop + variable).filterNot(exists)
          if (nonExisting.nonEmpty) {
            createVariables(nonExisting).assign(variable, expr)
          } else {
            val index = env.getPositive(variable)
            val Normalized(literals, interval) = normalize(expr)

            val res = literals match {
              case Nil =>
                val dbm = closedDbm.clone().assignConstant(index, interval)
                factory(env, Some(dbm), None)
              case List(Positive(id)) =>
                val dbm = closedDbm.clone()
                if (id == variable) dbm.assignAddition(index, interval)
                else dbm.assignRelational(index, env.getPositive(id), interval)
                factory(env, Some(dbm), None)
              case List(Negative(id)) =>
                val dbm = closedDbm.clone()
                if (id != variable) dbm.assignRelational(index, env.getPositive(id), Interval.Zero)
                dbm.assignNegate(index)
                if (interval != Interval.Zero) dbm.assignAddition(index, interval)
                factory(env, Some(dbm), None)
              case _ =>
                val value = evaluate(Normalized(literals, interval))
                val dbm = closedDbm.clone().assignConstant(index, value)
                factory(env, Some(dbm), None)
              /*case _ => setToTop(variable) match {
                case oct: Inner => {
                  val lhs = Normalized(List(Positive(variable)), Interval.Zero)
                  val rhs = Normalized(literals, interval)
                  oct.assumeNormalized(lhs - rhs) glb oct.assumeNormalized(rhs - lhs)
                }
                case _ => _
              }*/
            }
            if (SystemParameters.DEBUG) assert(!res.isBottom)
            res
          }
        }
      } else this
    }

    override def ids: IdentifierSet = env.set

    // NORMALIZE

    /** Computes a normalized expression from an expression.
      *
      * @param expr The expression to normalize.
      * @return The resulting normalized expression.
      */
    def normalize(expr: Expression): Normalized = expr match {
      case const: Constant => Normalized(Nil, evaluate(const))
      case id: Identifier => Normalized(List(Positive(id)), Interval.Zero)
      case UnaryArithmeticExpression(arg, ArithmeticOperator.+, _) => normalize(arg)
      case UnaryArithmeticExpression(arg, ArithmeticOperator.-, _) => -normalize(arg)
      case BinaryArithmeticExpression(lhs, rhs, ArithmeticOperator.+) => normalize(lhs) + normalize(rhs)
      case BinaryArithmeticExpression(lhs, rhs, ArithmeticOperator.-) => normalize(lhs) - normalize(rhs)
      case _ => Normalized(Nil, evaluate(expr))
    }

    // EVALUATE

    /** Evaluates an expression into an interval.
      *
      * @param expr The expression to be evaluated.
      * @return The resulting interval.
      */
    def evaluate(expr: Expression): Interval = expr match {
      case Constant("true", _, _) => Interval.One
      case Constant("false", _, _) => Interval.Zero
      case Constant(value, _, _) => toInterval(value)
      case id: Identifier => evaluate(id)
      case UnaryArithmeticExpression(arg, ArithmeticOperator.+, _) => evaluate(arg)
      case UnaryArithmeticExpression(arg, ArithmeticOperator.-, _) => -evaluate(arg)
      case BinaryArithmeticExpression(lhs, rhs, op) =>
        val left = evaluate(lhs)
        val right = evaluate(rhs)
        op match {
          case ArithmeticOperator.+ => left + right
          case ArithmeticOperator.- => left - right
          case ArithmeticOperator.* => left * right
          case ArithmeticOperator./ => divide(left, right)
          case _ => Interval.Top
        }
      case _ => Interval.Top
    }

    def toInterval(s: String): Interval

    /** Evaluates a normalized expression into an interval.
      *
      * @param value The normalized expression to be evaluated.
      * @return The resulting interval.
      */
    def evaluate(value: Normalized): Interval =
      value.literals.map(evaluate).fold(value.interval)(_ + _)

    /** Evaluates a literal into an interval.
      *
      * @param literal The literal to be evaluated.
      * @return The resulting interval.
      */
    def evaluate(literal: Literal): Interval = literal match {
      case Positive(id) => evaluate(id)
      case Negative(id) => -evaluate(id)
    }

    /** Evaluates an identifier into an interval.
      *
      * @param id The identifier to be evaluated.
      * @return The resulting interval.
      */
    def evaluate(id: Identifier): Interval = getDbm.getBounds(env.getPositive(id))

    // STRING REPRESENTATION
    override def toString: String = s"Octagon(" +
      s"\n\tenvironment: ${env.ids}" +
      s"\n\tconstraints: \n\t\t${getConstraints(env.ids.toSet).mkString("\n\t\t")}" +
      s"\n)"

    // HELPERS

    override def getConstraints(ids: Set[Identifier]): Set[Expression] = {
      val dbm = getDbm

      // collect all constraints involving one variable
      val (constraints, relational) = ids.foldLeft((Set.empty[Expression], List.empty[Identifier])) {
        case ((currConstraints, currRelational), idA) =>
          val i = env.getIndex(idA)
          val bounds = dbm.getBounds(i)
          val newConstraints = currConstraints ++ makeConstraints(idA, bounds)
          val newRelational = if (bounds.low == bounds.high) currRelational else idA :: currRelational
          (newConstraints, newRelational)
      }

      val pairs = for {
        a <- relational
        b <- relational
        if env.getIndex(a) < env.getIndex(b)
      } yield (a, b)

      // collect all relational constraints that are not (trivially) implied by
      // the constraints we already have
      pairs.foldLeft(constraints) {
        case (currConstraints, (idA, idB)) =>
          val sumBounds = dbm.getBounds(env.getNegative(idB), env.getPositive(idA))
          val diffBounds = dbm.getBounds(env.getPositive(idB), env.getPositive(idA))
          val sum = BinaryArithmeticExpression(idA, idB, ArithmeticOperator.+)
          val diff = BinaryArithmeticExpression(idA, idB, ArithmeticOperator.-)
          currConstraints ++ makeConstraints(sum, sumBounds) ++ makeConstraints(diff, diffBounds)
      }
    }

    /** A helper function that constructs a set of constraints for an expression
      * that is bounded by the given interval.
      */
    private def makeConstraints(expression: Expression, bounds: Interval): Set[Expression] = {
      val Interval(low, high) = bounds
      if (expression.typ.isBooleanType) {
        if (low == high) {
          if (low == 0) Set(NegatedBooleanExpression(expression))
          else Set(expression)
        } else Set.empty
      } else {
        if (low == high) {
          // return an equality
          val constant = makeConstant(bounds.low)
          Set(BinaryArithmeticExpression(expression, constant, ArithmeticOperator.==))
        } else {
          // construct lower bound if it is not negative infinity
          val lower = if (bounds.low.isNegInfinity) None else {
            val constant = makeConstant(bounds.low)
            val inequality = BinaryArithmeticExpression(constant, expression, ArithmeticOperator.<=)
            Some(inequality)
          }
          // construct upper bound if it is not positive infinity
          val upper = if (bounds.high.isPosInfinity) None else {
            val constant = makeConstant(bounds.high)
            val inequality = BinaryArithmeticExpression(expression, constant, ArithmeticOperator.<=)
            Some(inequality)
          }
          // return constraints
          lower.toSet ++ upper.toSet
        }
      }
    }

    /** Divides the first interval by the second interval. This method is
      * abstract to allow different behavior for integer octagons and double
      * octagons.
      *
      * @param left  The first interval.
      * @param right The second interval.
      * @return The result of the division.
      */
    protected def divide(left: Interval, right: Interval): Interval

    protected def copy(newEnv: Environment, from: List[Int], to: List[Int]): S
  }

  /** Represents a positive or a negative occurrence of an identifier.
    *
    * @author Jerome Dohrau
    */
  sealed trait Literal {
    /** Returns the identifier.
      *
      * @return the identifier.
      */
    def id: Identifier

    /** Negates the literal.
      *
      * @return The negation of the literal.
      */
    def unary_-(): Literal = this match {
      case Positive(id) => Negative(id)
      case Negative(id) => Positive(id)
    }
  }

  /**
    * An difference bound matrix (DBM) d stores constraints for some set of
    * variables v_0, v_2, ..., v_{d-1}. For every variable v_i we introduce a
    * positive version x_{2*i} = +v_i and a negative version x_{2*i+1} = -v_i.
    * The octagon constraints are stored in a (2*d x 2*d)-Matrix. An entry
    * m_{i,j} = c in the matrix represents the constraint x_j - x_i <= c.
    *
    * Due to symmetry, it is enough to store the lower left elements of the
    * matrix. These elements are stored in a row-major order fashion. As an
    * example, the following figure illustrates the order of the elements of a
    * three dimensional octagon matrix in the array.
    *
    * +--+--+  +  +  +  +
    * | 0| 1|
    * +--+--+  +  +  +  +
    * | 2| 3|
    * +--+--+--+--+  +  +
    * | 4| 5| 6| 7|
    * +--+--+--+--+  +  +
    * | 8| 9|10|11|
    * +--+--+--+--+--+--+
    * |12|13|14|15|16|17|
    * +--+--+--+--+--+--+
    * |18|19|20|21|22|23|
    * +--+--+--+--+--+--+
    *
    * @author Jerome Dohrau
    */
  trait Dbm[S <: Dbm[S]] {
    this: S =>

    import Dbm._

    require(arr.length == size(dim))

    /** The dimension (i.e., the number of identifiers) of the DBM.
      *
      * @return The dimension of the DBM.
      */
    def dim: Int

    /** The internal matrix of the DBM.
      *
      * @return The internal matrix of the DBM.
      */
    def arr: Array[Double]

    /** Creates a new DBM where all identifiers are top.
      *
      * @param dim The dimension of the DBM.
      * @param arr The internal matrix of the DBM.
      * @return The newly created DBM.
      */
    def factory(dim: Int, arr: Array[Double]): S

    /** Creates a new DBM that corresponds to the top element.
      *
      * @param dim The dimension of the DBM.
      * @return The newly created DBM.
      */
    def factory(dim: Int): S =
      factory(dim, topArr(dim))

    def isTop: Boolean = {
      Range(0, 2 * dim).forall(r => (0 until r).forall(c => arr(index(r, c)) == Infinity))
    }

    def isBottom: Boolean = {
      // Due to rounding errors, we may get something slightly less than 0.
      // This is not nice but it works (restored from Jeromes previous version)
      Range(0, 2 * dim).exists(i => arr(index(i, i)) < -NumericalAnalysisConstants.epsilon)
    }

    def lub(other: S): S = {
      require(dim == other.dim)
      factory(dim, arr.zip(other.arr).map(p => math.max(p._1, p._2)))
    }

    def glb(other: S): S = {
      require(dim == other.dim)
      factory(dim, arr.zip(other.arr).map(p => math.min(p._1, p._2)))
    }

    def widening(other: S): S = {
      require(dim == other.dim)
      factory(dim, arr.zip(other.arr).map(p => if (p._1 >= p._2) p._1 else Infinity))
    }

    def lessThan(other: S): Boolean = {
      require(dim == other.dim)
      arr.zip(other.arr).forall(p => p._1 <= p._2)
    }

    def remove(index: Int): S = {
      factory(dim, arr.clone()).assignTop(index)
    }

    /**
      * Note: modifies the internal matrix
      */
    def assume(row: Int, col: Int, value: Double): S = {
      val rc = index(row, col)
      arr(rc) = math.min(arr(rc), value)
      close(row, col)
    }

    /**
      * @param i The index corresponding to the literal x_i.
      * @return An interval [low, high] such that low <= x_i <= high.
      */
    def getBounds(i: Int): Interval = {
      val low = -arr(lower(i, i ^ 1)) / 2
      val high = arr(lower(i ^ 1, i)) / 2
      Interval(low, high)
    }

    /**
      * @param i The index corresponding to the literal x_i.
      * @param j The index corresponding to the literal x_j.
      * @return An interval [low, high] such that low <= x_j - x_i <= high.
      */
    def getBounds(i: Int, j: Int): Interval = {
      val low = -getBound(j, i)
      val high = getBound(i, j)
      Interval(low, high)
    }

    def getBound(i: Int, j: Int): Double = arr(index(i, j))

    /**
      * v_{i/2} := [a, b]
      *
      * Note: modifies the internal matrix
      */
    def assignConstant(i: Int, const: Interval): S = {
      require(i % 2 == 0)

      assignTop(i)

      // set lower bound and upper bound
      val Interval(low, high) = const
      arr(lower(i, i ^ 1)) = -2 * low
      arr(lower(i ^ 1, i)) = 2 * high

      close(i, i)
      close(i ^ 1, i ^ 1)

      this
    }

    /**
      * x_i = x_i + [a, b]
      *
      * Note: modifies the internal matrix
      */
    def assignAddition(i: Int, const: Interval): S = {
      require(i % 2 == 0)

      val Interval(low, high) = const

      for (k <- 0 until i) {
        val pk = lower(i, k)
        val nk = lower(i ^ 1, k)
        arr(pk) = arr(pk) - low
        arr(nk) = arr(nk) + high
      }
      for (k <- i + 2 until 2 * dim) {
        val kp = lower(k, i)
        val kn = lower(k, i ^ 1)
        arr(kp) = arr(kp) + high
        arr(kn) = arr(kn) - low
      }

      val pn = lower(i, i ^ 1)
      val np = lower(i ^ 1, i)
      arr(pn) = arr(pn) - 2 * low
      arr(np) = arr(np) + 2 * high

      this
    }

    /**
      * x_i := x_j + [a, b]
      *
      * Note: modifies the internal matrix
      */
    def assignRelational(i: Int, j: Int, const: Interval): S = {
      require(i % 2 == 0)
      require(j % 2 == 0)

      assignTop(i)

      val Interval(low, high) = const
      arr(index(i, j)) = -low
      arr(index(i ^ 1, j ^ 1)) = high

      close(i, j)
      close(i ^ 1, j ^ 1)

      this
    }

    /**
      * Note: modifies the internal matrix
      */
    def assignTop(i: Int): S = {
      require(i % 2 == 0)

      // forget relations with other variables
      for (k <- 0 until i) {
        arr(lower(i, k)) = Infinity
        arr(lower(i ^ 1, k)) = Infinity
        arr(lower(i, k ^ 1)) = Infinity
        arr(lower(i ^ 1, k ^ 1)) = Infinity
      }
      for (k <- i + 2 until 2 * dim) {
        arr(lower(k ^ 1, i ^ 1)) = Infinity
        arr(lower(k ^ 1, i)) = Infinity
        arr(lower(k, i ^ 1)) = Infinity
        arr(lower(k, i)) = Infinity
      }

      // forget conflicting constraints involving v_i
      arr(lower(i, i)) = 0.0
      arr(lower(i ^ 1, i ^ 1)) = 0.0

      // set lower bound to a and upper bound to b
      arr(lower(i, i ^ 1)) = Infinity
      arr(lower(i ^ 1, i)) = Infinity

      this
    }

    /**
      * x_i := - x_i
      *
      * Note: modifies the internal matrix
      */
    def assignNegate(i: Int): S = {
      require(i % 2 == 0)

      // swap positive and negative row
      for (r <- 0 until i + 2) {
        // compute indices
        val irp = lower(i, r)
        val irn = lower(i ^ 1, r)
        // swap elements
        val temp = arr(irp)
        arr(irp) = arr(irn)
        arr(irn) = temp
      }

      // swap positive and negative column
      for (r <- i until 2 * dim) {
        // compute indices
        val rip = lower(r, i)
        val rin = lower(r, i ^ 1)
        // swap elements
        val temp = arr(rip)
        arr(rip) = arr(rin)
        arr(rin) = temp
      }

      this
    }

    /** Computes the closure of the DBM. This method is abstract to allow
      * different implementations for integer DBMs and double DBMs.
      *
      * Note: modifies the internal matrix
      *
      * @return This DBM.
      */
    def close(): S

    /** Computes the closure of the DBM at the given row and column. This method
      * is abstract to allow different implementations for integer DBMs and
      * double DBMs.
      *
      * Note: modifies the internal matrix
      *
      * @param row The row index.
      * @param col The column index.
      * @return This DBM.
      */
    def close(row: Int, col: Int): S

    /** Note: modifies the internal matrix
      */
    def copy(other: S, from: List[Int], to: List[Int]): S = {
      require(from.length == to.length)
      require(from.forall(i => i % 2 == 0))
      require(to.forall(i => i % 2 == 0))

      val map = from.zip(to)

      for ((fr, tr) <- map; (fc, tc) <- map) {
        if (tr >= tc) {
          arr(lower(tr, tc)) = other.arr(index(fr, fc))
          arr(lower(tr ^ 1, tc)) = other.arr(index(fr ^ 1, fc))
          arr(lower(tr, tc ^ 1)) = other.arr(index(fr, fc ^ 1))
          arr(lower(tr ^ 1, tc ^ 1)) = other.arr(index(fr ^ 1, fc ^ 1))
        }
      }

      this
    }

    /** This method is equivalent to first copying the rows/columns and then
      * computing the least upper bound with the old DBM.
      *
      * Note: modifies the internal matrix
      *
      * @param other the DBM to copy the rows/columns from
      * @param from  the list of row/column indices to copy from
      * @param to    the list of row/column indices to copy to
      * @return This DBM with the updates.
      */
    def maxCopy(other: S, from: List[Int], to: List[Int]): S = {
      require(from.length == to.length)
      require(from.forall(i => i % 2 == 0))
      require(to.forall(i => i % 2 == 0))

      val map = from.zip(to)
      for ((fr, tr) <- map; (fc, tc) <- map) {
        if (tr >= tc) {
          val pp = lower(tr, tc)
          val np = lower(tr ^ 1, tc)
          val pn = lower(tr, tc ^ 1)
          val nn = lower(tr ^ 1, tc ^ 1)
          arr(pp) = math.max(arr(pp), other.arr(index(fr, fc)))
          arr(np) = math.max(arr(np), other.arr(index(fr ^ 1, fc)))
          arr(pn) = math.max(arr(pn), other.arr(index(fr, fc ^ 1)))
          arr(nn) = math.max(arr(nn), other.arr(index(fr ^ 1, fc ^ 1)))
        }
      }

      this
    }

    override def clone(): S =
      factory(dim, arr.clone())

    override def toString: String =
      (0 until 2 * dim)
        .map(row => (0 until 2 * dim)
          .map(col => index(row, col))
          .map(idx => arr(idx))
          .map(num => if (num == Infinity) "  ." else f"${num.toInt}%3d")
          .reduce(_ + " " + _))
        .reduce(_ + "\n" + _)
  }

  /** A DBM for integer constraints.
    *
    * @param dim The dimension of the DBM.
    * @param arr The internal matrix of the DBM.
    * @author Jerome Dohrau
    */
  case class IntegerDbm(dim: Int, arr: Array[Double])
    extends Dbm[IntegerDbm] {

    import Dbm.{index, lower}

    override def factory(dim: Int, arr: Array[Double]): IntegerDbm =
      IntegerDbm(dim, arr)

    override def close(): IntegerDbm = {
      for (row <- 0 until dim; col <- row until dim) {
        close(2 * row, 2 * col)
      }

      this
    }

    override def close(row: Int, col: Int): IntegerDbm = {
      // compute indices involving r and c
      val rc = index(row, col)
      val rr = index(row ^ 1, row)
      val cc = index(col, col ^ 1)
      // loop through rows
      for (i <- 0 until 2 * dim) {
        // compute indices involving i
        val ir = index(i, row)
        val ic = index(i, col ^ 1)
        val ci = index(col, i ^ 1)
        // loop through columns
        for (j <- 0 until (i / 2 + 1) * 2) {
          // compute indices involving j
          val ij = lower(i, j)
          val cj = index(col, j)
          val rj = index(row ^ 1, j)
          // first update step
          if (i == (j ^ 1)) {
            arr(ij) = math.min(arr(ij), 2 * arr(rc) + 2 * arr(ic) + arr(rr))
            arr(ij) = math.min(arr(ij), 2 * arr(rc) + 2 * arr(ir) + arr(cc))
            arr(ij) = math.min(arr(ij), 2 * math.floor((arr(ir) + arr(rc) + arr(ci)) / 2))
          } else {
            arr(ij) = math.min(arr(ij), arr(ir) + arr(rc) + arr(cj))
            arr(ij) = math.min(arr(ij), arr(ic) + arr(rc) + arr(rj))
          }
        }
      }

      // loop through rows and columns
      for (i <- 0 until 2 * dim; j <- 0 until (i / 2 + 1) * 2) {
        // compute indices involving i and j
        val ij = lower(i, j)
        val ii = lower(i, i ^ 1)
        val jj = lower(j ^ 1, j)
        // second update step
        arr(ij) = math.min(arr(ij), (arr(ii) + arr(jj)) / 2)
      }

      this
    }

  }

  /** A DBM for non-integer constraints.
    *
    * @param dim The dimension of the DBM.
    * @param arr The internal matrix of the DBM.
    * @author Jerome Dohrau
    */
  case class DoubleDbm(dim: Int, arr: Array[Double])
    extends Dbm[DoubleDbm] {

    import Dbm.{index, lower}

    override def factory(dim: Int, arr: Array[Double]): DoubleDbm =
      DoubleDbm(dim, arr)

    override def close(): DoubleDbm = {
      for (row <- 0 until dim; col <- row until dim) {
        close(2 * row, 2 * col)
      }

      this
    }

    override def close(row: Int, col: Int): DoubleDbm = {
      // compute indices involving r and c
      val rc = index(row, col)
      val rr = index(row ^ 1, row)
      val cc = index(col, col ^ 1)
      // loop through rows
      for (i <- 0 until 2 * dim) {
        // compute indices involving i
        val ir = index(i, row)
        val ic = index(i, col ^ 1)
        val ci = index(col, i ^ 1)
        // loop through columns
        for (j <- 0 until (i / 2 + 1) * 2) {
          // compute indices involving j
          val ij = lower(i, j)
          val cj = index(col, j)
          val rj = index(row ^ 1, j)
          // first update step
          if (i == (j ^ 1)) {
            arr(ij) = math.min(arr(ij), 2 * arr(rc) + 2 * arr(ic) + arr(rr))
            arr(ij) = math.min(arr(ij), 2 * arr(rc) + 2 * arr(ir) + arr(cc))
            arr(ij) = math.min(arr(ij), arr(ir) + arr(rc) + arr(ci))
          } else {
            arr(ij) = math.min(arr(ij), arr(ir) + arr(rc) + arr(cj))
            arr(ij) = math.min(arr(ij), arr(ic) + arr(rc) + arr(rj))
          }
        }
      }

      // loop through rows and columns
      for (i <- 0 until 2 * dim; j <- 0 until (i / 2 + 1) * 2) {
        // compute indices involving i and j
        val ij = lower(i, j)
        val ii = lower(i, i ^ 1)
        val jj = lower(j ^ 1, j)
        // second update step
        arr(ij) = math.min(arr(ij), (arr(ii) + arr(jj)) / 2)
      }

      this
    }

    /** A helper function that constructs a constant from the given value.
      */
    def makeConstant(value: Double): Expression =
      Constant(value.toString, SystemParameters.tm.Int)
  }

  /**
    * Helper class that lexicographically sorts the identifiers of an identifier
    * in order to any given identifier to the corresponding row [column] index
    * in the DBM.
    *
    * @author Jerome Dohrau
    */
  case class Environment(set: IdentifierSet) {
    /**
      * The sorted list of identifiers.
      */
    lazy val ids: List[Identifier] = set.getNonTop.toList.sortBy(_.getName)

    /**
      * The map mapping from identifiers to their corresponding row [column]
      * index in the DBM.
      */
    private lazy val map = ids.zipWithIndex.toMap

    /** Returns true if the environment represents the top element.
      *
      * @return True if the environment represents the top element
      */
    def isTop: Boolean = set.isTop

    /** Returns true if the environment represents the bottom element.
      *
      * @return True if the environment represents the bottom element.
      */
    def isBottom: Boolean = set.isBottom

    def size: Int = ids.length

    /** Returns the index in the DBM of the given literal.
      *
      * @param literal The literal.
      * @return THe index in the DBM of the given literal.
      */
    def getIndex(literal: Literal): Int =
      literal match {
        case Positive(id) => getPositive(id)
        case Negative(id) => getNegative(id)
      }

    /** Returns the index in the DBM of the negative occurrence of the given
      * identifier.
      *
      * @param id The identifier.
      * @return The index in the DBM of the negative occurrence of the given
      *         identifier.
      */
    def getNegative(id: Identifier): Int =
      2 * map(id) + 1

    /** Returns the index in the DBM of the positive occurrence of the given
      * identifier.
      *
      * @param id The identifier.
      * @return The index in the DBM of the positive occurrence of the given
      *         identifier.
      */
    def getPositive(id: Identifier): Int =
      2 * map(id)

    /** Returns the list of indices in the DBM for the given list of
      * identifiers.
      *
      * @param ids The list opf identifiers.
      * @return The list of indices in the DBM for the given list of
      *         identifiers.
      */
    def getIndices(ids: List[Identifier]): List[Int] = ids.map(getIndex)

    /** Returns the index in the DBM of (positive occurrence) the given
      * identifier.
      *
      * @param id The identifier.
      * @return The index in the DBM of the given identifier.
      */
    def getIndex(id: Identifier): Int =
      getPositive(id)

    // OPERATORS

    def ++(other: Environment): Environment = this ++ other.set

    def ++(ids: IdentifierSet): Environment = Environment(set ++ ids)

    def ++(ids: Set[Identifier]): Environment = this ++ IdentifierSet.Inner(ids)

    def --(other: Environment): Environment = this -- other.set

    def --(ids: Set[Identifier]): Environment = this -- IdentifierSet.Inner(ids)

    def --(ids: IdentifierSet): Environment = Environment(set -- ids)

    def +(id: Identifier): Environment = Environment(set + id)

    def -(id: Identifier): Environment = Environment(set - id)
  }

  /** Represents a normalized expression. A normalized expression is of the form
    * l_1 + ... + l_k + [a, b], where l_i are literals.
    */
  case class Normalized(literals: List[Literal], interval: Interval) {
    // For higher precision we want that all positive and negative occurrences
    // of literals are cancelled out.
    if (SystemParameters.DEBUG) assert(literals == cancel(literals))

    /** Subtracts the given other normalized expression from this normalized
      * expression.
      *
      * @param other THe normalized expression to be subtracted.
      * @return The difference between this and the other normalized expression.
      */
    def -(other: Normalized) = this + -other

    def unary_-(): Normalized = Normalized(literals.map(x => -x), -interval)

    /** Adds the given other normalized expression to this normalized
      * expression.
      *
      * @param other The normalized expression to be added.
      * @return The sum of this and the other normalized expression.
      */
    def +(other: Normalized) = Normalized(cancel(literals ++ other.literals), interval + other.interval)

    /** A helper function that cancels out positive and negative occurrences of
      * literals.
      *
      * @param literals The literals to cancel out.
      * @return The given literals with all positive and negative occurrences
      *         cancelled out.
      */
    private def cancel(literals: List[Literal]): List[Literal] = {
      literals.foldLeft(Map.empty[Identifier, List[Literal]]) {
        (map, lit) =>
          map.get(lit.id) match {
            case Some(Nil) | None => map + (lit.id -> (lit :: Nil))
            case Some(x :: xs) => (lit, x) match {
              case (Positive(_), Positive(_)) |
                   (Negative(_), Negative(_)) =>
                // The literals have the same sign and do not cancel out
                map + (lit.id -> (lit :: x :: xs))
              case (Negative(_), Positive(_)) |
                   (Positive(_), Negative(_)) =>
                // The literals have different signs and cancel out
                map + (lit.id -> xs)
            }
          }
      }.values.toList.flatten
    }

    /** Adds the given interval to this normalized expression.
      *
      * @param value The interval to be added.
      * @return The normalized expression after the given interval is added.
      */
    def +(value: Interval) = Normalized(literals, interval + value)

    /** Subtracts the given interval from this normalized expression.
      *
      * @param value The interval to be subtracted.
      * @return The normalized expression after the given interval is
      *         subtracted.
      */
    def -(value: Interval) = Normalized(literals, interval - value)
  }

  /** Represents a positive occurrence of an identifier.
    *
    * @param id The identifier.
    * @author Jerome Dohrau
    */
  case class Positive(id: Identifier) extends Literal

  /** Represents a negative occurrence of an identifier.
    *
    * @param id The identifier.
    * @author Jerome Dohrau
    */
  case class Negative(id: Identifier) extends Literal

  /** Represents an interval.
    *
    * @param low  The lower bound of the interval.
    * @param high The upper bound of the interval.
    * @author Jerome Dohrau
    */
  case class Interval(low: Double, high: Double) {
    /** Negates the interval.
      *
      * @return The negation of the interval.
      */
    def unary_-(): Interval = Interval(-high, -low)

    /** Adds the given other interval to this interval.
      *
      * @param other The interval to be added.
      * @return The sum of this and the other interval.
      */
    def +(other: Interval): Interval = Interval(low + other.low, high + other.high)

    /** Subtracts the given other interval from this interval.
      *
      * @param other The interval to be subtracted.
      * @return The difference between this and the other interval.
      */
    def -(other: Interval): Interval = Interval(low - other.high, high - other.low)

    /** Multiplies the given other interval with this interval.
      *
      * @param other The interval to multiply with.
      * @return The product of this and the other interval.
      */
    def *(other: Interval): Interval = {
      val a = low * other.low
      val b = low * other.high
      val c = high * other.low
      val d = high * other.high
      Interval(min(a, b, c, d), max(a, b, c, d))
    }

    /** A helper function that returns the minimum of the four given numbers.
      *
      * @param a The first number.
      * @param b The second number.
      * @param c The third number.
      * @param d The fourth number.
      * @return The minimum of the four given numbers.
      */
    private def min(a: Double, b: Double, c: Double, d: Double): Double =
      math.min(math.min(a, b), math.min(c, d))

    /** A helper function that returns the maximum of the four given numbers.
      *
      * @param a The first number.
      * @param b The second number.
      * @param c The third number.
      * @param d The fourth number.
      * @return The maximum of the four given numbers.
      */
    private def max(a: Double, b: Double, c: Double, d: Double): Double =
      math.max(math.max(a, b), math.max(c, d))

    /** Divides this interval by the given other interval.
      *
      * @param other The interval to divide by.
      * @return The quotient of this and the other interval.
      */
    def /(other: Interval): Interval = {
      if (other.low < 0.0 && 0.0 < other.high) {
        val Interval(a, b) = this / Interval(other.low, 0.0)
        val Interval(c, d) = this / Interval(0.0, other.high)
        Interval(math.min(a, c), math.max(c, d))
      } else {
        val a = low / other.low
        val b = low / other.high
        val c = high / other.low
        val d = high / other.high
        Interval(min(a, b, c, d), max(a, b, c, d))
      }
    }

    /** Truncates the bounds of the interval to integer values. This function is
      * used to mimic the behavior of an integer division.
      *
      * @return The interval with its bounds truncated to integer values.
      */
    def truncate(): Interval =
      Interval(truncate(low), truncate(high))

    /** A helper function that truncates a number to an integer value. This
      * function is used to mimic the behavior of an integer division.
      *
      * @param a The number to be truncated to an integer value.
      * @return The given number truncated to an integer value.
      */
    private def truncate(a: Double): Double =
      if (a >= 0) math.floor(a) else math.ceil(a)
  }

  object Dbm {
    /** Computes the index of a matrix element with the specified row and column
      * indices in the array.
      *
      * @param row the row index of the element
      * @param col the column index of the element
      * @return the index of the matrix element in the array
      */
    def index(row: Int, col: Int): Int = if (row < col) lower(col ^ 1, row ^ 1) else lower(row, col)

    /** Computes the index of a lower left matrix element with the specified row
      * and column indices in the array.
      *
      * Note: A lower left matrix element is a matrix element where the row and
      * column indices satisfy the inequality row/2 >= col/2.
      *
      * @param row the row index of the matrix element
      * @param col the column index of the matrix element
      * @return the index of the matrix element in the array
      */
    def lower(row: Int, col: Int): Int = (row + 1) * (row + 1) / 2 + col

    /** Returns the internal matrix of a DBM with the given dimension where all
      * identifiers are top.
      *
      * @param dim The dimension.
      * @return The internal matrix of the DBM.
      */
    def topArr(dim: Int) = {
      val arr = Array.fill(size(dim))(Infinity)
      for (i <- 0 until 2 * dim) {
        arr(lower(i, i)) = 0.0
      }
      arr
    }

    /** A shorter name for positive infinity.
      *
      * @return The double representing positive infinity.
      */
    def Infinity: Double = Double.PositiveInfinity

    /** Computes the number of matrix elements in an DBM with the
      * specified dimension.
      *
      * @param dim The dimension of the DBM.
      * @return The number of matrix elements in the DBM.
      */
    def size(dim: Int): Int = 2 * dim * (dim + 1)
  }

  object Interval {
    /** Returns the interval from negative infinity to positive infinity.
      *
      * @return The interval from negative infinity to positive infinity.
      */
    def Top = Interval(Double.NegativeInfinity, Double.PositiveInfinity)

    /** Returns the interval containing zero.
      *
      * @return The interval containing zero.
      */
    def Zero = Interval(0.0, 0.0)

    /** Returns the interval containing one.
      *
      * @return The interval containing one.
      */
    def One = Interval(1.0, 1.0)


    def Epsilon = Interval(NumericalAnalysisConstants.epsilon, NumericalAnalysisConstants.epsilon)

  }

}

/** An element of the integer octagon domain.
  *
  * @author Jerome Dohrau
  */
sealed trait IntegerOctagons
  extends Octagons[IntegerOctagons] {

  import Octagons.Dbm.topArr

  override def factory(env: Environment): IntegerOctagons =
    if (env.isTop) top()
    else IntegerOctagons.Inner(env, Some(IntegerDbm(env.size, topArr(env.size))), None)

  override def top(): IntegerOctagons = IntegerOctagons.Top

  override def bottom(): IntegerOctagons = IntegerOctagons.Bottom
}

object IntegerOctagons {

  /** An element of the integer octagon domain that is neither the top element
    * nor the bottom element.
    *
    * Internally the constraints are stored in a DBM. There is an optional
    * closed DBM and an optional open DBM; however, at least one of them has to
    * be defined.
    *
    * @param env    The environment containing the identifiers.
    * @param closed The optional closed DBM.
    * @param open   The optional open DBM.
    * @author Jerome Dohrau
    */
  case class Inner(env: Environment,
                   var closed: Option[IntegerDbm],
                   open: Option[IntegerDbm])
    extends IntegerOctagons
      with Octagons.Inner[IntegerOctagons, IntegerDbm] {

    /** Returns the underlying closed DBM. If closed DBM is present, it is
      * computed from the open DBM.
      *
      * @return The underlying closed DBM.
      */
    override def closedDbm: IntegerDbm = {
      if (closed.isEmpty) closed = open.map(_.clone().close())
      closed.get
    }

    require(closed.isDefined || open.isDefined)

    /** Returns the underlying DBM. When an open and a closed DBM are present,
      * the open version is returned.
      *
      * @return
      */
    override def openDbm: IntegerDbm =
      open.orElse(closed).get

    /** Returns true if there is a closed version of the underlying DBM.
      *
      * @return True if there is a closed version of the underlying DBM.
      */
    override def isClosed: Boolean =
      closed.isDefined

    override def epsilon = Interval.One

    override def toInterval(value: String) = Interval(value.toInt, value.toInt)

    override def getStringOfId(id: Identifier): String = {
      val bounds = getDbm.getBounds(env.getIndex(id))
      if (bounds.low.isNegInfinity && bounds.high.isPosInfinity) "Top"
      else if (bounds.low.isNegInfinity) s"$id<=${bounds.high.toInt}"
      else if (bounds.high.isPosInfinity) s"${bounds.low.toInt}<=$id"
      else if (bounds.low == bounds.high) s"$id==${bounds.low.toInt}"
      else s"${bounds.low.toInt}<=$id<=${bounds.high.toInt}"
    }

    /** Returns the underlying DBM. When an open and a closed DBM are present,
      * the closed version is returned.
      *
      * @return The underlying DBM.
      */
    override def getDbm: IntegerDbm =
      closed.orElse(open).get

    override def makeConstant(value: Double): Expression =
      Constant(value.toInt.toString, SystemParameters.tm.Int)

    override protected def copy(newEnv: Environment, from: List[Int], to: List[Int]): IntegerOctagons = {
      val newClosed = closed.map(dbm => getDbm.factory(newEnv.size).copy(dbm, from, to))
      val newOpen = open.map(dbm => getDbm.factory(newEnv.size).copy(dbm, from, to))
      factory(newEnv, newClosed, newOpen)
    }

    override def factory(env: Environment, closed: Option[IntegerDbm], open: Option[IntegerDbm]): IntegerOctagons = {
      val dbm = closed.orElse(open).get
      if (env.isTop) Top
      else if (dbm.isBottom)
        Bottom
      else Inner(env, closed, open)
    }

    /** Divides the first interval by the second interval.
      *
      * @param left  The first interval.
      * @param right The second interval.
      * @return The result of the division.
      */
    override protected def divide(left: Interval, right: Interval): Interval =
      (left / right).truncate()
  }

  /** The top element of the integer octagon domain.
    *
    * @author Jerome Dohrau
    */
  object Top
    extends IntegerOctagons
      with Octagons.Top[IntegerOctagons]

  /** The bottom element of the integer octagon domain.
    *
    * @author Jerome Dohrau
    */
  object Bottom
    extends IntegerOctagons
      with Octagons.Bottom[IntegerOctagons]

}

/** An element of the double octagon domain.
  *
  * NOTE: This domain is unsound due to rounding errors.
  *
  * @author Jerome Dohrau
  */
sealed trait DoubleOctagons
  extends Octagons[DoubleOctagons] {

  import Octagons.Dbm.topArr

  override def factory(env: Environment): DoubleOctagons =
    if (env.isTop) top()
    else DoubleOctagons.Inner(env, Some(DoubleDbm(env.size, topArr(env.size))), None)

  override def top(): DoubleOctagons = DoubleOctagons.Top

  override def bottom(): DoubleOctagons = DoubleOctagons.Bottom
}

object DoubleOctagons {

  /** An element of the double octagon domain that is neither the top element
    * nor the bottom element.
    *
    * Internally the constraints are stored in a DBM. There is an optional
    * closed DBM and an optional open DBM; however, at least one of them has to
    * be defined.
    *
    * @param env    The environment containing the identifiers.
    * @param closed The optional closed DBM.
    * @param open   The optional open DBM.
    * @author Jerome Dohrau
    */
  case class Inner(env: Environment,
                   var closed: Option[DoubleDbm],
                   open: Option[DoubleDbm])
    extends DoubleOctagons
      with Octagons.Inner[DoubleOctagons, DoubleDbm] {

    /** Returns the underlying closed DBM. If closed DBM is present, it is
      * computed from the open DBM.
      *
      * @return The underlying closed DBM.
      */
    override def closedDbm: DoubleDbm = {
      if (closed.isEmpty) closed = open.map(_.clone().close())
      closed.get
    }

    /** Returns the underlying DBM. When an open and a closed DBM are present,
      * the open version is returned.
      *
      * @return
      */
    override def openDbm: DoubleDbm =
      open.orElse(closed).get

    /** Returns true if there is a closed version of the underlying DBM.
      *
      * @return True if there is a closed version of the underlying DBM.
      */
    override def isClosed: Boolean =
      closed.isDefined

    override def epsilon = Interval.Epsilon

    override def toInterval(value: String) = Interval(value.toDouble, value.toDouble)

    override def getStringOfId(id: Identifier): String = {
      val bounds = getDbm.getBounds(env.getIndex(id))
      if (bounds.low.isNegInfinity && bounds.high.isPosInfinity) "Top"
      else if (bounds.low.isNegInfinity) s"$id<=${bounds.high}"
      else if (bounds.high.isPosInfinity) s"${bounds.low}<=$id"
      else if (bounds.low == bounds.high) s"$id==${bounds.low}"
      else s"${bounds.low}<=$id<=${bounds.high}"
    }

    /** Returns the underlying DBM. When an open and a closed DBM are present,
      * the closed version is returned.
      *
      * @return The underlying DBM.
      */
    override def getDbm: DoubleDbm =
      closed.orElse(open).get

    override def makeConstant(value: Double): Expression =
      Constant(value.toString, SystemParameters.tm.Int)

    override protected def copy(newEnv: Environment, from: List[Int], to: List[Int]): DoubleOctagons = {
      val newClosed = closed.map(dbm => getDbm.factory(newEnv.size).copy(dbm, from, to))
      val newOpen = open.map(dbm => getDbm.factory(newEnv.size).copy(dbm, from, to))
      factory(newEnv, newClosed, newOpen)
    }

    override def factory(env: Environment, closed: Option[DoubleDbm], open: Option[DoubleDbm]): DoubleOctagons = {
      val dbm = closed.orElse(open).get
      if (env.isTop) Top
      else if (dbm.isBottom)
        Bottom
      else Inner(env, closed, open)
    }

    /** Divides the first interval by the second interval.
      *
      * @param left  The first interval.
      * @param right The second interval.
      * @return The result of the division.
      */
    override protected def divide(left: Interval, right: Interval): Interval =
      left / right
  }

  /** The top element of the double octagon domain.
    *
    * @author Jerome Dohrau
    */
  object Top
    extends DoubleOctagons
      with Octagons.Top[DoubleOctagons]

  /** The bottom element of the double octagon domain.
    *
    * @author Jerome Dohrau
    */
  object Bottom
    extends DoubleOctagons
      with Octagons.Bottom[DoubleOctagons]

}