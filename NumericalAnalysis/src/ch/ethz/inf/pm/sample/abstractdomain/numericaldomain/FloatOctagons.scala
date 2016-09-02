/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.SetDomain.Default
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.FloatOctagons._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyBooleanType, DummyNumericalType, Type}

/**
  * Todo: make sure we do not have issues with infinity - infinity
  * Todo: maybe avoid unnecessary copying of octagon matrices by having a flag that indicates whether they may be visible to the outside
  * Todo: information hiding => which methods/classes can be made private?
  * Todo: improve performance of maxCopy
  */
object FloatOctagonTest {
  def main(args: Array[String]): Unit = {

    val a: Identifier = VariableIdentifier("a")(DummyNumericalType)
    val b: Identifier = VariableIdentifier("x")(DummyNumericalType)

    val three = Constant("3", DummyNumericalType)
    val two = Constant("2", DummyNumericalType)
    val e1 = BinaryArithmeticExpression(a, two, ArithmeticOperator.>, DummyBooleanType)

    val o1 = FloatOctagons.Top.assume(e1)
    val o2 = o1.assign(b, three).assign(a, b)
    val o3 = o2.assume(e1)

    println(o1)
    println(o2)
    println(o3)
  }
}

/**
  * @author Jerome Dohrau
  */
object FloatOctagons {

  object Top
    extends FloatOctagons
      with NumericalDomain.Relational.Top[FloatOctagons]
      with SimplifiedMergeDomain.Top[FloatOctagons]
      with BooleanExpressionSimplifier[FloatOctagons] {

    override def assign(variable: Identifier, expr: Expression): FloatOctagons =
      Inner(Environment(expr.ids + variable)).assign(variable, expr)

    override def assumeSimplified(expression: Expression): FloatOctagons =
      expression.ids match {
        case IdentifierSet.Top => this
        case IdentifierSet.Bottom => Bottom
        case ids => Inner(Environment(ids)).assume(expression)
      }
  }

  object Bottom
    extends FloatOctagons
      with NumericalDomain.Relational.Bottom[FloatOctagons]
      with SimplifiedMergeDomain.Bottom[FloatOctagons] {

    override def add(ids: Set[Identifier]): FloatOctagons = factory(ids)

    override def createVariable(variable: Identifier, typ: Type): FloatOctagons = add(Set(variable))
  }

  object Inner {
    def apply(env: Environment): FloatOctagons =
      Inner(env, Some(OctagonMatrix.top(env.size)), None)

    def apply(env: Environment, closed: Option[OctagonMatrix], open: Option[OctagonMatrix]): FloatOctagons =
      new Inner(env, closed, open)
  }

  class Inner(val env: Environment,
              var closed: Option[OctagonMatrix],
              open: Option[OctagonMatrix])
    extends FloatOctagons
      with NumericalDomain.Relational.Inner[FloatOctagons, Inner]
      with BooleanExpressionSimplifier[FloatOctagons] {

    require(closed.isDefined || open.isDefined)

    /**
      * @return preferably the closed version of the octagon matrix
      */
    def getMatrix: OctagonMatrix = closed.orElse(open).get

    /**
      * @return true iff a closed version of the octagon matrix is present
      */
    def isClosed: Boolean = closed.isDefined

    def getClosed: OctagonMatrix = {
      if (closed.isEmpty) {
        closed = open.map(_.clone().close())
      }
      closed.get
    }

    def getOpen: OctagonMatrix = open.orElse(closed).get

    override def lubSameEnvInner(that: Inner): FloatOctagons = {
      val newClosed = Some(getClosed.lub(that.getClosed))
      val newOpen = None
      factory(env, newClosed, newOpen)
    }

    override def glbSameEnvInner(that: Inner): FloatOctagons = {
      val newMat = Some(getMatrix.glb(that.getMatrix))
      val (newClosed, newOpen) = if (isClosed && that.isClosed) (newMat, None) else (None, newMat)
      factory(env, newClosed, newOpen)
    }

    override def wideningSameEnvInner(that: Inner): FloatOctagons = {
      val newClosed = None
      val newOpen = Some(getOpen.widening(that.getClosed))
      factory(env, newClosed, newOpen)
    }

    override def lessEqualSameEnvInner(that: Inner): Boolean =
      getClosed.lessThan(that.getClosed)

    override def assumeSimplified(expression: Expression): FloatOctagons = {
      val nonExisting = expression.ids.getNonTop.filterNot(exists)
      val x = if (nonExisting.nonEmpty)
        createVariables(nonExisting).assume(expression)
      else expression match {
        case BinaryArithmeticExpression(lhs, rhs, op, typ) =>
          val left = normalize(lhs)
          val right = normalize(rhs)
          op match {
            case ArithmeticOperator.== =>
              val res = assumeNormalized(left - right) glb assumeNormalized(right - left)
              res
            case ArithmeticOperator.!= =>
              val res = assumeNormalized(left - right + Interval.Epsilon) lub assumeNormalized(right - left + Interval.Epsilon)
              res
            case ArithmeticOperator.<= =>
              val res = assumeNormalized(left - right)
              res
            case ArithmeticOperator.< =>
              val res = assumeNormalized(left - right + Interval.Epsilon)
              res
            case ArithmeticOperator.>= =>
              val res = assumeNormalized(right - left)
              res
            case ArithmeticOperator.> =>
              val res = assumeNormalized(right - left + Interval.Epsilon)
              res
          }
        case _ => throw new IllegalArgumentException("The argument is expected to be a comparision")
      }
      x
    }

    def assumeNormalized(normalized: Normalized): FloatOctagons = {
      val Normalized(literals, interval) = normalized
      if (literals.isEmpty)
        if (interval.low > 0) Bottom else this
      else {
        val indices: List[Int] = literals.map(env.getIndex)
        val matrix = getClosed.clone()

        indices match {
          case i :: Nil => matrix.assume(i ^ 1, i, -2 * interval.low)
          case i :: j :: Nil =>
            matrix.assume(j ^ 1, i, -interval.low)
          case _ =>
            val evaluated = literals.map(evaluate)
            val totalSum = evaluated.fold(Interval.Zero)(_ + _)
            for (i <- indices.indices; j <- i + 1 until indices.length) {
              val partialSum = totalSum - evaluated(i) - evaluated(j)
              matrix.assume(j ^ 1, i, -partialSum.low)
            }
        }
        factory(env, Some(matrix), None)
      }
    }

    override def getConstraints(ids: Set[Identifier]): Set[Expression] = {
      val matrix = getMatrix

      // collect all constraints involving one variable
      val (constraints, relational) = ids.foldLeft((Set.empty[Expression], List.empty[Identifier])) {
        case ((currConstraints, currRelational), idA) =>
          val i = env.getIndex(idA)
          val bounds = matrix.getBounds(i)
          val newConstraints = currConstraints ++ makeConstraints(idA, bounds)
          val newRelational = if (bounds.low == bounds.high) currRelational else idA :: currRelational
          (newConstraints, newRelational)
      }

      val pairs = for {
        a <- relational
        b <- relational
        if env.getIndex(a) < env.getIndex(b)
      } yield (a,b)

      // collect all relational constraints that are not (trivially) implied by
      // the constraints we already have
      pairs.foldLeft(constraints) {
        case (currConstraints, (idA, idB)) =>
          val sumBounds = matrix.getBounds(env.getNegative(idB), env.getPositive(idA))
          val diffBounds = matrix.getBounds(env.getPositive(idB), env.getPositive(idA))
          val sum = BinaryArithmeticExpression(idA, idB, ArithmeticOperator.+, DummyNumericalType)
          val diff = BinaryArithmeticExpression(idA, idB, ArithmeticOperator.-, DummyNumericalType)
          currConstraints ++ makeConstraints(sum, sumBounds) ++ makeConstraints(diff, diffBounds)
      }
    }

    /** A helper function that constructs a set of constraints for an expression
      * that is bounded by the given interval.
      */
    private def makeConstraints(expression: Expression, bounds: Interval): Set[Expression] = {
      if (bounds.low == bounds.high) {
        // return an equality
        val constant = makeConstant(bounds.low)
        Set(BinaryArithmeticExpression(expression, constant, ArithmeticOperator.==, DummyBooleanType))
      } else {
        // construct lower bound if it is not negative infinity
        val lower = if (bounds.low.isNegInfinity) None else {
          val constant = makeConstant(bounds.low)
          val inequality = BinaryArithmeticExpression(constant, expression, ArithmeticOperator.<=, DummyBooleanType)
          Some(inequality)
        }
        // construct upper bound if it is not positive infinity
        val upper = if (bounds.high.isPosInfinity) None else {
          val constant = makeConstant(bounds.high)
          val inequality = BinaryArithmeticExpression(expression, constant, ArithmeticOperator.<=, DummyBooleanType)
          Some(inequality)
        }
        // return constraints
        lower.toSet ++ upper.toSet
      }
    }

    /** A helper function that constructs a constant from the given value.
      */
    private def makeConstant(value: Double): Expression =
      Constant(value.toInt.toString, DummyNumericalType)

    /**
      * Assumption: the environments of this and that are disjoint.
      */
    override def unifyInner(that: Inner): FloatOctagons = {
      val newEnv = env ++ that.env
      val newClosed = Some(OctagonMatrix.top(newEnv.size)
        .copy(getMatrix, env.getIndices(env.ids), newEnv.getIndices(env.ids))
        .copy(that.getMatrix, that.env.getIndices(that.env.ids), newEnv.getIndices(that.env.ids))
        .close())
      val newOpen = None
      factory(newEnv, newClosed, newOpen)
    }

    override def expand(idA: Identifier, idsB: Set[Identifier]): FloatOctagons = {
      if (numerical(idA)) {
        val newIds = idsB.filterNot(exists).filter(numerical)
        if (exists(idA) && newIds.nonEmpty) {
          val newEnv = env - idA ++ newIds
          val commonIds = (env.set.getNonTop - idA).toList
          val from = env.getIndices(commonIds) ++ List.fill(idsB.size)(env.getPositive(idA))
          val to = newEnv.getIndices(commonIds) ++ newEnv.getIndices(idsB.toList)
          val newMat = Some(OctagonMatrix.top(newEnv.size).copy(getMatrix, from, to))
          val (newClosed, newOpen) = if (isClosed) (newMat, None) else (None, newMat)
          factory(newEnv, newClosed, newOpen)
        } else {
          remove(Set(idA)).add(newIds)
        }
      } else this
    }

    override def rename(idA: Identifier, idB: Identifier): FloatOctagons = {
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

    override def fold(idsA: Set[Identifier], idB: Identifier): FloatOctagons = {
      if (numerical(idB)) {
        val ids = idsA.filter(exists).filter(numerical)
        if (!exists(idB) && ids.nonEmpty) {
          val newEnv = env -- idsA + idB
          val commonIds = (env.set.getNonTop -- idsA).toList
          val from = env.getIndices(commonIds) ++ env.getIndices(idsA.toList)
          val to = newEnv.getIndices(commonIds) ++ List.fill(idsA.size)(newEnv.getPositive(idB))
          val newClosed = Some(OctagonMatrix.top(newEnv.size).maxCopy(getClosed, from, to))
          val newOpen = None
          factory(newEnv, newClosed, newOpen)
        } else remove(ids).add(Set(idB))
      } else this
    }

    override def removeVariable(id: Identifier): FloatOctagons = remove(Set(id))

    override def createVariable(variable: Identifier, typ: Type): FloatOctagons = add(Set(variable))

    override def add(ids: Set[Identifier]): FloatOctagons = {
      val diff = ids.filterNot(exists).filter(numerical)
      if (diff.nonEmpty) {
        val newEnv = Environment(env.set ++ ids)
        copy(newEnv, env.getIndices(env.ids), newEnv.getIndices(env.ids))
      } else this
    }

    override def remove(ids: Set[Identifier]): FloatOctagons = {
      val diff = ids.filter(exists).filter(numerical)
      if (diff.nonEmpty) {
        val newEnv = Environment(env.set -- diff)
        copy(newEnv, env.getIndices(newEnv.ids), newEnv.getIndices(newEnv.ids))
      } else this
    }

    override def getStringOfId(id: Identifier): String = {
      this.getConstraints(Set(id)).mkString(",").replace(id.toString,"X")
//      val bounds = getMatrix.getBounds(env.getIndex(id))
//      if (bounds.low.isNegInfinity && bounds.high.isPosInfinity) "Top"
//      else if (bounds.low.isNegInfinity) s"$id<=${bounds.high.toInt}"
//      else if (bounds.high.isPosInfinity) s"${bounds.low.toInt}<=$id"
//      else if (bounds.low == bounds.high) s"$id==${bounds.low.toInt}"
//      else s"${bounds.low.toInt}<=$id<=${bounds.high.toInt}"
    }

    override def setToTop(variable: Identifier): FloatOctagons = {
      if (numerical(variable)) {
        if (exists(variable)) {
          val newClosed = Some(getClosed.clone().assignTop(env.getPositive(variable)))
          val newOpen = None
          factory(env, newClosed, newOpen)
        } else add(Set(variable)).setToTop(variable)
      } else this
    }

    override def getPossibleConstants(id: Identifier): Default[Constant] = SetDomain.Default.Top[Constant]()

    override def assign(variable: Identifier, expr: Expression): FloatOctagons = {
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

            literals match {
              case Nil =>
                val matrix = getClosed.clone().assignConstant(index, interval)
                factory(env, Some(matrix), None)
              case List(Positive(id)) =>
                val matrix = getClosed.clone()
                if (id == variable) matrix.assignAddition(index, interval)
                else matrix.assignRelational(index, env.getPositive(id), interval)
                factory(env, Some(matrix), None)
              case List(Negative(id)) =>
                val matrix = getClosed.clone()
                if (id != variable) matrix.assignRelational(index, env.getPositive(id), Interval.Zero)
                matrix.assignNegate(index)
                if (interval != Interval.Zero) matrix.assignAddition(index, interval)
                factory(env, Some(matrix), None)
              case _ =>
                val value = evaluate(Normalized(literals, interval))
                val matrix = getClosed.clone().assignConstant(index, value)
                factory(env, Some(matrix), None)
              /*case _ => setToTop(variable) match {
                case oct: Inner => {
                  val lhs = Normalized(List(Positive(variable)), Interval.Zero)
                  val rhs = Normalized(literals, interval)
                  oct.assumeNormalized(lhs - rhs) glb oct.assumeNormalized(rhs - lhs)
                }
                case _ => _
              }*/
            }
          }
        }
      } else this
    }

    override def ids: IdentifierSet = env.set

    // NORMALIZE

    def normalize(expr: Expression): Normalized = expr match {
      case const: Constant => Normalized(Nil, evaluate(const))
      case id: Identifier => Normalized(List(Positive(id)), Interval.Zero)
      case UnaryArithmeticExpression(arg, ArithmeticOperator.+, _) => normalize(arg)
      case UnaryArithmeticExpression(arg, ArithmeticOperator.-, _) => normalize(arg).unary_-()
      case BinaryArithmeticExpression(lhs, rhs, ArithmeticOperator.+, _) =>
        val Normalized(xs, left) = normalize(lhs)
        val Normalized(ys, right) = normalize(rhs)
        Normalized(xs ++ ys, left + right)
      case BinaryArithmeticExpression(lhs, rhs, ArithmeticOperator.-, _) =>
        val Normalized(xs, left) = normalize(lhs)
        val Normalized(ys, right) = -normalize(rhs)
        Normalized(xs ++ ys, left + right)
      case _ => Normalized(Nil, evaluate(expr))
    }

    // EVALUATE

    def evaluate(expr: Expression): Interval = expr match {
      case Constant("true", _, _) => Interval(1, 1)
      case Constant("false", _, _) => Interval(0, 0)
      case Constant(value, _, _) => Interval(value.toDouble, value.toDouble)
      case id: Identifier => evaluate(id)
      case UnaryArithmeticExpression(arg, ArithmeticOperator.+, _) => evaluate(arg)
      case UnaryArithmeticExpression(arg, ArithmeticOperator.-, _) => -evaluate(arg)
      case BinaryArithmeticExpression(lhs, rhs, op, _) =>
        val left = evaluate(lhs)
        val right = evaluate(rhs)
        op match {
          case ArithmeticOperator.+ => left + right
          case ArithmeticOperator.- => left - right
          case ArithmeticOperator.* => left * right
          case ArithmeticOperator./ => left / right
          case _ => Interval.Top
        }
      case _ => Interval.Top
    }

    def evaluate(value: Normalized): Interval =
      value.literals.map(evaluate).fold(value.interval)(_ + _)

    def evaluate(literal: Literal): Interval = literal match {
      case Positive(id) => evaluate(id)
      case Negative(id) => -evaluate(id)
    }

    def evaluate(id: Identifier): Interval = getMatrix.getBounds(env.getPositive(id))

    // HELPERS

    private def exists(id: Identifier): Boolean = env.set.contains(id)

    private def numerical(id: Identifier): Boolean = id.typ.isNumericalType

    private def copy(newEnv: Environment, from: List[Int], to: List[Int]): FloatOctagons = {
      val newClosed = closed.map(mat => OctagonMatrix.top(newEnv.size).copy(mat, from, to))
      val newOpen = open.map(mat => OctagonMatrix.top(newEnv.size).copy(mat, from, to))
      factory(newEnv, newClosed, newOpen)
    }
  }

  /**
    * An octagon matrix of dimension d stores constraints for some set of
    * variables v_0, v_2, ..., v_{d-1}. For every variable v_i we introduce a
    * positive version x_{2*i} = +v_i and a negative version x_{2*i+1} = -v_i.
    * The octagon constraints are stored in a (2*d x 2*d)-Matrix. An entry
    * m_{i,j} = c in the matrix represents the constraint x_i - x_j <= c.
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
  object OctagonMatrix {
    def Infinity: Double = Double.PositiveInfinity

    /**
      * Computes the number of matrix elements in an octagon matrix with the
      * specified dimension.
      *
      * @param dim the dimension of the octagon matrix
      * @return the number of matrix elements in the octagon matrix
      */
    private def size(dim: Int): Int = 2 * dim * (dim + 1)

    /**
      * Computes the index of a matrix element with the specified row and column
      * indices in the array.
      *
      * @param row the row index of the element
      * @param col the column index of the element
      * @return the index of the matrix element in the array
      */
    private def index(row: Int, col: Int): Int = if (row < col) lower(col ^ 1, row ^ 1) else lower(row, col)

    /**
      * Computes the index of a lower left matrix element with the specified row
      * and column indices in the array.
      *
      * Note: A lower left matrix element is a matrix element where the row and
      * column indices satisfy the inequality row/2 >= col/2.
      *
      * @param row the row index of the matrix element
      * @param col the column index of the matrix element
      * @return the index of the matrix element in the array
      */
    private def lower(row: Int, col: Int): Int = (row + 1) * (row + 1) / 2 + col

    def top(dim: Int): OctagonMatrix = {
      val arr = Array.fill(size(dim))(Infinity)
      for (i <- 0 until 2 * dim)
        arr(lower(i, i)) = 0.0
      OctagonMatrix(dim, arr)
    }

    def apply(dim: Int): OctagonMatrix = top(dim)

    def apply(dim: Int, arr: Array[Double]): OctagonMatrix = new OctagonMatrix(dim, arr)
  }

  /**
    * The internal representation of the matrix is a mutable array.
    *
    * @author Jerome Dohrau
    */
  class OctagonMatrix(val dim: Int, val arr: Array[Double]) {
    require(arr.length == 2 * dim * (dim + 1))

    import OctagonMatrix.{Infinity, index, lower}

    def isTop: Boolean = {
      Range(0, 2 * dim).forall(r => (0 until r).forall(c => arr(index(r, c)) == Infinity))
    }

    def isBottom: Boolean = {
      !Range(0, 2 * dim).exists(i => arr(index(i, i)) >= -0.1)
    }

    def lub(other: OctagonMatrix): OctagonMatrix = {
      require(dim == other.dim)
      OctagonMatrix(dim, arr.zip(other.arr).map(p => math.max(p._1, p._2)))
    }

    def glb(other: OctagonMatrix): OctagonMatrix = {
      require(dim == other.dim)
      OctagonMatrix(dim, arr.zip(other.arr).map(p => math.min(p._1, p._2)))
    }

    def widening(other: OctagonMatrix): OctagonMatrix = {
      require(dim == other.dim)
      OctagonMatrix(dim, arr.zip(other.arr).map(p => if (p._1 >= p._2) p._1 else Infinity))
    }

    def lessThan(other: OctagonMatrix): Boolean = {
      require(dim == other.dim)
      arr.zip(other.arr).forall(p => p._1 <= p._2)
    }

    def remove(index: Int): OctagonMatrix = {
      OctagonMatrix(dim, arr.clone()).assignTop(index)
    }

    /**
      * Note: modifies the internal matrix
      */
    def assume(row: Int, col: Int, value: Double): OctagonMatrix = {
      val rc = index(row, col)
      arr(rc) = math.min(arr(rc), value)
      close(row, col)
    }

    /**
      * Note: modifies the internal matrix
      */
    def assignTop(i: Int): OctagonMatrix = {
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

    def getBound(row: Int, col: Int): Double = arr(index(row, col))

    def getBounds(i: Int): Interval = {
      val low = -arr(lower(i, i ^ 1)) / 2
      val high = arr(lower(i ^ 1, i)) / 2
      Interval(low, high)
    }

    /**
      *
      * @param i The index corresponding to the literal x_i.
      * @param j The index corresponding to the literal x_j.
      * @return An interval [low, high] such that low <= x_j - x_i <= high.
      */
    def getBounds(i: Int, j: Int): Interval = {
      val low = -getBound(j, i)
      val high = getBound(i, j)
      Interval(low, high)
    }

    /**
      * v_{i/2} := [a, b]
      *
      * Note: modifies the internal matrix
      */
    def assignConstant(i: Int, const: Interval): OctagonMatrix = {
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
    def assignAddition(i: Int, const: Interval): OctagonMatrix = {
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
    def assignRelational(i: Int, j: Int, const: Interval): OctagonMatrix = {
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
      * x_i := - x_i
      *
      * Note: modifies the internal matrix
      */
    def assignNegate(i: Int): OctagonMatrix = {
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

    /**
      * Note: modifies the internal matrix
      */
    def close(): OctagonMatrix = {
      for (row <- 0 until dim; col <- row until dim) {
        close(2 * row, 2 * col)
      }

      this
    }

    /**
      * Note: modifies the internal matrix
      */
    def close(row: Int, col: Int) = {
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

    /**
      * Note: modifies theinternal matrix
      */
    def copy(other: OctagonMatrix, from: List[Int], to: List[Int]): OctagonMatrix = {
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

    /**
      * This method is equivalent to first copying the rows/columns and then
      * computing the least upper bound with the old matrix.
      *
      * Note: modifies the internal matrix
      *
      * @param other the matrix to copy the rows/columns from
      * @param from  the list of row/column indices to copy from
      * @param to    the list of row/column indices to copy to
      * @return this matrix
      */
    def maxCopy(other: OctagonMatrix, from: List[Int], to: List[Int]): OctagonMatrix = {
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

    override def clone(): OctagonMatrix = OctagonMatrix(dim, arr.clone())

    override def toString: String =
      (0 until 2 * dim)
        .map(row => (0 until 2 * dim)
          .map(col => index(row, col))
          .map(idx => arr(idx))
          .map(num => if (num == Infinity) "  ." else f"${num.toInt}%3d")
          .reduce(_ + " " + _))
        .reduce(_ + "\n" +_)
  }

  /**
    * Helper class that lexicographically sorts the identifiers of an identifier
    * in order to any given identifier to the corresponding row [column] index
    * in the octagon matrix.
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
      * index in the octagon matrix.
      */
    private lazy val map = ids.zipWithIndex.toMap

    def isTop: Boolean = set.isTop

    def isBottom: Boolean = set.isBottom

    def size: Int = ids.length

    def getPositive(id: Identifier): Int = 2 * map(id)

    def getNegative(id: Identifier): Int = 2 * map(id) + 1

    def getIndex(id: Identifier): Int = getPositive(id)

    def getIndex(literal: Literal): Int = literal match {
      case Positive(id) => getPositive(id)
      case Negative(id) => getNegative(id)
    }

    def getIndices(ids: List[Identifier]): List[Int] = ids.map(getIndex)

    // OPERATORS

    def ++(other: Environment): Environment = this ++ other.set

    def ++(ids: IdentifierSet): Environment = Environment(set ++ ids)

    def ++(ids: Set[Identifier]): Environment = this ++ IdentifierSet.Inner(ids)

    def --(other: Environment): Environment = this -- other.set

    def --(ids: IdentifierSet): Environment = Environment(set -- ids)

    def --(ids: Set[Identifier]): Environment = this -- IdentifierSet.Inner(ids)

    def +(id: Identifier): Environment = Environment(set + id)

    def -(id: Identifier): Environment = Environment(set - id)
  }

  /**
    * A normalized expression of the form l_1 + ... + l_k + [a, b], where l_i are literals.
    */
  case class Normalized(literals: List[Literal], interval: Interval) {

    if (SystemParameters.DEBUG) assert(literals == cancel(literals))

    def unary_-(): Normalized = Normalized(literals.map(x => -x), -interval)

    def +(other: Normalized) = Normalized(cancel(literals ++ other.literals), interval + other.interval)

    def -(other: Normalized) = this + -other

    def +(v: Interval) = Normalized(literals, interval + v)

    def -(v: Interval) = Normalized(literals, interval - v)

    private def cancel(literals:List[Literal]):List[Literal] = {
      val counts =
        literals.foldLeft(Map.empty[Identifier, Int]) {
          (map: Map[Identifier, Int], lit: Literal) =>
            val x = lit match {
              case Negative(_) => -1;
              case Positive(_) => +1
            }
            map.get(lit.id) match {
              case Some(y) => map + (lit.id -> (y + x))
              case None => map + (lit.id -> x)
            }
        }
      counts.flatMap(x =>
        if (x._2 >= 0)
          List.fill(x._2)(Positive(x._1))
        else
          List.fill(-x._2)(Negative(x._1))
      ).toList
    }


  }

  /**
    * A positive or a negative occurrence of an identifier
    */
  sealed trait Literal {
    def id: Identifier

    def unary_-(): Literal = this match {
      case Positive(id) => Negative(id)
      case Negative(id) => Positive(id)
    }
  }

  case class Positive(id: Identifier) extends Literal

  case class Negative(id: Identifier) extends Literal

  object Interval {
    def Top = Interval(Double.NegativeInfinity, Double.PositiveInfinity)

    def Zero = Interval(0.0, 0.0)

    def One = Interval(1.0, 1.0)

    def Epsilon = Interval(NumericalAnalysisConstants.epsilon, NumericalAnalysisConstants.epsilon)
  }

  /**
    *
    * @param low  the lower bound of the interval
    * @param high the upper bound of the interval
    */
  case class Interval(low: Double, high: Double) {
    def unary_-(): Interval = Interval(-high, -low)

    def +(other: Interval): Interval = Interval(low + other.low, high + other.high)

    def -(other: Interval): Interval = Interval(low - other.high, high - other.low)

    def *(other: Interval): Interval = {
      val a = low * other.low
      val b = low * other.high
      val c = high * other.low
      val d = high * other.high
      Interval(min(a, b, c, d), max(a, b, c, d))
    }

    def /(other: Interval): Interval = {
      if (other.low < 0.0 && 0.0 < other.high) {
        val Interval(a, b) = this / Interval(other.low, 0.0)
        val Interval(c, d) = this / Interval(0.0, other.high)
        Interval(math.min(a, c), math.max(c, d))
      } else {
        val a = truncate(low / other.low)
        val b = truncate(low / other.high)
        val c = truncate(high / other.low)
        val d = truncate(high / other.high)
        Interval(min(a, b, c, d), max(a, b, c, d))
      }
    }

    private def min(a: Double, b: Double, c: Double, d: Double): Double =
      math.min(math.min(a, b), math.min(c, d))

    private def max(a: Double, b: Double, c: Double, d: Double): Double =
      math.max(math.max(a, b), math.max(c, d))

    private def truncate(a: Double): Double = a
  }

}

/**
  * @author Jerome Dohrau
  */
trait FloatOctagons
  extends NumericalDomain.Relational[FloatOctagons]
    with SimplifiedSemanticDomain[FloatOctagons]
    with SimplifiedMergeDomain[FloatOctagons] {

  override def factory(): FloatOctagons = factory(Environment(IdentifierSet.Top))

  def factory(ids: Set[Identifier]): FloatOctagons =
    factory(Environment(IdentifierSet.Inner(ids)))

  def factory(env: Environment): FloatOctagons =
    if (env.isTop) Top
    else Inner(env)

  def factory(env: Environment, closed: Option[OctagonMatrix], open: Option[OctagonMatrix]): FloatOctagons = {
    val matrix = closed.orElse(open).get
    if (env.isTop) Top
    else if (!env.isBottom && matrix.isBottom) Bottom
    else Inner(env, closed, open)
  }

  override def top(): FloatOctagons = FloatOctagons.Top

  override def bottom(): FloatOctagons = FloatOctagons.Bottom

  override def toString: String = this match {
    case FloatOctagons.Top => "Top\n"
    case FloatOctagons.Bottom => "Bottom\n"
    case a: FloatOctagons.Inner => a.getConstraints(a.ids.getNonTop).mkString("\n")
  }
}
