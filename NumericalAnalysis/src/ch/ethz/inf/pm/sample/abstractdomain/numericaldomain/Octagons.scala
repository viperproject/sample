/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain.SetDomain.Default
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Octagons._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyBooleanType, DummyNumericalType, Type}

/**
  * Todo: make sure we do not have issues with infinity - infinity
  * Todo: maybe avoid unnecessary copying of octagon matrices by having a flag that indicates whether they may be visible to the outside
  * Todo: information hiding => which methods/classes can be made private?
  * Todo: improve performance of maxCopy
  */
object OctagonTest {
  def main(args: Array[String]): Unit = {

    val a: Identifier = VariableIdentifier("a")(DummyNumericalType)
    val b: Identifier = VariableIdentifier("x")(DummyNumericalType)

    val three = Constant("3", DummyNumericalType)
    val two = Constant("2", DummyNumericalType)
    val e1 = BinaryArithmeticExpression(a, two, ArithmeticOperator.>, DummyBooleanType)

    val o1 = Octagons.Top.assume(e1)
    val o2 = o1.assign(b, three).assign(a, b)
    val o3 = o2.assume(e1);

    o1.print()
    o2.print()
    o3.print()
  }
}

/**
  * @author Jerome Dohrau
  */
object Octagons {

  object Top
    extends Octagons
      with NumericalDomain.Relational.Top[Octagons]
      with SimplifiedMergeDomain.Top[Octagons]
      with BooleanExpressionSimplifier[Octagons] {

    override def assign(variable: Identifier, expr: Expression): Octagons =
      Inner(Environment(expr.ids + variable)).assign(variable, expr)

    override def assumeSimplified(expression: Expression): Octagons =
      expression.ids match {
        case IdentifierSet.Top => this
        case IdentifierSet.Bottom => Bottom
        case ids => Inner(Environment(ids)).assume(expression)
      }
  }

  object Bottom
    extends Octagons
      with NumericalDomain.Relational.Bottom[Octagons]
      with SimplifiedMergeDomain.Bottom[Octagons] {

    override def add(ids: Set[Identifier]): Octagons = factory(ids)

    override def createVariable(variable: Identifier, typ: Type): Octagons = add(Set(variable))
  }

  object Inner {
    def apply(env: Environment): Octagons =
      Inner(env, Some(OctagonMatrix.top(env.size)), None)

    def apply(env: Environment, closed: Option[OctagonMatrix], open: Option[OctagonMatrix]): Octagons =
      new Inner(env, closed, open)
  }

  class Inner(val env: Environment,
              var closed: Option[OctagonMatrix],
              open: Option[OctagonMatrix])
    extends Octagons
      with NumericalDomain.Relational.Inner[Octagons, Inner]
      with BooleanExpressionSimplifier[Octagons] {

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

    override def lubSameEnvInner(that: Inner): Octagons = {
      val newClosed = Some(getClosed.lub(that.getClosed))
      val newOpen = None
      factory(env, newClosed, newOpen)
    }

    override def glbSameEnvInner(that: Inner): Octagons = {
      val newMat = Some(getMatrix.glb(that.getMatrix))
      val (newClosed, newOpen) = if (isClosed && that.isClosed) (newMat, None) else (None, newMat)
      factory(env, newClosed, newOpen)
    }

    override def wideningSameEnvInner(that: Inner): Octagons = {
      val newClosed = None
      val newOpen = Some(getOpen.widening(that.getClosed))
      factory(env, newClosed, newOpen)
    }

    override def lessEqualSameEnvInner(that: Inner): Boolean =
      getClosed.lessThan(that.getClosed)

    override def assumeSimplified(expression: Expression): Octagons = {
      val nonExisting = expression.ids.getNonTop.filterNot(exists)
      if (nonExisting.nonEmpty)
        createVariables(nonExisting).assume(expression)
      else expression match {
        case BinaryArithmeticExpression(lhs, rhs, op, typ) =>
          val left = normalize(lhs)
          val right = normalize(rhs)
          op match {
            case ArithmeticOperator.== => assumeNormalized(left - right) glb assumeNormalized(right - left)
            case ArithmeticOperator.!= => assumeNormalized(left - right + Interval.One) lub assumeNormalized(right - left + Interval.One)
            case ArithmeticOperator.<= => assumeNormalized(left - right)
            case ArithmeticOperator.< => assumeNormalized(left - right + Interval.One)
            case ArithmeticOperator.>= => assumeNormalized(right - left)
            case ArithmeticOperator.> => assumeNormalized(right - left + Interval.One)
          }
        case _ => throw new IllegalArgumentException("The argument is expected to be a comparision")
      }
    }

    def assumeNormalized(normalized: Normalized): Octagons = {
      val Normalized(literals, interval) = normalized
      if (literals.isEmpty)
        if (interval.high <= 0) this else Bottom
      else {
        val indices: List[Int] = literals.map(env.getIndex(_))
        val matrix = getClosed.clone()

        indices match {
          case i :: Nil => matrix.assume(i ^ 1, i, -2 * interval.low)
          case i :: j :: Nil => matrix.assume(j ^ 1, i, -interval.low)
          case _ => {
            val evaluated = literals.map(evaluate)
            val toatalSum = evaluated.fold(Interval.Zero)(_ + _)
            for (i <- 0 until indices.length; j <- i + 1 until indices.length) {
              val partialSum = toatalSum - evaluated(i) - evaluated(j)
              matrix.assume(j ^ 1, i, -partialSum.low)
            }
          }
        }
        factory(env, Some(matrix), None)
      }
    }

    override def getConstraints(ids: Set[Identifier]): Set[Expression] = ???

    /**
      * Assumption: the environments of this and that are disjoint.
      */
    override def unifyInner(that: Inner): Octagons = {
      val newEnv = env ++ that.env
      val newClosed = Some(OctagonMatrix.top(newEnv.size)
        .copy(getMatrix, env.getIndices(env.ids), newEnv.getIndices(env.ids))
        .copy(that.getMatrix, that.env.getIndices(that.env.ids), newEnv.getIndices(that.env.ids))
        .close())
      val newOpen = None
      factory(newEnv, newClosed, newOpen)
    }

    override def expand(idA: Identifier, idsB: Set[Identifier]): Octagons = {
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

    override def rename(idA: Identifier, idB: Identifier): Octagons = {
      if (numerical(idA) && numerical(idB)) {
        if (exists(idA) && !exists(idB)) {
          val newEnv = env - idA + idB
          val commonIds = (env.set.getNonTop - idA).toList
          val from = env.getPositive(idA) :: env.getIndices(commonIds)
          val to = newEnv.getPositive(idB) :: newEnv.getIndices(commonIds)
          copy(newEnv, from, to);
        } else remove(Set(idA)).add(Set(idB))
      } else this
    }

    override def fold(idsA: Set[Identifier], idB: Identifier): Octagons = {
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

    override def removeVariable(id: Identifier): Octagons = remove(Set(id))

    override def createVariable(variable: Identifier, typ: Type): Octagons = add(Set(variable))

    override def add(ids: Set[Identifier]): Octagons = {
      val diff = ids.filterNot(exists).filter(numerical)
      if (diff.nonEmpty) {
        val newEnv = Environment(env.set ++ ids)
        copy(newEnv, env.getIndices(env.ids), newEnv.getIndices(env.ids))
      } else this
    }

    override def remove(ids: Set[Identifier]): Octagons = {
      val diff = ids.filter(exists).filter(numerical)
      if (diff.nonEmpty) {
        val newEnv = Environment(env.set -- diff)
        copy(newEnv, env.getIndices(newEnv.ids), newEnv.getIndices(newEnv.ids))
      } else this
    }

    override def getStringOfId(id: Identifier): String = ???

    override def setToTop(variable: Identifier): Octagons = {
      if (numerical(variable)) {
        if (exists(variable)) {
          val newClosed = Some(getClosed.clone().assignTop(env.getPositive(variable)))
          val newOpen = None
          factory(env, newClosed, newOpen)
        } else add(Set(variable)).setToTop(variable)
      } else this
    }

    override def getPossibleConstants(id: Identifier): Default[Constant] = ???

    override def assign(variable: Identifier, expr: Expression): Octagons = {
      if (numerical(variable)) {
        if (expr.ids.getNonTop.exists(!numerical(_))) {
          setToTop(variable)
        } else {
          val nonExisting = (expr.ids.getNonTop + variable).filterNot(exists)
          if (nonExisting.nonEmpty) {
            createVariables(nonExisting).assign(variable, expr)
          } else {
            val index = env.getPositive(variable);
            val Normalized(literals, interval) = normalize(expr)

            literals match {
              case Nil => {
                val matrix = getClosed.clone().assignConstant(index, interval)
                factory(env, Some(matrix), None)
              }
              case List(Positive(id)) => {
                val matrix = getClosed.clone()
                if (id == variable) matrix.assignAddition(index, interval)
                else matrix.assignRelational(index, env.getPositive(id), interval)
                factory(env, Some(matrix), None)
              }
              case List(Negative(id)) => {
                val matrix = getClosed.clone()
                if (id != variable) matrix.assignRelational(index, env.getPositive(id), Interval.Zero)
                matrix.assignNegate(index)
                if (interval != Interval.Zero) matrix.assignAddition(index, interval)
                factory(env, Some(matrix), None)
              }
              case _ => {
                val value = evaluate(Normalized(literals, interval))
                val matrix = getClosed.clone().assignConstant(index, value)
                factory(env, Some(matrix), None)
              }
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
      case BinaryArithmeticExpression(lhs, rhs, ArithmeticOperator.+, _) => {
        val Normalized(xs, left) = normalize(lhs)
        val Normalized(ys, right) = normalize(rhs)
        Normalized(xs ++ ys, left + right)
      }
      case BinaryArithmeticExpression(lhs, rhs, ArithmeticOperator.-, _) => {
        val Normalized(xs, left) = normalize(lhs)
        val Normalized(ys, right) = -normalize(rhs)
        Normalized(xs ++ ys, left + right)
      }
      case _ => Normalized(Nil, evaluate(expr))
    }

    // EVALUATE

    def evaluate(expr: Expression): Interval = expr match {
      case Constant(value, _, _) => Interval(value.toInt, value.toInt)
      case id: Identifier => evaluate(id)
      case UnaryArithmeticExpression(arg, ArithmeticOperator.+, _) => evaluate(arg)
      case UnaryArithmeticExpression(arg, ArithmeticOperator.-, _) => -evaluate(arg)
      case BinaryArithmeticExpression(lhs, rhs, op, _) => {
        val left = evaluate(lhs)
        val right = evaluate(rhs)
        op match {
          case ArithmeticOperator.+ => left + right
          case ArithmeticOperator.- => left - right
          case ArithmeticOperator.* => left * right
          case ArithmeticOperator./ => left / right
          case _ => Interval.Top
        }
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

    private def copy(newEnv: Environment, from: List[Int], to: List[Int]): Octagons = {
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

    /**
      * Prints the lower left half of the matrix.
      */
    def printLower(): Unit = {
      var offset = 0
      for (i <- 0 until 2 * dim) {
        val length = (i / 2) * 2 + 2
        println(arr
          .slice(offset, offset + length)
          .foldRight("")((a, b) => (if (a == Infinity) " . " else f"${a.toInt}%2d ") + b))
        offset += length
      }
      println()
    }

    /**
      * Prints the entire matrix.
      */
    def printFull(): Unit = {
      for (i <- 0 until 2 * dim)
        println((0 until 2 * dim)
          .map(j => arr(index(i, j)))
          .foldRight("")((a, b) => (if (a == Infinity) " . " else f"${a.toInt}%2d ") + b))
      println()
    }
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
      * The sorted list of identifers.
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

    def getPositive(id: Identifier): Int = 2 * map.get(id).get

    def getNegative(id: Identifier): Int = 2 * map.get(id).get + 1

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
    * A normalized expression of the form id + [a,b] or -id + [a,b].
    */
  case class Normalized(literals: List[Literal], interval: Interval) {
    def unary_-(): Normalized = Normalized(literals.map(x => -x), -interval);

    def +(other: Normalized) = Normalized(literals ++ other.literals, interval + other.interval)

    def -(other: Normalized) = this + -other

    def +(v: Interval) = Normalized(literals, interval + v)

    def -(v: Interval) = Normalized(literals, interval - v)
  }

  /**
    * A positive or a negative occurence of an identifier
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

    private def truncate(a: Double): Double =
      if (a >= 0) math.floor(a) else math.ceil(a)
  }

}

/**
  * @author Jerome Dohrau
  */
trait Octagons
  extends NumericalDomain.Relational[Octagons]
    with SimplifiedSemanticDomain[Octagons]
    with SimplifiedMergeDomain[Octagons] {

  override def factory(): Octagons = factory(Environment(IdentifierSet.Bottom))

  def factory(ids: Set[Identifier]): Octagons =
    factory(Environment(IdentifierSet.Inner(ids)))

  def factory(env: Environment): Octagons =
    if (env.isTop) Top
    else if (env.isBottom) Bottom
    else Inner(env)

  def factory(env: Environment, closed: Option[OctagonMatrix], open: Option[OctagonMatrix]): Octagons =
    if (env.isTop) Top
    else if (env.isBottom) Bottom
    else Inner(env, closed, open)

  override def top(): Octagons = Octagons.Top

  override def bottom(): Octagons = Octagons.Bottom

  def print() = this match {
    case Octagons.Top => println("Top\n")
    case Octagons.Bottom => println("Bottom\n")
    case a: Octagons.Inner => a.getMatrix.printFull()
  }
}
