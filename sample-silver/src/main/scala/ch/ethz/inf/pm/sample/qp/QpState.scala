/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.qp

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.domain.{MapDomain, StackDomain}
import ch.ethz.inf.pm.sample.execution.{CfgPosition, SilverState, Simplifications}
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSilverConverter
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.util.SampleExpressions._
import com.typesafe.scalalogging.LazyLogging

/**
  * A state used for the quantified permission inference.
  *
  * @param pp    The current program point.
  * @param expr  The current expression.
  * @param stack The stack of domains.
  * @tparam D The type of the domain.
  * @author Jerome Dohrau
  */
case class QpState[D <: QpDomain[D]](pp: ProgramPoint,
                                     expr: ExpressionSet,
                                     stack: StackDomain[D])
  extends SilverState[QpState[D]]
    with Simplifications[QpState[D]]
    with StateWithRefiningAnalysisStubs[QpState[D]]
    with LazyLogging {

  /* ------------------------------------------------------------------------- *
   * LATTICE FUNCTIONS
   */

  override def isTop: Boolean = stack.isTop

  override def isBottom: Boolean = stack.isBottom

  override def factory(): QpState[D] = {
    logger.trace("factory()")
    copy(stack = stack.factory())
  }

  override def top(): QpState[D] = {
    logger.trace("top()")
    copy(stack = stack.top())
  }

  override def bottom(): QpState[D] = {
    logger.trace("bottom()")
    copy(stack = stack.bottom())
  }

  override def lub(other: QpState[D]): QpState[D] = {
    logger.trace("lub()")
    copy(stack = stack lub other.stack)
  }

  override def glb(other: QpState[D]): QpState[D] = {
    logger.trace("glb()")
    copy(stack = stack glb other.stack)
  }

  override def widening(other: QpState[D]): QpState[D] = {
    logger.trace("widening()")
    copy(stack = stack widening other.stack)
  }

  override def lessEqual(other: QpState[D]): Boolean = {
    logger.trace("lessEqual()")
    stack lessEqual other.stack
  }

  /* ------------------------------------------------------------------------- *
   * STATE FUNCTIONS
   */

  override def before(pp: ProgramPoint): QpState[D] = copy(pp = pp)

  override def setExpression(expr: ExpressionSet): QpState[D] = copy(expr = expr)

  override def removeExpression(): QpState[D] = copy(expr = ExpressionSet())

  override def ids: IdentifierSet = ???

  override def setArgument(x: ExpressionSet, right: ExpressionSet): QpState[D] = ???

  override def throws(t: ExpressionSet): QpState[D] = ???

  override def createObject(typ: Type, pp: ProgramPoint): QpState[D] = ???

  override def pruneUnreachableHeap(): QpState[D] = ???

  override def pruneVariables(filter: (VariableIdentifier) => Boolean): QpState[D] = ???

  /* ------------------------------------------------------------------------- *
   * SIMPLE STATE FUNCTIONS
   */

  override def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): QpState[D] = this

  override def createVariableForArgument(variable: VariableIdentifier, typ: Type): QpState[D] =
    createVariable(variable, typ, DummyProgramPoint)

  override def removeVariable(variable: VariableIdentifier): QpState[D] = this

  override def assume(expression: Expression): QpState[D] = {
    logger.trace(s"assume($expression)")
    copy(stack = stack.update(_.assume(expression)))
  }

  override def assignVariable(target: Expression, value: Expression): QpState[D] = {
    logger.trace(s"assignVariable($target, $value)")
    copy(stack = stack.update(_.assign(target, value)))
  }

  override def assignField(target: Expression, field: String, value: Expression): QpState[D] = {
    logger.trace(s"assignField($target, $value)")
    copy(stack = stack.update(_.assign(target, value)))
  }

  override def setVariableToTop(variable: Expression): QpState[D] = ???

  /* ------------------------------------------------------------------------- *
   * SILVER STATE FUNCTIONS
   */

  override def inhale(expression: Expression): QpState[D] = {
    logger.trace(s"inhale($expression)")
    copy(stack = stack.update(_.inhale(expression)))
  }

  override def exhale(expression: Expression): QpState[D] = {
    logger.trace(s"exhale($expression)")
    copy(stack = stack.update(_.exhale(expression)))
  }

  override def enterLoop(): QpState[D] = ???

  // TODO: Project out changing variables.
  // TODO: If no variables change make sure we take care of the changing heap.
  def enterLoop(changing: Seq[VariableIdentifier], position: CfgPosition): QpState[D] = {
    logger.trace(s"enterLoop($changing)")
    val newStack = stack.pop { case (inner, outer) => outer.merge(changing, position, inner) }
    copy(stack = newStack)
  }

  override def leaveLoop(): QpState[D] = {
    logger.trace("leaveLoop()")
    copy(stack = stack.push(_ => stack.default.bottom()))
  }

  def reset(): QpState[D] = {
    val initialized = stack
      .map(_.bottom())
      .update(_ => stack.default)
    copy(stack = initialized)
  }

}

trait QpDomain[D <: QpDomain[D]]
  extends Lattice[D] {

  this: D =>

  def assign(target: Expression, value: Expression): D

  def assume(condition: Expression): D

  def inhale(expression: Expression): D

  def exhale(expression: Expression): D

  def merge(changing: Seq[VariableIdentifier], position: CfgPosition, inner: D): D
}

case class QpSpecification(under: List[Expression] = List.empty,
                           over: List[Expression] = List.empty,
                           records: MapDomain[VariableIdentifier, QpRecord] = MapDomain.Inner(Map.empty, QpRecord()))
  extends QpDomain[QpSpecification] {

  def initialize(fields: Seq[VariableIdentifier]): QpSpecification = {
    val newRecords = fields.foldLeft(records.clear()) { case (result, field) => result.set(field, QpRecord()) }
    QpSpecification(records = newRecords)
  }

  override def isTop: Boolean = records.isTop

  override def isBottom: Boolean = records.isBottom

  override def factory(): QpSpecification = QpSpecification()

  override def top(): QpSpecification = QpSpecification(records = records.top())

  override def bottom(): QpSpecification = QpSpecification(records = records.bottom())

  override def lub(other: QpSpecification): QpSpecification = QpSpecification(records = records lub other.records)

  override def glb(other: QpSpecification): QpSpecification = QpSpecification(records = records glb other.records)

  override def widening(other: QpSpecification): QpSpecification = QpSpecification(records = records widening other.records)

  override def lessEqual(other: QpSpecification): Boolean = records lessEqual other.records

  override def assign(target: Expression, value: Expression): QpSpecification = {
    val newRecords = records.map { case (_, record) => record.assign(target, value) }
    QpSpecification(records = newRecords).write(target).read(value)
  }

  override def assume(condition: Expression): QpSpecification = condition match {
    case FunctionCallExpression("under", arguments, _) => copy(under = under ++ arguments)
    case FunctionCallExpression("over", arguments, _) => copy(over = over ++ arguments)
    case _ =>
      val newRecords = records.map { case (_, record) => record.assume(condition) }
      QpSpecification(records = newRecords).read(condition)
  }

  override def inhale(expression: Expression): QpSpecification = expression match {
    case FieldAccessPredicate(FieldAccessExpression(receiver, _), permission) =>
      val leaf = toLeaf(toCondition(receiver), permission)
      val newRecords = records.map { case (_, record) => record.gain(leaf) }
      QpSpecification(records = newRecords).read(expression)
  }

  override def exhale(expression: Expression): QpSpecification = expression match {
    case FieldAccessPredicate(FieldAccessExpression(receiver, _), permission) =>
      val leaf = toLeaf(toCondition(receiver), permission)
      val newRecords = records.map { case (_, record) => record.lose(leaf) }
      QpSpecification(records = newRecords).read(expression)
  }

  override def merge(changing: Seq[VariableIdentifier], position: CfgPosition, inner: QpSpecification): QpSpecification = {
    // get loop invariant
    lazy val invariant = {
      val numerical = QpContext.getNumerical
      val domain = numerical.preStateAt(position).domain
      val constraints = domain.getConstraints(domain.ids.toSet)
      AndList(constraints)
    }
    // try to extract over- and underapproximate loop invariants
    val overapproximate = if (over.isEmpty) invariant else AndList(over)
    val underapproximate = if (under.isEmpty) False else AndList(under)

    val (newRecords, b) = (records, inner.records) match {
      case (MapDomain.Inner(m1, default), MapDomain.Inner(m2, _)) =>
        val keys = m1.keySet ++ m2.keySet
        val (a, b) = keys.foldLeft((Map.empty[VariableIdentifier, QpRecord], Map.empty[VariableIdentifier, QpRecord])) { case ((r1, rb), key) =>
          val outerOriginal = records.get(key)
          val innerOriginal = inner.records.get(key)
          if (outerOriginal.isTop || innerOriginal.isTop) ???
          else if (outerOriginal.isBottom || innerOriginal.isBottom) ???
          else {
            val outerProjected = outerOriginal.project(changing, overapproximate, underapproximate)
            val innerProjected = innerOriginal.project(changing, overapproximate, underapproximate)

            // combine preconditions
            val precondition = {
              val loop = innerProjected.precondition
              val propagated = Minus(outerProjected.precondition, innerProjected.difference)
              val ConditionalExpression(Not(condition), after, _) = outerOriginal.precondition
              ConditionalExpression(condition, Max(loop, propagated), after)
            }
            // combine differences
            val difference = {
              val loop = innerProjected.difference
              val propagated = outerProjected.difference
              val ConditionalExpression(Not(condition), after, _) = outerOriginal.difference
              ConditionalExpression(condition, Plus(loop, propagated), after)
            }
            val combined = QpRecord(precondition, difference)
            (r1.updated(key, combined), rb.updated(key, innerProjected))
          }
        }
        (MapDomain.Inner(a, default), MapDomain.Inner(b, default))
      case _ => ???
    }

    QpContext.setInvariant(position, QpSpecification(records = b))
    QpSpecification(records = newRecords)
  }

  def read(expression: Expression): QpSpecification = access(expression, Permission(1, 100))

  def write(expression: Expression): QpSpecification = access(expression, Full)

  def access(expression: Expression, permission: Expression): QpSpecification = expression match {
    case _: Constant => this
    case _: VariableIdentifier => this
    case NegatedBooleanExpression(argument) => read(argument)
    case BinaryBooleanExpression(left, right, _) => read(left).read(right)
    case UnaryArithmeticExpression(argument, _, _) => read(argument)
    case BinaryArithmeticExpression(left, right, _) => read(left).read(right)
    case ReferenceComparisonExpression(left, right, _) => read(left).read(right)
    case FunctionCallExpression(_, arguments, _) => arguments.foldLeft(this) { case (updated, argument) => updated.read(argument) }
    case FieldAccessPredicate(FieldAccessExpression(receiver, _), _) => read(receiver)
    case FieldAccessExpression(receiver, field) =>
      val leaf = toLeaf(toCondition(receiver), permission)
      val newRecords = records.update(field, _.assert(leaf))
      QpSpecification(records = newRecords)
    case _ => ???
  }

  private def toLeaf(condition: Expression, permission: Expression): Expression =
    ConditionalExpression(condition, permission, No)

  private def toCondition(receiver: Expression): Expression = receiver match {
    case FunctionCallExpression(name, arguments, _) =>
      val quantified = QpContext.getQuantified(name)
      val variables = quantified.map { quantified =>
        val typ = DefaultSilverConverter.convert(quantified.typ)
        VariableIdentifier(quantified.name)(typ)
      }
      val zipped = variables zip arguments
      AndList(zipped.map { case (q, a) => Equal(q, a) })
  }
}

case class QpRecord(precondition: Expression = No,
                    difference: Expression = No,
                    isTop: Boolean = false,
                    isBottom: Boolean = false)
  extends Lattice[QpRecord] {

  def postcondition: Expression = Plus(precondition, difference)

  override def factory(): QpRecord = QpRecord()

  override def top(): QpRecord = QpRecord(isTop = true)

  override def bottom(): QpRecord = QpRecord(isBottom = true)

  override def lub(other: QpRecord): QpRecord =
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else {
      def merge(left: Expression, right: Expression): Expression = (left, right) match {
        case (ConditionalExpression(condition, positive, No), ConditionalExpression(Not(negated), negative, No)) if condition == negated => ConditionalExpression(condition, positive, negative)
        case (ConditionalExpression(Not(negated), negative, No), ConditionalExpression(condition, positive, No)) if condition == negated => ConditionalExpression(condition, positive, negative)
        case _ => ???
      }

      QpRecord(precondition = merge(precondition, other.precondition), difference = merge(difference, other.difference))
    }

  override def glb(other: QpRecord): QpRecord = ???

  override def widening(other: QpRecord): QpRecord = ???

  override def lessEqual(other: QpRecord): Boolean =
    if (isBottom || other.isTop) true
    else if (isTop || other.isBottom) false
    else ???

  def assume(condition: Expression): QpRecord = update(ConditionalExpression(condition, _, No))

  def gain(leaf: Expression): QpRecord = update(pre => Max(Minus(pre, leaf), No), diff => Plus(diff, leaf))

  def lose(leaf: Expression): QpRecord = update(pre => Plus(pre, leaf), diff => Minus(diff, leaf))

  def assert(leaf: Expression): QpRecord = update(pre => Max(pre, leaf), identity)

  def assign(target: Expression, value: Expression): QpRecord = target match {
    case _: VariableIdentifier => transform {
      case `target` => value
      case other => other
    }
    case FieldAccessExpression(receiver, field) => transform {
      case FieldAccessExpression(`receiver`, `field`) => value
      case expression@FieldAccessExpression(other, `field`) =>
        val equality = ReferenceComparisonExpression(receiver, other, ReferenceOperator.==)
        ConditionalExpression(equality, value, expression)
      case other => other
    }
  }

  def project(changing: Seq[VariableIdentifier], over: Expression, under: Expression): QpRecord = {
    val newPrecondition = {
      val formula = BigMax(changing, ConditionalExpression(over, precondition, No))
      QpElimination.eliminate(formula)
    }
    val newDifference = {
      val negative = Min(BigMin(changing, ConditionalExpression(over, difference, No)), No)
      val positive = Max(BigMax(changing, ConditionalExpression(under, Max(difference, No), No)), No)
      QpElimination.eliminate(Plus(negative, positive))
    }
    copy(precondition = newPrecondition, difference = newDifference)
  }

  def transform(f: Expression => Expression): QpRecord = update(_.transform(f))

  @inline
  def update(f: Expression => Expression): QpRecord = update(f, f)

  @inline
  def update(f1: Expression => Expression, f2: Expression => Expression): QpRecord =
    if (isTop || isBottom) this else copy(precondition = f1(precondition), difference = f2(difference))

}

