/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.util.{AccumulatingTimer, MapUtil}
import ch.ethz.inf.pm.sample.{SystemParameters, ToStringUtilities}
import ch.ethz.inf.pm.td.analysis
import ch.ethz.inf.pm.td.analysis.{ApiField, TouchAnalysisParameters, TouchVariablePacking}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.Set

object HeapIdentifier {

  def makeDummy(typ: Type) = HeapIdentifier(DummyProgramPoint, typ, summary = false, 0)

}

case class HeapIdentifier(pp: ProgramPoint, typ: Type, summary: Boolean, unique: Int) extends Identifier.HeapIdentifier {
  override val getName: String = pp + (if (summary) "Σ" else "") + "[v" + unique + "]" + typ.toString

  override def getField: Option[String] = None

  override def representsSingleVariable: Boolean = !summary
}

case class FieldIdentifier(obj: HeapIdentifier, field: String, typ: Type) extends Identifier.FieldIdentifier {
  override def pp: ProgramPoint = obj.pp

  override val getName: String = obj + "." + field

  override def getField: Option[String] = Some(field)

  override def representsSingleVariable: Boolean = obj.representsSingleVariable
}

/**
  * Describes an interface that is to be implemented by all states to be plugged into TouchGuru
  */
trait TouchStateInterface[T <: TouchStateInterface[T]] extends State[T] {
  this: T =>

  def ids: IdentifierSet

  def endOfFunctionCleanup(): T = this

  def getPossibleConstants(id: Identifier): SetDomain.Default[Constant]

  /**
    * For a given IdentifierSet, returns all paths of identifiers that may reach any of the given identifiers
    *
    * @param ids The identifiers to reached
    * @return The reaching heap paths
    */
  def reachingHeapPaths(ids: IdentifierSet): SetDomain.Default[List[Identifier]]

  /**
    * For a given IdentifierSet, returns all identifiers that may reach any of the given identifiers
    *
    * @param ids The identifiers to reached
    * @return The reaching heap paths
    */
  def reachingIdentifiers(ids: IdentifierSet): IdentifierSet
}


/**
  * A new state for TouchDevelop
  */
trait TouchState[S <: SemanticDomain[S], T <: TouchState[S, T]]
  extends SimpleState[T]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging
    with TouchStateInterface[T] {
  self: T =>

  val forwardMay: Map[Identifier, Set[HeapIdentifier]]
  val forwardMust: Map[Identifier, Set[HeapIdentifier]]
  val backwardMay: Map[HeapIdentifier, Set[Identifier]]
  val versions: Map[(ProgramPoint, Type), Seq[HeapIdentifier]]
  val valueState: S
  val expr: ExpressionSet
  val isTop: Boolean

  def copy(
      forwardMay: Map[Identifier, Set[HeapIdentifier]] = forwardMay,
      forwardMust: Map[Identifier, Set[HeapIdentifier]] = forwardMust,
      backwardMay: Map[HeapIdentifier, Set[Identifier]] = backwardMay,
      versions: Map[(ProgramPoint, Type), Seq[HeapIdentifier]] = versions,
      valueState: S = valueState,
      expr: ExpressionSet = expr,
      isTop: Boolean = isTop
  ): T = {

    val res = factory(forwardMay, forwardMust, backwardMay, versions, valueState, expr, isTop)

    // Invariants
    if (SystemParameters.DEBUG) {
      val isBottom = res.isBottom
      val isLessEqualBottom = res.lessEqual(res.bottom())
      val resIds = res.ids

      assert((isBottom && isLessEqualBottom) || (!isBottom && !isLessEqualBottom))
      assert(!res.forwardMay.keySet.exists(_.isInstanceOf[HeapIdentifier]))
      assert(!res.forwardMust.keySet.exists(_.isInstanceOf[HeapIdentifier]))
      //assert(res.valueState.ids.find(_.isInstanceOf[HeapIdentifier]).isEmpty)

      // ASSERT FOR EVERY HEAP IDENTIFIER, THAT ONLY EITHER A SUMMARY OR A NONSUMMARY EXISTS
      if (!resIds.isTop) {
        for (id <- resIds.getNonTop) id match {
          case h: HeapIdentifier => assert(!(resIds.contains(h) && resIds.contains(h.copy(summary = !h.summary))))
          case f: FieldIdentifier => assert(!(resIds.contains(f) && resIds.contains(f.copy(obj = f.obj.copy(summary = !f.obj.summary)))))
          case _ => ()
        }
      }

      // forward May corresponds to backward May
      for ((obj, pointers) <- res.backwardMay; pointer <- pointers) {
        assert(res.forwardMay.getOrElse(pointer, Set.empty).contains(obj))
      }
      for ((pointer, objects) <- res.forwardMay; obj <- objects) {
        assert(res.backwardMay.getOrElse(obj, Set.empty).contains(pointer))
      }

    }

    res
  }

  def factory(
      forwardMay: Map[Identifier, Set[HeapIdentifier]] = Map.empty,
      forwardMust: Map[Identifier, Set[HeapIdentifier]] = Map.empty,
      backwardMay: Map[HeapIdentifier, Set[Identifier]] = Map.empty,
      versions: Map[(ProgramPoint, Type), Seq[HeapIdentifier]] = Map.empty,
      valueState: S = valueState.factory(),
      expr: ExpressionSet = ExpressionFactory.unitExpr,
      isTop: Boolean = false
  ): T

  /** Creates a variable given a `VariableIdentifier`.
    * Implementations can already assume that this state is non-bottom.
    */
  override def createVariable(va: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    copy(
      forwardMay, // + (va -> Set.empty),
      forwardMust, // + (va -> Set.empty),
      backwardMay,
      versions,
      valueState.createVariable(va)
    ) //.garbageCollect(forwardMust.getOrElse(va,Set.empty).toList).canonicalizeEnvironment
  }

  /** Removes the given variable.
    * Implementations can assume this state is non-bottom
    */
  def removeVariables(va: Set[VariableIdentifier]): T = {
    val result = copy(
      forwardMay -- va,
      forwardMust -- va,
      backwardMay.map(x => x._1 -> (x._2 -- va)),
      versions,
      valueState.removeVariables(va)
    )
    val result2 = result.garbageCollect(va flatMap (forwardMay.getOrElse(_, Set.empty)))
    val result3 = result2.canonicalizeEnvironment

    if (SystemParameters.DEBUG) {
      val ids = result3.ids
      assert(ids.getNonTop.intersect(va.toSet[Identifier]).isEmpty)
    }

    // Garbage collection invariant
    if (SystemParameters.DEBUG) {
      for ((k, v) <- result3.backwardMay) {
        assert(v.nonEmpty)
      }
    }

    result3
  }


  override def removeVariable(va: VariableIdentifier): T = removeVariables(Set(va))

  /** Returns a new state whose `ExpressionSet` holds the value of the given field.
    * Implementations can already assume that this state is non-bottom.
    */
  override def getFieldValue(obj: Expression, field: String, typ: Type): T = obj match {
    case h: HeapIdentifier =>
      copy(
        expr = new ExpressionSet(typ, SetDomain.Default.Inner(Set(FieldIdentifier(h, field, typ))))
      )
    case a: Identifier =>
      val objects = forwardMay.getOrElse(a, Set.empty)
      if (objects.nonEmpty) {
        copy(
          expr = new ExpressionSet(typ, SetDomain.Default.Inner(objects.map(FieldIdentifier(_, field, typ))))
        )
      } else {
        copy(expr = expr.bottom())
      }
    case _ => bottom()
  }

  /**
    * Returns all objects pointed to by the field which may / must match the given filter
    *
    * @param obj    An expression containing objects
    * @param field  The name of the field
    * @param typ    The type of the field
    * @param filter The filter, that, given an object and a state, returns whether it matches
    * @return A may set and a must set of object
    */
  def getFieldValueWhere(obj: Expression, field: String, typ: Type, filter: (Identifier, T) => Boolean): (Set[Identifier], Set[Identifier]) = {
    if (SystemParameters.DEBUG)
      assert(typ.isObject)

    val objs = obj match {
      case h: HeapIdentifier => Set(h)
      case a: Identifier => forwardMay.getOrElse(a, Set.empty)
      case i: InvalidExpression => return (Set.empty, Set.empty)
    }

    val valsMay = (for (o <- objs) yield {
      val fieldTargets = forwardMay.getOrElse(FieldIdentifier(o, field, typ), Set.empty)
      fieldTargets.filter {
        filter(_, this)
      }
    }).flatten.toSet[Identifier]

    val valsMust = (for (o <- objs) yield {
      val fieldTargets = forwardMust.getOrElse(FieldIdentifier(o, field, typ), Set.empty)
      fieldTargets.filter {
        filter(_, this)
      }
    }).flatten.toSet[Identifier]

    (valsMay, valsMust)
  }

  /**
    * Returns all objects pointed to by the field which may / must match the given filter
    *
    * @param objSet An expression set containing objects
    * @param field  The name of the field
    * @param typ    The type of the field
    * @param filter The filter, that, given an object and a state, returns whether it matches
    * @return A may set and a must set of object
    */
  override def getFieldValueWhere(objSet: ExpressionSet, field: String, typ: Type, filter: (Identifier, T) => Boolean): (Set[Identifier], Set[Identifier]) = {
    val (left, right) = objSet.toSetOrFail.map(getFieldValueWhere(_, field, typ, filter)).unzip
    val (newLeft, newRight) = (left.flatten, right.flatten)
    (newLeft, newRight)
  }

  /** Assumes an expression.
    * Implementations can already assume that this state is non-bottom.
    */
  override def assume(cond: Expression): T = {

    // Short-cut to simplified assumptions
    if (cond.canonical) return assumeSimplified(cond)

    cond match {

      // This must be first -- Shortcut in simplified version
      case b@BinaryArithmeticExpression(left, right, op, typ) if !left.typ.isBooleanType && !right.typ.isBooleanType =>
        assumeSimplified(b)

      // Boolean constants
      case Constant("true", _, _) => this
      case Constant("false", _, _) => this.bottom()
      case NegatedBooleanExpression(Constant("true", _, _)) => this.bottom()
      case NegatedBooleanExpression(Constant("false", _, _)) => this
      case BinaryArithmeticExpression(Constant(a, _, _), Constant(b, _, _), ArithmeticOperator.==, _) if a == b =>
        this
      case BinaryArithmeticExpression(Constant(a, _, _), Constant(b, _, _), ArithmeticOperator.!=, _) if a == b =>
        bottom()
      case BinaryArithmeticExpression(Constant("true", _, _), Constant("false", _, _), ArithmeticOperator.==, _) =>
        bottom()
      case BinaryArithmeticExpression(Constant("false", _, _), Constant("true", _, _), ArithmeticOperator.==, _) =>
        bottom()
      case BinaryArithmeticExpression(Constant("true", _, _), Constant("false", _, _), ArithmeticOperator.!=, _) =>
        this
      case BinaryArithmeticExpression(Constant("false", _, _), Constant("true", _, _), ArithmeticOperator.!=, _) =>
        this

      // Boolean variables
      case x: Identifier =>
        if (SystemParameters.DEBUG) assert(x.typ.isBooleanType)
        val res = assume(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.!=))
        res

      case NegatedBooleanExpression(x: Identifier) =>
        if (SystemParameters.DEBUG) assert(x.typ.isBooleanType)
        val res = assume(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.==))
        res

      // And and Or
      case BinaryBooleanExpression(left, right, op, _) => op match {
        case BooleanOperator.&& =>
          val l = assume(left)
          if (l.isBottom) l
          else l.assume(right)
        case BooleanOperator.|| =>
          val l = assume(left)
          val r = assume(right)
          val res = l lub r
          res
      }

      // Double-Negation + De-Morgan
      case NegatedBooleanExpression(NegatedBooleanExpression(x)) =>
        assume(x)

      case NegatedBooleanExpression(BinaryBooleanExpression(left, right, op, typ)) =>
        val nl = NegatedBooleanExpression(left)
        val nr = NegatedBooleanExpression(right)
        val nop = op match {
          case BooleanOperator.&& => BooleanOperator.||
          case BooleanOperator.|| => BooleanOperator.&&
        }
        assume(BinaryBooleanExpression(nl, nr, nop, typ))

      // Inverting of operators
      case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
        val res = assume(BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op), typ))
        res

      // Handling of monomes
      case _ =>
        cond.canonical = true
        assumeSimplified(cond)
    }

  }

  def assumeSimplified(expr: Expression): T = {

    expr match {
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.==, _)
        if left.typ.isObject && left.isInstanceOf[Identifier]
          && right.typ.isObject && right.isInstanceOf[Identifier] => // TODO: Reference equality?
        if ((forwardMay.getOrElse(left.asInstanceOf[Identifier], Set.empty)
          intersect forwardMay.getOrElse(right.asInstanceOf[Identifier], Set.empty)).isEmpty) {
          bottom()
        } else this
      case _ =>
        val result = copy(
          valueState = valueState.assume(expr)
        )
        result
    }
  }

  /** Assigns an expression to a variable.
    * Implementations can already assume that this state is non-bottom.
    */
  override def assignVariable(left: Expression, right: Expression): T = left match {
    case leftVariable: VariableIdentifier =>
      if (leftVariable.representsSingleVariable) {
        // strong update
        right match {
          case rightHeap: HeapIdentifier => strongUpdateReference(leftVariable, rightHeap)
          case rightIdentifier: Identifier => strongUpdateAlias(leftVariable, rightIdentifier)
          case _ => strongUpdateValue(leftVariable, right)
        }
      } else {
        // weak update
        right match {
          case rightHeap: HeapIdentifier => weakUpdateReference(leftVariable, rightHeap)
          case rightIdentifier: Identifier => weakUpdateAlias(leftVariable, rightIdentifier)
          case _ => weakUpdateValue(leftVariable, right)
        }
      }
    case leftField: FieldIdentifier =>
      assignField(leftField.obj, leftField.field, right)
    case _: ValidExpression =>
      this
    case _: InvalidExpression =>
      this
  }

  /** Assigns an expression to a field.
    * Implementations can already assume that this state is non-bottom.
    */
  override def assignField(obj: Expression, field: String, right: Expression): T = obj match {
    case leftHeap: HeapIdentifier =>
      obj.typ.representedFields.find(_.getName == field) match {
        case Some(declaration) =>
          val leftField = fieldFromString(leftHeap, field)
          if (leftHeap.representsSingleVariable && !declaration.asInstanceOf[ApiField].isAccumulating) {
            // strong update
            right match {
              case rightHeap: HeapIdentifier => strongUpdateReference(leftField, rightHeap)
              case rightIdentifier: Identifier => strongUpdateAlias(leftField, rightIdentifier)
              case _ => strongUpdateValue(leftField, right)
            }
          } else {
            // weak update
            right match {
              case rightHeap: HeapIdentifier => weakUpdateReference(leftField, rightHeap)
              case rightIdentifier: Identifier => weakUpdateAlias(leftField, rightIdentifier)
              case _ => weakUpdateValue(leftField, right)
            }
          }
        case None => this
      }
    case leftIdentifier: Identifier =>
      forwardMay.get(leftIdentifier) match {
        case Some(set) if set.nonEmpty =>
          Lattice.bigLub(set.map {
            assignField(_, field, right)
          })
        case _ =>
          bottom()
      }
    case _ => bottom()
  }

  /**
    * Removes the current expression
    *
    * @return The abstract state after removing the current expression
    */
  override def removeExpression(): T = copy(expr = ExpressionFactory.unitExpr)

  /**
    * Removes all variables satisfying filter
    */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T =
  ids match {
    case IdentifierSet.Bottom => this
    case IdentifierSet.Top => this
    case IdentifierSet.Inner(v) =>
      removeVariables(v.collect {
        case vs: VariableIdentifier if filter(vs) => vs
      })
  }

  /**
    * Evaluates a numerical constant
    *
    * @param value The string representing the numerical constant
    * @param typ   The type of the numerical constant
    * @param pp    The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the state that contains an expression representing this constant
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = {
    this.setExpression(ExpressionSet(Constant(value, typ, pp)))
  }

  /**
    * Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): T = {
    val oldVersions = versions.getOrElse((pp, typ), Seq.empty[HeapIdentifier])
    val unique = oldVersions.lastOption match {
      case Some(x) => x.unique + 1
      case None => 0
    }
    val id = HeapIdentifier(pp, typ, summary = false, unique)
    val newVersions = versions.getOrElse((pp, typ), Seq.empty[HeapIdentifier]) :+ id

    val fieldIdentifiers = for (field <- typ.representedFields) yield {
      FieldIdentifier(id, field.getField.get, field.typ).asInstanceOf[Identifier]
    }

    val fieldIdentifierObjects = fieldIdentifiers.filter(_.typ.isObject)
    val fieldAssignment = fieldIdentifierObjects.map(_ -> Set.empty[HeapIdentifier])

    val result = copy(
      forwardMay = forwardMay ++ fieldAssignment,
      forwardMust = forwardMust ++ fieldAssignment,
      versions = versions + ((pp, typ) -> newVersions),
      valueState = valueState.createVariables(fieldIdentifiers),
      expr = new ExpressionSet(typ).add(id)
    ).boundVersions.canonicalizeEnvironment

    // POST CONDITIONS
    if (SystemParameters.DEBUG) {
      if (!result.ids.isTop) {
        // May not contains versions higher than k
        for (id <- result.ids.getNonTop) {
          id match {
            case x: HeapIdentifier =>
              assert(x.unique < TouchAnalysisParameters.get.numberOfVersions)
            case _ => ()
          }
        }
      }
    }

    result
  }

  /**
    * Sets the current expression
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): T = {
    copy(expr = expr)
  }

  /**
    * Gets the value of a variable
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): T = copy(expr = new ExpressionSet(id.typ).add(id))

  /**
    * Returns the top value of the lattice
    *
    * @return The top value, that is, a value x that is greater or equal than any other value
    */
  override def top(): T = factory(valueState = valueState.top(), expr = expr.top(), isTop = true)

  /**
    * Returns the bottom value of the lattice
    *
    * @return The bottom value, that is, a value x that is less or equal than any other value
    */
  override def bottom(): T = factory(valueState = valueState.bottom(), expr = expr.bottom(), isTop = false)


  /**
    * Computes the upper bound of two elements
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater or equal than the two arguments
    */
  override def lub(other: T): T = {

    if (this eq other) return this
    if (isBottom) return other
    if (other.isBottom) return this
    if (isTop) return this
    if (other.isTop) return other

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.lub")

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.lub.adaptEnv")
    val (left, right) = adaptEnvironments(this, other)
    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.lub.adaptEnv")

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.lub.valueState")
    val valueStateLub = left.valueState.lub(right.valueState)
    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.lub.valueState")

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.lub.forward")
    val forward = MapUtil.mapToSetUnion(left.forwardMay, right.forwardMay)
    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.lub.forward")

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.lub.must")
    val must = MapUtil.mapToSetIntersectionKeepUndefined(left.forwardMust, right.forwardMust)
    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.lub.must")

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.lub.backward")
    val backward = MapUtil.mapToSetUnion(left.backwardMay, right.backwardMay)
    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.lub.backward")

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.lub.versions")
    val versions = MapUtil.mapToSeqKeepLonger(left.versions, right.versions)
    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.lub.versions")

    val result = factory(
      forward,
      must,
      backward,
      versions,
      valueStateLub,
      left.expr.lub(right.expr),
      left.isTop || right.isTop
    )

    // POST CONDITIONS
    if (SystemParameters.DEBUG) {

      // Garbage collection invariant
      for ((k, v) <- result.backwardMay) {
        assert(v.nonEmpty)
      }
    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.lub")

    result
  }

  /**
    * Computes widening of two elements
    *
    * @param other The new value
    * @return The widening of <code>left</code> and <code>right</code>
    */
  override def widening(other: T): T = {

    if (this eq other) return this
    if (isBottom) return other
    if (other.isBottom) return this
    if (isTop) return this
    if (other.isTop) return other

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.widening")

    val (left, right) = adaptEnvironments(this, other)

    val result = factory(
      MapUtil.mapToSetUnion(left.forwardMay, right.forwardMay),
      MapUtil.mapToSetIntersectionKeepUndefined(left.forwardMust, right.forwardMust),
      MapUtil.mapToSetUnion(left.backwardMay, right.backwardMay),
      MapUtil.mapToSeqKeepLonger(left.versions, right.versions),
      left.valueState.widening(right.valueState),
      left.expr.widening(right.expr),
      left.isTop || right.isTop
    )

    // POST CONDITIONS
    if (SystemParameters.DEBUG) {

      // Garbage collection invariant
      for ((k, v) <- result.backwardMay) {
        assert(v.nonEmpty)
      }

    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.widening")

    result
  }

  /**
    * Returns true iff <code>this</code> is less or equal than <code>r</code>
    *
    * @param r The value to compare
    * @return true iff <code>this</code> is less or equal than <code>r</code>
    */
  override def lessEqual(r: T): Boolean = {

    if (isBottom || r.isTop) return true
    if (r.isBottom || isTop) return false

    val (left, right) = adaptEnvironments(this, r)
    if (!MapUtil.mapToSetContainment(left.forwardMay, right.forwardMay)) return false
    if (!MapUtil.mapToSetContainment(right.forwardMust, left.forwardMust)) return false
    // no check on versions. however, this should be fine?
    if (!left.valueState.lessEqual(right.valueState)) return false
    if (!left.expr.lessEqual(right.expr)) return false

    true
  }

  override def isBottom: Boolean = {

    if (isTop) return false
    //    if (forwardMay.exists( _._2.isEmpty))
    //      return true
    if (valueState.isBottom) return true
    false

  }


  def mergeForwards(rep: Replacement, map: Map[Identifier, Set[HeapIdentifier]], revMap: Map[HeapIdentifier, Set[Identifier]]): Map[Identifier, Set[HeapIdentifier]] = {

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.mergeForwards")

    var localRevMap = revMap
    if (rep.isEmpty()) return map
    var result = map
    for ((left, right) <- rep.value) {
      // compute the lub of everything on the left side
      val lub = left.foldLeft(Set.empty[HeapIdentifier])((s, l) => l match {
        case a: Identifier => s ++ map.getOrElse(a, Set.empty);
        case _ => s
      })
      // assign it to everything on the right side
      for (r <- right) {
        r match {
          case a: HeapIdentifier => ()
          case a: Identifier => result = result + (a -> lub)
          case _ => ()
        }
      }
      // update local reverse map
      for (l <- lub) {
        localRevMap = localRevMap + (l -> (localRevMap.getOrElse(l, Set.empty) ++ right))
      }
    }

    // remove everything on the left side in the keys of the map
    val toBeRemoved = rep.value.keySet.flatten -- rep.value.values.flatten
    result = result -- toBeRemoved

    // perform replacements on the right side
    for ((l, r) <- rep.value) {
      val toBeReplacedSet = l.view.collect { case x: HeapIdentifier => x }.toSet
      val byThis = r.view.collect { case x: HeapIdentifier => x }.toSet

      // look it up using the reverse map
      for (toBeReplaced <- toBeReplacedSet; pointer <- localRevMap.getOrElse(toBeReplaced, Set.empty); if !toBeRemoved.contains(pointer)) {
        result = result + (pointer -> ((result.getOrElse(pointer, Set.empty) -- toBeReplacedSet) ++ byThis))
      }
    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.mergeForwards")

    result
  }

  def mergeBackwards(rep: Replacement, map: Map[HeapIdentifier, Set[Identifier]], revMap: Map[Identifier, Set[HeapIdentifier]]): Map[HeapIdentifier, Set[Identifier]] = {

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.mergeBackwards")

    if (rep.isEmpty()) return map
    var localRevMap = revMap
    val mapKeys = map.keySet.toSet[Identifier]
    val lefts = rep.value.keySet.flatten.collect { case x: HeapIdentifier => x }
    val rights = rep.value.values.flatten.collect { case x: HeapIdentifier => x }
    val toBeRemoved = lefts.toSet -- rights

    var result = map
    for ((left, right) <- rep.value if (left intersect mapKeys).nonEmpty) {
      // compute the lub of everything on the left side
      val lub = left.foldLeft(Set.empty[Identifier])((s, l) => l match {
        case a: HeapIdentifier => s ++ map.getOrElse(a, Set.empty);
        case _ => s
      })
      // assign it to everything on the right side
      for (r <- right) {
        r match {
          case a: HeapIdentifier =>
            result = result + (a -> lub)
          case _ => ()
        }
      }
      // update local reverse map
      for (l <- lub) {
        localRevMap = localRevMap + (l -> (localRevMap.getOrElse(l, Set.empty) ++ right.view.collect { case x: HeapIdentifier => x }))
      }
    }

    // remove everything on the left side in the keys of the map
    result = result -- toBeRemoved

    // perform replacements on the right side
    for ((l, r) <- rep.value) {
      val toBeReplacedSet = l.view.filter { case x: HeapIdentifier => false; case _ => true }.toSet
      val byThis = r.view.filter { case x: HeapIdentifier => false; case _ => true }.toSet

      // look it up using the reverse map
      for (toBeReplaced <- toBeReplacedSet; pointer <- localRevMap.getOrElse(toBeReplaced, Set.empty); if !toBeRemoved.contains(pointer)) {
        result = result + (pointer -> ((result.getOrElse(pointer, Set.empty) -- toBeReplacedSet) ++ byThis))
      }

    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.mergeBackwards")

    result
  }


  def mergeVersions(rep: Replacement, map: Map[(ProgramPoint, Type), Seq[HeapIdentifier]]): Map[(ProgramPoint, Type), Seq[HeapIdentifier]] = {


    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.mergeVersions")

    var cur = map
    for ((ls, rs) <- rep.value) {
      for (l <- ls) {

        l match {

          case h: HeapIdentifier =>

            // replace all, but only add one copy of the right side
            val versions = cur((h.pp, h.typ)) // must exist
          var already = false // ugly, works.
          val newVersions = versions.foldRight(Seq.empty[HeapIdentifier])(
            (a, b) =>
              if (ls.contains(a)) {
                if (already) b
                else {
                  already = true
                  rs.toSeq.sortBy(_.toString).map(_.asInstanceOf[HeapIdentifier]) ++: b // tooo slow
                }
              } else a +: b
          )
            cur = cur + ((l.pp, l.typ) -> newVersions)

          case _ => ()

        }

      }
    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.mergeVersions")

    cur
  }

  override def merge(r: Replacement): T = {

    if (r.isEmpty()) return this

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.merge")

    val result = copy(
      mergeForwards(r, forwardMay, backwardMay),
      mergeForwards(r, forwardMust, backwardMay),
      mergeBackwards(r, backwardMay, forwardMay),
      mergeVersions(r, versions),
      valueState.merge(r),
      expr.merge(r)
    )

    // DEBUG CODE
    if (SystemParameters.DEBUG) {
      val removedIdentifiers = r.removedIdentifiers
      val ids = result.ids.getNonTop
      val remaining = ids intersect removedIdentifiers
      assert(remaining.isEmpty)
      // The following is not always true, if we have no domain that wants to represent the identifier (no value domain)
      //val addedIdentifiers = r.addedIdentifiers
      //val nonAdded = addedIdentifiers -- ids
      //assert (nonAdded.isEmpty)
    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.merge")

    result
  }

  override def pruneUnreachableHeap(): T = this

  // garbage collection is immediate
  override def before(pp: ProgramPoint): T = this

  // nothing to be done
  override def factory(): T = factory(valueState = valueState.factory())

  override def glb(other: T): T = ???

  override def setVariableToTop(varExpr: Expression): T = ???

  override def setArgument(x: ExpressionSet, right: ExpressionSet): T =
    throw new NotImplementedError("TouchDevelop does not use the argument interface")

  override def createVariableForArgument(x: VariableIdentifier, typ: Type): T =
    throw new NotImplementedError("TouchDevelop does not use the argument interface")

  override def throws(t: ExpressionSet): T =
    throw new NotImplementedError("TouchDevelop does not use exceptions")

  def removeObjects(nodes: Set[HeapIdentifier]): T = {
    val fields = nodes.flatMap(fieldsOf)
    copy(
      (forwardMay -- fields).map { x => x._1 -> (x._2 -- nodes) },
      (forwardMust -- fields).map { x => x._1 -> (x._2 -- nodes) },
      (backwardMay -- nodes).map { x => x._1 -> (x._2 -- fields) },
      nodes.foldLeft(versions) {
        (curVersions, node) =>
          val newVersions = curVersions.getOrElse((node.pp, node.typ), Seq.empty).filter(_ != node)
          if (newVersions.nonEmpty)
            curVersions + ((node.pp, node.typ) -> newVersions)
          else
            curVersions - ((node.pp, node.typ))
      },
      valueState.removeVariables(fields ++ nodes)
    )
  }

  /** Prune all objects that have no references, starting from startNode */
  def garbageCollect(nodes: Set[HeapIdentifier]): T = {

    if (nodes.isEmpty) return this
    val toRemove = nodes filter (x => backwardMay.get(x) match {
      case None => true;
      case Some(objs) => objs.isEmpty
    })
    val nextInLine = toRemove.flatMap(fieldsOf).flatMap(forwardMay.getOrElse(_, Set.empty)) -- toRemove
    val result = removeObjects(toRemove).garbageCollect(nextInLine)

    if (SystemParameters.DEBUG) {
      val ids = result.ids
      for (n <- nodes) {
        if (result.backwardMay.getOrElse(n, Set.empty).isEmpty) {
          // unreachable
          assert(!ids.contains(n))
          for (m <- fieldsOf(n)) {
            assert(!ids.contains(m))
          }
        }
      }
    }

    result
  }

  def fieldsOf(node: HeapIdentifier): Set[FieldIdentifier] = {
    node.typ.representedFields map {
      case field: ApiField => FieldIdentifier(node, field.getField.get, field.typ)
    }
  }

  def fieldFromString(node: HeapIdentifier, field: String): FieldIdentifier = {
    val tField = node.typ.representedFields.find {
      _.getField.get == field
    }.get
    FieldIdentifier(node, tField.getField.get, tField.typ)
  }


  def strongUpdateAlias(left: Identifier, right: Identifier): T = {
    // copy references from other variable/field
    val newObjectsMay = forwardMay.getOrElse(right, Set.empty)
    val newObjectsMust = forwardMust.getOrElse(right, Set.empty)
    val result = copy(
      forwardMay + (left -> newObjectsMay),
      forwardMust + (left -> newObjectsMust),
      (backwardMay map { x => x._1 -> (x._2 - left) }) ++ (newObjectsMay map { x => x -> (backwardMay.getOrElse(x, Set.empty) + left) }),
      versions,
      valueState.assign(left, right),
      ExpressionFactory.unitExpr
    ).garbageCollect(forwardMay.getOrElse(left, Set.empty)).canonicalizeEnvironment
    result
  }


  def strongUpdateReference(left: Identifier, right: HeapIdentifier): T = {
    // reference object
    val afterUpdate = this.copy(
      forwardMay + (left -> Set(right)),
      forwardMust + (left -> Set(right)),
      (backwardMay map { x => x._1 -> (x._2 - left) }) + (right -> (backwardMay.getOrElse(right, Set.empty) + left)),
      versions,
      valueState.assign(left, ValidExpression(left.typ, left.pp)),
      ExpressionFactory.unitExpr
    )
    val afterGarbageCollect = afterUpdate.garbageCollect(forwardMay.getOrElse(left, Set.empty))
    val canonicalized = afterGarbageCollect.canonicalizeEnvironment
    canonicalized
  }

  def strongUpdateValue(left: Identifier, right: Expression): T = {
    // reference object
    val res = copy(
      forwardMay,
      forwardMust,
      backwardMay,
      versions,
      valueState.assign(left, right),
      ExpressionFactory.unitExpr
    )
    res
  }

  def weakUpdateReference(left: Identifier, right: HeapIdentifier): T = {
    // reference object
    copy(
      forwardMay + (left -> (forwardMay.getOrElse(left, Set.empty) + right)),
      forwardMust + (left -> (forwardMust.getOrElse(left, Set.empty) + right)),
      backwardMay + (right -> (backwardMay.getOrElse(right, Set.empty) + left)),
      versions,
      valueState.assign(left, ValidExpression(left.typ, left.pp)).lub(valueState),
      ExpressionFactory.unitExpr
    )
    // nothing may become unreachable
  }

  def weakUpdateAlias(left: Identifier, right: Identifier): T = {
    // copy references from other variable/field
    val newObjectsMay = forwardMay.getOrElse(right, Set.empty)
    val newObjectsMust = forwardMust.getOrElse(right, Set.empty)
    copy(
      forwardMay + (left -> (forwardMay.getOrElse(left, Set.empty) ++ newObjectsMay)),
      forwardMust + (left -> (forwardMust.getOrElse(left, Set.empty) ++ newObjectsMust)),
      backwardMay ++ (newObjectsMay map { x => x -> (backwardMay.getOrElse(x, Set.empty) + left) }),
      versions,
      valueState.assign(left, right).lub(valueState),
      ExpressionFactory.unitExpr
    )
    // nothing may become unreachable
  }

  def weakUpdateValue(left: Identifier, right: Expression): T = {
    // reference object
    copy(
      forwardMay,
      forwardMust,
      backwardMay,
      versions,
      valueState.assign(left, right).lub(valueState),
      ExpressionFactory.unitExpr
    )
  }

  def ids: IdentifierSet = {
    valueState.ids ++ (forwardMay.keySet ++ forwardMay.values.flatten)
  }

  /**
    * Cut sequences and merge to summary nodes
    *
    * @return
    */
  def boundVersions: T = {

    val replacement = new Replacement()

    for ((pp, typ) <- versions.keys) {
      val theseVersions = versions.getOrElse((pp, typ), Seq.empty)
      if (theseVersions.length > TouchAnalysisParameters.get.numberOfVersions) {

        // Merge objects
        val summaryID = HeapIdentifier(pp, typ, summary = true, theseVersions.head.unique)
        val objectsToBeMerged = theseVersions.take(theseVersions.length - TouchAnalysisParameters.get.numberOfVersions + 1).toSet[Identifier]
        replacement.value += (objectsToBeMerged -> Set[Identifier](summaryID))

        // Merge fields
        for (field <- typ.representedFields) {
          val summaryField = FieldIdentifier(summaryID, field.getField.get, field.typ)
          val fieldsToBeMerged = objectsToBeMerged.map { x => FieldIdentifier(x.asInstanceOf[HeapIdentifier], field.getField.get, field.typ) }.toSet[Identifier]
          replacement.value += (fieldsToBeMerged -> Set[Identifier](summaryField))
        }

      }
    }

    merge(replacement)
  }

  /**
    * Makes sure that versions are called according to their indexes in the list
    */
  def canonicalizeEnvironment: T = {

    if (SystemParameters.TIME) AccumulatingTimer.start("TouchState.canonicalizeEnvironment")

    var curState = this

    for ((k, v) <- versions) {
      for ((x, i) <- v.view.zipWithIndex) {
        if (x.unique != i) {
          val rep = new Replacement(isPureRenaming = true)
          val newObject = x.copy(unique = i)
          rep.value(Set(x)) = Set(newObject.asInstanceOf[Identifier])
          for (field <- x.typ.representedFields) {
            val from = FieldIdentifier(x, field.getField.get, field.typ)
            val to = FieldIdentifier(newObject, field.getField.get, field.typ)
            if (SystemParameters.DEBUG) assert(!curState.ids.contains(to))
            rep.value(Set(from).toSet[Identifier]) = Set(to).toSet[Identifier]
          }
          curState = curState.merge(rep)
        }
      }
    }


    // POST Conditions
    if (SystemParameters.DEBUG) {

      for ((k, v) <- curState.versions) {
        for ((x, i) <- v.view.zipWithIndex) {
          assert(x.unique == i)
        }
      }

      // Garbage collection invariant
      for ((k, v) <- curState.backwardMay) {
        assert(v.nonEmpty)
      }

    }

    if (SystemParameters.TIME) AccumulatingTimer.stop("TouchState.canonicalizeEnvironment")

    curState

  }

  /**
    * Makes sure left is summary iff right is summary
    *
    * precondition: l and r canonicalized!
    */
  def adaptSummaryNodes(l: T, r: T): (T, T) = {

    val replaceLeft = new Replacement(isPureRenaming = true)
    val replaceRight = new Replacement(isPureRenaming = true)

    for (
      pp <- l.versions.keySet intersect r.versions.keySet;
      (left, right) = (l.versions(pp), r.versions(pp))
      if left.nonEmpty && right.nonEmpty
    ) {

      if (left.head.summary && !right.head.summary) {
        replaceRight.value(Set[Identifier](right.head)) = Set[Identifier](left.head)
        for (f <- fieldsOf(right.head)) replaceRight.value(Set[Identifier](f)) = Set[Identifier](f.copy(obj = left.head))
      } else if (!left.head.summary && right.head.summary) {
        replaceLeft.value(Set[Identifier](left.head)) = Set[Identifier](right.head)
        for (f <- fieldsOf(left.head)) replaceLeft.value(Set[Identifier](f)) = Set[Identifier](f.copy(obj = right.head))
      }

    }

    (l.merge(replaceLeft), r.merge(replaceRight))

  }

  def adaptEnvironments(l: T, r: T): (T, T) = {
    adaptSummaryNodes(l, r)
  }

  override def toString: String = {

    "Points-To:\n" +
      ToStringUtilities.indent(forwardMay.map {
        case (a, b) =>
          val must = forwardMust.getOrElse(a, Set.empty)
          a.toString + " -> " + b.map { x => x.toString + (if (must.contains(x)) " (must)" else " (may)") }.mkString(", ")
      }.mkString("\n")) +
      "\nPointed-By:\n" +
      ToStringUtilities.indent(backwardMay.map {
        case (a, b) =>
          a.toString + " -> " + b.map { x => x.toString }.mkString(", ")
      }.mkString("\n")) +
      "\nVersions:\n" +
      ToStringUtilities.indent(versions.map {
        case (a, b) =>
          a.toString + " -> " + b.map { x => x.unique + (if (x.summary) "Σ" else "") }.mkString(", ")
      }.mkString("\n")) +
      "\nValues:\n" +
      ToStringUtilities.indent(valueState.toString) +
      "\nExpression: " + expr.toString

  }

  /**
    * Graph-like interface
    */
  lazy val vertices: Set[Identifier] = forwardMay.flatMap { case (a, b) => b.toSet[Identifier] + a }.toSet

  lazy val edges: Set[(Identifier, String, Identifier)] = {
    (forwardMay.flatMap { case (a, b) => b.map((a, "may", _)) } ++
      forwardMust.flatMap { case (a, b) => b.map((a, "must", _)) } ++
      backwardMay.flatMap { case (a, b) => b.map((a, "back", _)) } ++
      (for (v <- vertices) yield {
        v match {
          case x: FieldIdentifier => Some((x.obj, "field", x))
          case _ => None
        }
      }).flatten).toSet
  }

  override def getPossibleConstants(id: Identifier) = valueState.getPossibleConstants(id)

  override def reachingIdentifiers(ids: IdentifierSet): IdentifierSet = {
    Lattice.lfp[IdentifierSet](ids, {
      case IdentifierSet.Top => IdentifierSet.Top
      case IdentifierSet.Bottom => IdentifierSet.Bottom
      case IdentifierSet.Inner(inner) =>
        IdentifierSet.Inner(
          inner.flatMap {
            case i: HeapIdentifier =>
              backwardMay.getOrElse(i, Set.empty) + i
            case f@FieldIdentifier(obj, _, _) =>
              backwardMay.getOrElse(obj, Set.empty) + f
            case x =>
              Set(x)
          }
        )
    }, 99)
  }

  /**
    * For a given IdentifierSet, returns all paths of identifiers that may reach any of the given identifiers
    *
    * @param ids The identifiers to reached
    * @return The reaching heap paths
    */
  override def reachingHeapPaths(ids: IdentifierSet): SetDomain.Default[List[Identifier]] = {
    ids match {

      case IdentifierSet.Top => SetDomain.Default.Top[List[Identifier]]()

      case IdentifierSet.Bottom => SetDomain.Default.Bottom[List[Identifier]]()

      case IdentifierSet.Inner(v) =>

        var res: Set[List[Identifier]] = v.map(List(_))
        var toContinue = res.filter(!_.head.isInstanceOf[VariableIdentifier])
        while (toContinue.nonEmpty) {
          val continued =
            toContinue.flatMap { x =>
              x.head match {
                case i: HeapIdentifier =>
                  backwardMay.getOrElse(i, Set.empty).map(y => if (x.contains(y)) x else y :: x)
                case f@FieldIdentifier(obj, _, _) =>
                  backwardMay.getOrElse(obj, Set.empty).map(y => if (x.contains(y)) x else y :: x)
                case _ => Set(x)
              }
            }
          res = (res -- toContinue) ++ continued
          toContinue = continued.filter(!_.head.isInstanceOf[VariableIdentifier]) -- toContinue // make sure we terminate in cycles
        }
        SetDomain.Default.Inner(res)
    }
  }
}

object TouchState {

  case class Default[S <: SemanticDomain[S]](
      forwardMay: Map[Identifier, Set[HeapIdentifier]] = Map.empty,
      forwardMust: Map[Identifier, Set[HeapIdentifier]] = Map.empty,
      backwardMay: Map[HeapIdentifier, Set[Identifier]] = Map.empty,
      versions: Map[(ProgramPoint, Type), Seq[HeapIdentifier]] = Map.empty,
      valueState: S,
      expr: ExpressionSet = ExpressionFactory.unitExpr,
      isTop: Boolean = false
  )
    extends TouchState[S, Default[S]] {

    def factory(
        forwardMay: Map[Identifier, Set[HeapIdentifier]] = Map.empty,
        forwardMust: Map[Identifier, Set[HeapIdentifier]] = Map.empty,
        backwardMay: Map[HeapIdentifier, Set[Identifier]] = Map.empty,
        versions: Map[(ProgramPoint, Type), Seq[HeapIdentifier]] = Map.empty,
        valueState: S,
        expr: ExpressionSet = ExpressionFactory.unitExpr,
        isTop: Boolean = false
    ): Default[S] =
      Default(forwardMay, forwardMust, backwardMay, versions, valueState, expr, isTop)

  }

  trait CollectingDomain
    extends SimplifiedSemanticDomain[CollectingDomain] {

    override def assign(id: Identifier, expr: Expression): CollectingDomain = {
      TouchVariablePacking.pack(expr.ids + id)
      factory(ids ++ expr.ids)
    }

    override def assume(cond: Expression): CollectingDomain = {
      TouchVariablePacking.pack(cond.ids)
      this
    }

    override def merge(rep: Replacement): CollectingDomain = {
      for (r <- rep.value) {
        TouchVariablePacking.pack(IdentifierSet.Inner(r._1 ++ r._2))
      }
      factory(ids ++ IdentifierSet.Inner(rep.value.flatMap(_._2).toSet))
    }

    def factory(id: Identifier) = CollectingDomain.Inner(IdentifierSet.Inner(Set(id)))

    def factory(ids: IdentifierSet) = CollectingDomain.Inner(ids)

    override def factory(): CollectingDomain = factory(IdentifierSet.Bottom)

    override def bottom(): CollectingDomain = CollectingDomain.Bottom

    override def top(): CollectingDomain = CollectingDomain.Top
  }

  object CollectingDomain {

    object Top extends CollectingDomain with SemanticDomain.Top[CollectingDomain]

    object Bottom extends CollectingDomain with SemanticDomain.Bottom[CollectingDomain] {
      override def createVariable(variable: Identifier, typ: Type) = factory(variable)
    }

    case class Inner(ids: IdentifierSet) extends CollectingDomain with SemanticDomain.Inner[CollectingDomain, CollectingDomain.Inner] {
      override def lubInner(other: CollectingDomain.Inner) = CollectingDomain.Inner(other.ids ++ ids)

      override def glbInner(other: CollectingDomain.Inner) = CollectingDomain.Inner(other.ids glb ids)

      override def wideningInner(other: CollectingDomain.Inner): CollectingDomain = CollectingDomain.Inner(other.ids widening ids)

      override def lessEqualInner(other: CollectingDomain.Inner) = other.ids lessEqual ids

      override def setToTop(variable: Identifier) = CollectingDomain.Inner(ids + variable)

      override def removeVariable(id: Identifier) = CollectingDomain.Inner(ids - id)

      override def createVariable(variable: Identifier, typ: Type) = CollectingDomain.Inner(ids + variable)

      override def getStringOfId(id: Identifier) = ""

      override def getPossibleConstants(id: Identifier) = SetDomain.Default.Top()

    }

  }


  /**
    * Implements a touch state which does not track values at all
    */
  case class PreAnalysis[S <: SemanticDomain[S]](
      forwardMay: Map[Identifier, Set[HeapIdentifier]] = Map.empty,
      forwardMust: Map[Identifier, Set[HeapIdentifier]] = Map.empty,
      backwardMay: Map[HeapIdentifier, Set[Identifier]] = Map.empty,
      versions: Map[(ProgramPoint, Type), Seq[HeapIdentifier]] = Map.empty,
      valueState: S,
      expr: ExpressionSet = ExpressionFactory.unitExpr,
      isTop: Boolean = false,
      inLoops: Set[ProgramPoint] = Set.empty,
      notInLoops: Set[ProgramPoint] = Set.empty
  )
    extends TouchState[S, PreAnalysis[S]] {

    def factory(
        forwardMay: Map[Identifier, Set[HeapIdentifier]] = Map.empty,
        forwardMust: Map[Identifier, Set[HeapIdentifier]] = Map.empty,
        backwardMay: Map[HeapIdentifier, Set[Identifier]] = Map.empty,
        versions: Map[(ProgramPoint, Type), Seq[HeapIdentifier]] = Map.empty,
        valueState: S,
        expr: ExpressionSet = ExpressionFactory.unitExpr,
        isTop: Boolean = false
    ): PreAnalysis[S] =
      PreAnalysis(forwardMay, forwardMust, backwardMay, versions, valueState, expr, isTop)


    override def copy(
        forwardMay: Map[Identifier, Set[HeapIdentifier]] = forwardMay,
        forwardMust: Map[Identifier, Set[HeapIdentifier]] = forwardMust,
        backwardMay: Map[HeapIdentifier, Set[Identifier]] = backwardMay,
        versions: Map[(ProgramPoint, Type), Seq[HeapIdentifier]] = versions,
        valueState: S = valueState,
        expr: ExpressionSet = expr,
        isTop: Boolean = isTop
    ): PreAnalysis[S] =
      PreAnalysis(forwardMay, forwardMust, backwardMay, versions, valueState, expr, isTop, inLoops, notInLoops)


    def copyLocal(
        forwardMay: Map[Identifier, Set[HeapIdentifier]] = forwardMay,
        forwardMust: Map[Identifier, Set[HeapIdentifier]] = forwardMust,
        backwardMay: Map[HeapIdentifier, Set[Identifier]] = backwardMay,
        versions: Map[(ProgramPoint, Type), Seq[HeapIdentifier]] = versions,
        valueState: S = valueState,
        expr: ExpressionSet = expr,
        isTop: Boolean = isTop,
        inLoops: Set[ProgramPoint] = inLoops,
        notInLoops: Set[ProgramPoint] = notInLoops
    ): PreAnalysis[S] =
      PreAnalysis(forwardMay, forwardMust, backwardMay, versions, valueState, expr, isTop, inLoops, notInLoops)

    override def getFieldValue(obj: Expression, field: String, typ: Type): PreAnalysis[S] = {
      analysis.Localization.collectAccess(obj.ids)
      super.getFieldValue(obj, field, typ)
    }

    override def getFieldValueWhere(obj: Expression, field: String, typ: Type, filter: (Identifier, PreAnalysis[S]) => Boolean): (Predef.Set[Identifier], Predef.Set[Identifier]) = {
      analysis.Localization.collectAccess(obj.ids)
      super.getFieldValueWhere(obj, field, typ, filter)
    }

    override def assume(cond: Expression): PreAnalysis[S] = {
      analysis.Localization.collectAccess(cond.ids)
      super.assume(cond)
    }

    override def assignVariable(left: Expression, right: Expression): PreAnalysis[S] = {
      analysis.Localization.collectAccess(left.ids ++ right.ids)
      val reAssigned = left.ids glb right.ids
      if (inLoops.nonEmpty && !reAssigned.isBottom) {
        TouchVariablePacking.collectLoopAssign(reAssigned, inLoops)
        logger.debug("Variables " + left.ids + " seem to be reassigned in loops " + inLoops.mkString(","))
      }
      super.assignVariable(left, right)
    }

    override def assignField(obj: Expression, field: String, right: Expression): PreAnalysis[S] = {
      analysis.Localization.collectAccess(obj.ids ++ right.ids)
      val leftIds = getFieldValue(obj, field, right.typ).expr.ids
      val reAssigned = leftIds glb right.ids
      if (inLoops.nonEmpty && !reAssigned.isBottom) {
        TouchVariablePacking.collectLoopAssign(reAssigned, inLoops)
        logger.debug("Fields " + leftIds + " seem to be reassigned in loops " + inLoops.mkString(","))
      }
      super.assignField(obj, field, right)
    }

    override def getVariableValue(id: Identifier): PreAnalysis[S] = {
      analysis.Localization.collectAccess(id.asInstanceOf[Identifier])
      super.getVariableValue(id)
    }

    override def setVariableToTop(varExpr: Expression): PreAnalysis[S] = {
      analysis.Localization.collectAccess(varExpr.ids)
      super.setVariableToTop(varExpr)
    }

    /** We may jump in a loop here - reset stats, pack variables */
    override def testTrue() = {
      val res = super.testTrue()
      expr._2 match {
        case SetDomain.Default.Top() => res
        case SetDomain.Default.Bottom() => res
        case SetDomain.Default.Inner(xs) =>
          val points = xs.map(_.pp)
          TouchVariablePacking.packLoopHeads(points, Lattice.bigLub(xs.map(_.ids)), valueState.ids)
          res.copyLocal(inLoops = inLoops ++ points, notInLoops = notInLoops -- points)
      }
    }

    /** We may jump out of a loop here - reset stats */
    override def testFalse() = {
      val res = super.testFalse()
      expr._2 match {
        case SetDomain.Default.Top() => res
        case SetDomain.Default.Bottom() => res
        case SetDomain.Default.Inner(xs) =>
          val points = xs.map(_.pp)
          res.copyLocal(inLoops = inLoops -- points, notInLoops = notInLoops ++ points)
      }
    }

    override def lub(other: PreAnalysis[S]) =
      super.lub(other).copyLocal(
        inLoops = (inLoops -- other.notInLoops) ++ (other.inLoops -- notInLoops),
        notInLoops = (notInLoops -- other.inLoops) ++ (other.notInLoops -- inLoops)
      )

    override def widening(other: PreAnalysis[S]) =
      super.widening(other).copyLocal(
        inLoops = (inLoops -- other.notInLoops) ++ (other.inLoops -- notInLoops),
        notInLoops = (notInLoops -- other.inLoops) ++ (other.notInLoops -- inLoops)
      )

    override def lessEqual(other: PreAnalysis[S]) =
      super.lessEqual(other) && inLoops.subsetOf(other.inLoops) && notInLoops.subsetOf(other.notInLoops)

    override def endOfFunctionCleanup() = {
      copyLocal(inLoops = Set.empty, notInLoops = Set.empty)
    }

    override def ids = {
      IdentifierSet.Bottom ++ (forwardMay.keySet ++ forwardMay.values.flatten)
    }

  }

}
