/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.{VariableIdentifier, _}
import ch.ethz.inf.pm.sample.oorepresentation.sil.PredType
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, Type}
import com.typesafe.scalalogging.LazyLogging

case class PredicateInstancesDomain(
    map: Map[Identifier, PredicateInstanceDomain] = Map.empty,
    isTop: Boolean = true,
    override val isBottom: Boolean = false,
    defaultValue: PredicateInstanceDomain = PredicateInstanceDomain())
  extends BoxedDomain[PredicateInstanceDomain, PredicateInstancesDomain]
  with SemanticDomain[PredicateInstancesDomain]
  with LazyLogging {

  import PredicateInstanceState.{Folded, Unfolded}

  require(map.keySet.forall(canHandle), "cannot handle all ids")

  def canHandle(id: Identifier): Boolean = id match {
    case EdgeLocalIdentifier(_, predInstId: PredicateInstanceIdentifier) => true
    case ValueHeapIdentifier(_, predInstId: PredicateInstanceIdentifier) => true
    case AccessPathIdentifier(path) if path.last.isInstanceOf[PredicateInstanceIdentifier] => true
    case _ => false
  }

  override def glb(other: PredicateInstancesDomain) = lub(other)

  def functionalFactory(
      map: Map[Identifier, PredicateInstanceDomain],
      isBottom: Boolean,
      isTop: Boolean) = {
    var newIsTop = isTop
    var newIsBottom = isBottom
    if (map.exists(_._2 == defaultValue.bottom())) {
      newIsBottom = true
    }
    // Problem: In `CustomGlbPreservingIdsStrategy`, we set the default value
    // of the partners temporarily to bottom to prevent losing permission
    // However, isTop may still be true, in which case glb(...) would take
    // a shortcut that would cause us to lose the permission.
    // TODO: This is extremely fragile and complicated
    if (defaultValue.isBottom) {
      newIsTop = false
    }
    PredicateInstancesDomain(map, isBottom = newIsBottom, isTop = newIsTop, defaultValue = defaultValue)
  }

  /** The predicate instance IDs of all folded and unfolded predicate instances. */
  def foldedAndUnfoldedInstIds: Set[PredicateInstanceIdentifier] = foldedInstIds ++ unfoldedInstIds

  def ids(instState: PredicateInstanceState): Set[PredicateIdentifier] =
    instIds(instState).map(_.predId)

  def foldedIds: Set[PredicateIdentifier] = foldedInstIds.map(_.predId)

  /** The predicate instance IDs of all folded predicate instances. */
  def foldedInstIds: Set[PredicateInstanceIdentifier] = instIds(Folded)

  /** Returns the IDs of all predicates instance for which we have an instance
    * in the given state.
    */
  def instIds(instState: PredicateInstanceState): Set[PredicateInstanceIdentifier] =
    map.keySet.collect({
      // Only consider target edge-local identifiers
      case id @ EdgeLocalIdentifier(field :: Nil, predId)
        if get(id).value.contains(instState) => predId.asInstanceOf[PredicateInstanceIdentifier]
    })

  def unfoldedIds: Set[PredicateIdentifier] = unfoldedInstIds.map(_.predId)

  /** The predicate instance IDs of all unfolded predicate instances. */
  def unfoldedInstIds: Set[PredicateInstanceIdentifier] = instIds(Unfolded)

  def foldedAndUnfoldedIds: Set[PredicateIdentifier] = foldedAndUnfoldedInstIds.map(_.predId)

  def createVariable(variable: Identifier, typ: Type) = {
    add(variable, defaultValue.top())
  }

  def setToTop(variable: Identifier) =
    add(variable, defaultValue.top())

  def assign(variable: Identifier, expr: Expression) = expr match {
    case (expr: PredicateInstanceState) =>
      add(variable, defaultValue.+(expr))
  }

  override def merge(r: Replacement): PredicateInstancesDomain = {
    if (r.isEmpty()) return this
    var result = this
    val removedVariables = r.keySet().flatten

    // We remove the variables from the result state
    for (v <- removedVariables)
      result = result.remove(v)

    for (s <- r.keySet()) {
      val toSet = r.apply(s)
      assert(toSet.size == 1)

      val to = toSet.head

      Lattice.bigLub(s.map(this.get))
    }

    result
  }

  def removeVariable(variable: Identifier) = remove(variable)

  def assume(expr: Expression) = expr match {
    case BinaryArithmeticExpression(id: Identifier,
    state: PredicateInstanceState, ArithmeticOperator.==) =>
      add(id, get(id).+(state))
    case _ =>
      logger.debug(s"Assuming expression $expr in predicate instances domain " +
        "is not supported")
      this
  }

  def get(key: Identifier) = map.getOrElse(key, defaultValue)

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = ???
  def setArgument(variable: Identifier, expr: Expression) = ???
  def backwardAssign(oldPreState: PredicateInstancesDomain, variable: Identifier, expr: Expression) = ???
}

/** Inverse set domain with empty set as top element and
  * {`Folded`, `Unfolded`} as bottom element.
  */
case class PredicateInstanceDomain(wrapped: SetDomain.Default[PredicateInstanceState] = SetDomain.Default.Top())
  extends InvertedSetDomain[PredicateInstanceState, PredicateInstanceDomain] {

  @Deprecated
  def value:Set[PredicateInstanceState] = wrapped match {
    case SetDomain.Default.Inner(v) => v
    case _ => Set.empty
  }

  override def wrapperFactory(wrapped: SetDomain.Default[PredicateInstanceState]): PredicateInstanceDomain =
    PredicateInstanceDomain(wrapped)
}

/** Ghost field identifier that combines a predicate definition identifier
  * with a predicate instance version number.
  *
  * In edge states (concretely in `PredicateInstancesDomain`), such identifiers
  * will be wrapped in `EdgeLocalIdentifier`s.
  */
class PredicateInstanceIdentifier(
    val predId: PredicateIdentifier,
    val version: Int)
  extends VariableIdentifier(name = s"$predId#$version")(PredType) {

}

object PredicateInstanceIdentifier {
  private val nextVersion = new ThreadLocal[Int]

  def resetVersion() = {
    nextVersion.set(0)
  }

  /** Make a predicate instance identifier with a fresh version number. */
  def make(predId: PredicateIdentifier): PredicateInstanceIdentifier =
    new PredicateInstanceIdentifier(predId, makeVersion())

  def makeVersion(): Int = {
    val version = nextVersion.get
    nextVersion.set(version + 1)
    version
  }
}

final case class PredicateInstanceState(name: String) extends Expression {
  def transform(f: (Expression) => Expression) = this
  def ids = IdentifierSet.Bottom
  def pp = DummyProgramPoint
  def typ = PredType

  override def toString = name

  def contains(f: (Expression => Boolean)): Boolean = f(this)
}

object PredicateInstanceState {
  val Folded = PredicateInstanceState("folded")
  val Unfolded = PredicateInstanceState("unfolded")
}