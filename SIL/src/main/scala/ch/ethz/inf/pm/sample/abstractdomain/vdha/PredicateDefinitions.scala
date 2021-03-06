/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.SetDomain.Default
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type, DummyProgramPoint}
import ch.ethz.inf.pm.sample.oorepresentation.sil.{PredType, Constants}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import com.typesafe.scalalogging.LazyLogging

/** Domain that represents (candidate) predicate definitions.
  * Concretely, it maps predicate identifiers to predicate bodies.
  */
case class PredicateDefinitionsDomain(
    map: Map[PredicateIdentifier, PredicateBody] =
      Map.empty[PredicateIdentifier, PredicateBody],
    isTop: Boolean = false,
    override val isBottom: Boolean = false,
    defaultValue: PredicateBody = PredicateBody().top())
  extends FunctionalDomain[PredicateIdentifier, PredicateBody, PredicateDefinitionsDomain]
  with SemanticDomain[PredicateDefinitionsDomain]
  with Lattice.Must[PredicateDefinitionsDomain]
  with LazyLogging {

  require(map.values.forall(_.nestedPredIds.subsetOf(map.keySet)),
    "all nested predicate IDs must be known and have a body themselves")

  /** Finds a predicate that is structurally equal to the given predicate,
    * which consists of an identifier as well as its body.
    *
    * @todo support nested predicate instances that are not directly recursive
    */
  def findEqual(needleId: PredicateIdentifier, needleBody: PredicateBody): Option[Identifier] = {
    for ((id, body) <- map) {
      val renamedBody = body.rename(id, needleId)
      if (renamedBody == needleBody) {
        return Some(id)
      }
    }
    None
  }

  /** Removes all nested predicates whose body is (top) true. */
  def removeNestedTopPredicates(): PredicateDefinitionsDomain = {
    // Find all top predicates
    val topPredIds = map.collect({
      case (predId, predBody) if predBody.isTop => predId
    }).toSet

    functionalFactory(value = map.mapValues(predBody => {
      predBody.copy(map = predBody.map.mapValues(_.remove(topPredIds)))
    }))
  }

  def functionalFactory(
      value: Map[PredicateIdentifier, PredicateBody],
      isBottom: Boolean,
      isTop: Boolean) =
    PredicateDefinitionsDomain(value, isTop, isBottom, defaultValue)

  /** Returns the set of set of fields that the predicate with the given ID
    * directly (not mutually) recurses over.
    */
  def recursionFields(predId: PredicateIdentifier): Set[Identifier] = {
    get(predId).map.flatMap({
      case (field, nestedPredIds) =>
        if (nestedPredIds.value.contains(predId)) Some(field)
        else None
    }).toSet
  }

  /** Returns the predicate body associated with a predicate identifier. */
  def get(key: PredicateIdentifier): PredicateBody =
    map.getOrElse(key, defaultValue)

  def merge(predIdMerge: PredicateIdentifierMerge): PredicateDefinitionsDomain = {
    // Nothing to do when there is only one predicate ID in the set
    // of IDs to be merged
    if (predIdMerge.predIds.size == 1) return this

    // Nothing to do when none of the predicate identifiers to be merged occur.
    // TODO: Investigate why this situation can occur for firstnaturals.sil.
    if (ids.getNonTopUnsafe.diff(predIdMerge.predIds.toSet[Identifier]).isEmpty) return this

    var newMap = map.mapValues(_.merge(predIdMerge))
    val newTargetPredBody = Lattice.bigLub(newMap.filterKeys(predIdMerge.predIds.contains).values)

    newMap = newMap.filterKeys(!predIdMerge.predIds.contains(_)) + (predIdMerge.target -> newTargetPredBody)
    val result = copy(map = newMap)

    result
  }

  // SemanticDomain has no type parameter for the type of identifiers
  // stored inside of it. Thus, the following methods perform type casts.
  def ids = IdentifierSet.Inner(map.keySet.toSet[Identifier])

  def removeVariable(id: Identifier) = id match {
    case id: PredicateIdentifier => remove(id)
  }

  def createVariable(id: Identifier, typ: Type) = id match {
    case id: PredicateIdentifier => add(id, defaultValue.top())
  }

  def createVariableForArgument(id: Identifier, typ: Type) = id match {
    case id: PredicateIdentifier => add(id, defaultValue.top())
  }

  def setToTop(id: Identifier) = id match {
    case id: PredicateIdentifier => add(id, defaultValue.top())
  }

  def assume(expr: Expression) = this

  def assign(id: Identifier, expr: Expression) = (id, expr) match {
    case (id: PredicateIdentifier, expr: PredicateBody) => add(id, expr)
  }

  def getStringOfId(id: Identifier) = id match {
    case id: PredicateIdentifier => get(id).toString
  }

  /** If there is a predicate with a field with more than one nested
    * predicate ID, returns the IDs so they can be merged, None otherwise.
    *
    * If there are multiple such fields, only return the set for
    * one of the fields. The idea is to do one merge at a time
    * and then check if there is still something to be merged.
    */
  def requiredIdMergeOption: Option[PredicateIdentifierMerge] =
    map.values.map(_.requiredIdMergeOption).flatten.headOption

  def setArgument(variable: Identifier, expr: Expression) = ???
  def backwardAssign(oldPreState: PredicateDefinitionsDomain, variable: Identifier, expr: Expression) = ???
  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = ???

  def merge(r: Replacement): PredicateDefinitionsDomain = {
    if (!r.isEmpty())
      logger.warn(s"Replacement $r ignored. " +
        "Use custom merge(PredicateIdentifierMerge) instead.")
    this
  }
}

/** Identifies a predicate definition in the analysis.
  *
  * @todo It should not be necessary for `PredicateIdentifier`
  * to extend `VariableIdentifier`.
  */
class PredicateIdentifier(override val name: String)
  extends VariableIdentifier(name)(PredType) {
}

object PredicateIdentifier {
  private val nextId = new ThreadLocal[Int]

  def reset() = {
    nextId.set(0)
  }

  /** Returns a fresh `PredicateIdentifier`. */
  def make(): PredicateIdentifier = {
    val id = nextId.get
    nextId.set(id + 1)
    val name = Constants.GhostSymbolPrefix + "p" + id
    new PredicateIdentifier(name)
  }
}

final case class PredicateBody(
    map: Map[Identifier, NestedPredicatesDomain] = Map.empty[Identifier, NestedPredicatesDomain],
    isTop: Boolean = false,
    override val isBottom: Boolean = false,
    defaultValue: NestedPredicatesDomain = NestedPredicatesDomain().top())
  extends FunctionalDomain[Identifier, NestedPredicatesDomain, PredicateBody]
  with Lattice.Must[PredicateBody]
  with Expression {

  def addPerm(field: Identifier): PredicateBody =
    add(field, NestedPredicatesDomain().top())

  def addPerm(field: Identifier, nestedPredId: PredicateIdentifier): PredicateBody =
    add(field, get(field).+(nestedPredId))

  def get(key: Identifier) = map.getOrElse(key, defaultValue)

  def functionalFactory(
      value: Map[Identifier, NestedPredicatesDomain],
      isBottom: Boolean,
      isTop: Boolean) =
    PredicateBody(value, isTop, isBottom, defaultValue)

  def transform(f: (Expression) => Expression) = this

  def ids = IdentifierSet.Inner(map.values.flatMap(_.value).toSet)

  def pp = DummyProgramPoint

  def typ = PredType

  /** Returns true if there are no nested predicate instances */
  def isShallow: Boolean =
    map.values.forall(_.value.isEmpty)

  /** Returns whether this definition contains permission to the given field. */
  def hasPerm(field: Identifier): Boolean =
    map.contains(field)

  /** Replaces all occurrences of a given predicate ID
    * with a given other predicate ID.
    */
  def rename(from: PredicateIdentifier, to: PredicateIdentifier): PredicateBody = {
    copy(map = map.mapValues(nestedPredIds => {
      if (nestedPredIds.value.contains(from)) {
        nestedPredIds.-(from).+(to)
      } else {
        nestedPredIds
      }
    }))
  }

  def merge(predIdMerge: PredicateIdentifierMerge): PredicateBody = {
    copy(map = map.mapValues(_.merge(predIdMerge)))
  }

  /** Returns a set of all directly nested predicate IDs. */
  def nestedPredIds: Set[PredicateIdentifier] =
    map.values.flatMap(_.value).toSet

  /** Returns a map of fields to their directly nested predicate IDs. */
  def nestedPredIdMap: Map[Identifier, PredicateIdentifier] =
    map.flatMap({
      case (field, nestedPredIds) =>
        require(!nestedPredIds.isBottom,
          "nested predicate IDs must not be bottom")
        if (nestedPredIds.value.isEmpty) None
        else Some(field -> nestedPredIds.value.head)
    })

  override def toString = {
    if (isBottom) "⊥"
    else if (isTop) "⊤"
    else map.map({
      case (field, nestedPredIds) =>  field + " → " + nestedPredIds
    }).mkString(", ")
  }

  /** If there is a field with more than one nested predicate ID,
    * returns them so they can be merged, None otherwise.
    *
    * If there are multiple such fields, only return the set for
    * one of the fields. The idea is to do one merge at a time
    * and then check if there is still something to be merged.
    */
  def requiredIdMergeOption: Option[PredicateIdentifierMerge] =
    map.values.map(_.requiredIdMergeOption).flatten.headOption

  def contains(f: (Expression => Boolean)): Boolean = f(this)
}

/** Basically an inverse 1-set domain with must semantics. */
final case class NestedPredicatesDomain(wrapped: SetDomain.Default[PredicateIdentifier] = SetDomain.Default.Top())
  extends InvertedSetDomain[PredicateIdentifier, NestedPredicatesDomain] {

  /**
   * Code accessing this should distinguish between bottom and top and inner!
   */
  @Deprecated
  def value:Set[PredicateIdentifier] = wrapped match {
    case SetDomain.Default.Inner(x) => x
    case _ => Set.empty
  }

  def merge(predIdMerge: PredicateIdentifierMerge): NestedPredicatesDomain = wrapped match {
    case SetDomain.Default.Inner(value) =>
      if (value.intersect(predIdMerge.predIds).isEmpty) this
      else wrapperFactory(wrapped.factory(value = value -- predIdMerge.predIds + predIdMerge.target))
    case _ => this
  }

  override def wrapperFactory(wrapped: Default[PredicateIdentifier]): NestedPredicatesDomain = NestedPredicatesDomain(wrapped)

  /** Removes all given predicate identifiers from the set. */
  def remove(predIds: Set[PredicateIdentifier]): NestedPredicatesDomain =
    predIds.foldLeft(this)(_.-(_))

  /** If there is more than one nested predicate ID,
    * returns them so they can be merged, None otherwise.
    */
  def requiredIdMergeOption: Option[PredicateIdentifierMerge] = wrapped match {
    case SetDomain.Default.Inner(value) =>
      if (value.size > 1) Some(PredicateIdentifierMerge(value))
      else None
    case _ => None
  }

}

/** Represents a merge of a set of predicates.
  *
  * All predicates in the set will be merged into the oldest one in the set,
  * according to the total order defined on predicate identifiers.
  *
  * This class is used instead of `Replacement`. The latter is very cumbersome
  * to work with, is not generic, etc.
  *
  * @param predIds the predicate identifiers to merge
  */
case class PredicateIdentifierMerge(predIds: Set[PredicateIdentifier]) {
  require(predIds.nonEmpty,
    "the set of predicate identifiers to merge must not be empty")

  /** Translates the merge to a `Replacement` that can be passed
    * to a `SemanticDomain`.
    */
  def toReplacement: Replacement = {
    val repl = new Replacement()
    repl.value += (predIds.toSet[Identifier] -> Set[Identifier](target))
    repl
  }

  /** Predicate ID that the predicate IDs will be merged into. */
  def target: PredicateIdentifier = {
    predIds.minBy(_.name) // TODO: Should sort the number of the predicate ID
  }
}