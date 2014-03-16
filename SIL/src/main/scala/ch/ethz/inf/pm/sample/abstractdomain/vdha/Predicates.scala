package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type, DummyProgramPoint}
import ch.ethz.inf.pm.sample.oorepresentation.sil.{PredType, Constants}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.util.Predef._

case class PredicatesDomain(
    map: Map[PredicateIdentifier, PredicateBody] =
      Map.empty[PredicateIdentifier, PredicateBody],
    isTop: Boolean = false,
    override val isBottom: Boolean = false,
    defaultValue: PredicateBody = PredicateBody().top())
  extends FunctionalDomain[PredicateIdentifier, PredicateBody, PredicatesDomain]
  with SemanticDomain[PredicatesDomain]
  with Lattice.Must[PredicatesDomain] {

  // The following class invariant is commented out because it may be violated
  // during the merge operation.
  // TODO: Either rewrite the merge operation or remove the comment
  // require(map.values.forall(_.nestedPredIds.subsetOf(map.keySet)),
  //   "all nested predicate IDs must be known and have a body themselves")

  def get(key: PredicateIdentifier): PredicateBody =
    map.getOrElse(key, defaultValue)

  def functionalFactory(
      value: Map[PredicateIdentifier, PredicateBody],
      isBottom: Boolean,
      isTop: Boolean) =
    PredicatesDomain(value, isTop, isBottom, defaultValue)

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
  def removeNestedTopPredicates(): PredicatesDomain = {
    // Find all top predicates
    val topPredIds = map.collect({
      case (predId, predBody) if predBody.isTop => predId
    }).toSet

    functionalFactory(value = map.mapValues(predBody => {
      predBody.copy(map = predBody.map.mapValues(_.remove(topPredIds)))
    }))
  }

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

  override def merge(r: Replacement): PredicatesDomain = {
    if (r.isEmpty()) return this

    assert(r.value.size == 1, "there must be only one replacement")

    // TODO: Ideally, Replacement would be generic
    var (fromSet, toSet) = r.value.head
      .asInstanceOf[(Set[PredicateIdentifier], Set[PredicateIdentifier])]

    fromSet = fromSet.intersect(map.keySet)
    toSet = toSet.intersect(map.keySet)

    if (fromSet.isEmpty && toSet.isEmpty) {
      this
    } else {
      assert(toSet.size == 1, "can only merge into one predicate definition")

      var result = this

      result = result.copy(map = result.map.mapValues(predBody => {
        predBody.copy(
          map = predBody.map.mapValues(nestedPredIds => {
            var newValue = nestedPredIds.value -- fromSet
            if (newValue.size < nestedPredIds.value.size) {
              newValue = newValue ++ toSet
            }
            nestedPredIds.copy(value = newValue)
          })
        )
      }))

      val newBody = Lattice.bigLub(fromSet.map(result.get))

      result = fromSet.foldLeft(result)(_.removeVariable(_))
      result = result.lub(result.assign(toSet.head, newBody))

      // TODO: Should also replace any other occurrences

      result
    }
  }

  // SemanticDomain has no type parameter for the type of identifiers
  // stored inside of it. Thus, the following methods perform type casts.
  def ids = map.keySet.toSet

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

  def setArgument(variable: Identifier, expr: Expression) = ???
  def backwardAssign(oldPreState: PredicatesDomain, variable: Identifier, expr: Expression) = ???
  def backwardAccess(field: Identifier) = ???
  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = ???
  def access(field: Identifier) = ???
}

/** @todo It should not be necessary for `PredicateIdentifier`
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

  def get(key: Identifier) = map.getOrElse(key, defaultValue)

  def addPerm(field: Identifier): PredicateBody =
    add(field, NestedPredicatesDomain().top())

  def addPerm(field: Identifier, nestedPredId: PredicateIdentifier): PredicateBody =
    add(field, get(field).add(nestedPredId))

  def functionalFactory(
      value: Map[Identifier, NestedPredicatesDomain],
      isBottom: Boolean,
      isTop: Boolean) =
    PredicateBody(value, isTop, isBottom, defaultValue)

  def transform(f: (Expression) => Expression) = this

  def ids = map.values.flatMap(_.value).toSet

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
        nestedPredIds.remove(from).add(to)
      } else {
        nestedPredIds
      }
    }))
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
}

/** Basically an inverse 1-set domain with must semantics. */
final case class NestedPredicatesDomain(
    value: Set[PredicateIdentifier] = Set.empty,
    isTop: Boolean = true,
    isBottom: Boolean = false)
  extends InverseSetDomain[PredicateIdentifier, NestedPredicatesDomain]
  with Lattice.Must[NestedPredicatesDomain] {

  require(value.isEmpty implies (isTop || isBottom),
    "an empty set must only represent top or bottom")

  require(isTop implies value.isEmpty,
    "top must be represented by an empty set")

  require(isBottom implies value.isEmpty,
    "top must be represented by an empty set")

  require(!isBottom implies value.size <= 1,
    "there must be at most one element in the set, unless it is bottom")

  override def setFactory(
      value: Set[PredicateIdentifier],
      isTop: Boolean,
      isBottom: Boolean) = {
    if (value.size > 1) bottom()
    else NestedPredicatesDomain(value, isTop, isBottom)
  }

  /** Removes all given predicate identifiers from the set. */
  def remove(predIds: Set[PredicateIdentifier]): NestedPredicatesDomain =
    predIds.foldLeft(this)(_.remove(_))
}