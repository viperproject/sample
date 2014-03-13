package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{Type, DummyProgramPoint}
import ch.ethz.inf.pm.sample.oorepresentation.sil.{PredType, Constants}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.util.Predef._

case class PredicatesDomain(
    map: Map[Identifier, PredicateBody] = Map.empty[Identifier, PredicateBody],
    isTop: Boolean = false,
    override val isBottom: Boolean = false,
    defaultValue: PredicateBody = PredicateBody().top())
  extends BoxedDomain[PredicateBody, PredicatesDomain]
  with SemanticDomain[PredicatesDomain]
  with Lattice.Must[PredicatesDomain] {

  // The following class invariant is commented out because it may be violated
  // during the merge operation.
  // TODO: Either rewrite the merge operation or remove the comment
  // require(map.values.forall(_.nestedPredIds.subsetOf(map.keySet)),
  //   "all nested predicate IDs must be known and have a body themselves")

  def get(key: Identifier): PredicateBody = map.getOrElse(key, defaultValue)

  def functionalFactory(
      value: Map[Identifier, PredicateBody],
      isBottom: Boolean,
      isTop: Boolean) =
    PredicatesDomain(value, isTop, isBottom, defaultValue)

  def removeVariable(variable: Identifier) = remove(variable)

  def createVariable(variable: Identifier, typ: Type) =
    add(variable, defaultValue.top())

  def setToTop(variable: Identifier) =
    add(variable, defaultValue.top())

  def assume(expr: Expression) = this

  def assign(variable: Identifier, expr: Expression) = expr match {
    case (expr: PredicateBody) => add(variable, expr)
  }

  /** Finds a predicate that is structurally equal to the given predicate,
    * which consists of an identifier as well as its body.
    *
    * @todo support nested predicate instances that are not directly recursive
    */
  def findEqual(needleId: Identifier, needleBody: PredicateBody): Option[Identifier] = {
    for ((id, body) <- map) {
      val renamedBody = body.rename(id, needleId)
      if (renamedBody == needleBody) {
        return Some(id)
      }
    }
    None
  }

  override def merge(r: Replacement): PredicatesDomain = {
    if (r.isEmpty()) return this

    assert(r.value.size == 1, "there must be only one replacement")
    var (fromSet, toSet) = r.value.head

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

      val newBody = Lattice.bigLub(fromSet.map(result.map.apply))

      result = fromSet.foldLeft(result)(_.removeVariable(_))
      result = result.lub(result.assign(toSet.head, newBody))

      // TODO: Should also replace any other occurrences

      result
    }
  }

  def setArgument(variable: Identifier, expr: Expression) = ???
  def backwardAssign(oldPreState: PredicatesDomain, variable: Identifier, expr: Expression) = ???
  def backwardAccess(field: Identifier) = ???
  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = ???
  def access(field: Identifier) = ???
}

object PredicatesDomain {
  private val nextId = new ThreadLocal[Int]

  def resetId() = {
    nextId.set(0)
  }

  def makeId(): VariableIdentifier = {
    val id = nextId.get
    nextId.set(id + 1)
    id.toString
    VariableIdentifier(Constants.GhostSymbolPrefix + "p" + id)(PredType)
  }
}

final case class PredicateBody(
    map: Map[Identifier, NestedPredDefDomain] = Map.empty[Identifier, NestedPredDefDomain],
    isTop: Boolean = false,
    override val isBottom: Boolean = false,
    defaultValue: NestedPredDefDomain = NestedPredDefDomain().top())
  extends FunctionalDomain[Identifier, NestedPredDefDomain, PredicateBody]
  with Lattice.Must[PredicateBody]
  with Expression {

  def get(key: Identifier) = map.getOrElse(key, defaultValue)

  def addPerm(field: Identifier): PredicateBody =
    add(field, NestedPredDefDomain().top())

  def functionalFactory(
      value: Map[Identifier, NestedPredDefDomain],
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
  def rename(from: Identifier, to: Identifier): PredicateBody = {
    copy(map = map.mapValues(nestedPredIds => {
      if (nestedPredIds.value.contains(from)) {
        nestedPredIds.remove(from).add(to)
      } else {
        nestedPredIds
      }
    }))
  }

  /** Returns a set of all directly nested predicate IDs. */
  def nestedPredIds: Set[Identifier] =
    map.values.flatMap(_.value).toSet

  override def toString = {
    if (isBottom) "⊥"
    else if (isTop) "⊤"
    else map.map({
      case (field, nestedPredIds) =>  field + " → " + nestedPredIds
    }).mkString(", ")
  }
}

/** Basically an inverse 1-set domain with must semantics. */
final case class NestedPredDefDomain(
    value: Set[Identifier] = Set.empty,
    isTop: Boolean = true,
    isBottom: Boolean = false)
  extends InverseSetDomain[Identifier, NestedPredDefDomain]
  with Lattice.Must[NestedPredDefDomain] {

  require(value.isEmpty implies (isTop || isBottom),
    "an empty set must only represent top or bottom")

  require(isTop implies value.isEmpty,
    "top must be represented by an empty set")

  require(isBottom implies value.isEmpty,
    "top must be represented by an empty set")

  require(!isBottom implies value.size <= 1,
    "there must be at most one element in the set, unless it is bottom")

  override def setFactory(value: Set[Identifier], isTop: Boolean, isBottom: Boolean) = {
    if (value.size > 1) bottom()
    else NestedPredDefDomain(value, isTop, isBottom)
  }
}