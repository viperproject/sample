package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{Type, DummyProgramPoint}
import ch.ethz.inf.pm.sample.oorepresentation.sil.{PredType, Constants}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.util.Predef._

case class PredicateDefinitionsDomain(
    map: Map[Identifier, PredicateDefinition] = Map.empty[Identifier, PredicateDefinition],
    isTop: Boolean = false,
    override val isBottom: Boolean = false,
    defaultValue: PredicateDefinition = PredicateDefinition().top())
  extends BoxedDomain[PredicateDefinition, PredicateDefinitionsDomain]
  with SemanticDomain[PredicateDefinitionsDomain]
  with Lattice.Must[PredicateDefinitionsDomain] {

  def get(key: Identifier): PredicateDefinition = map.getOrElse(key, defaultValue)

  def functionalFactory(
      value: Map[Identifier, PredicateDefinition],
      isBottom: Boolean,
      isTop: Boolean) =
    PredicateDefinitionsDomain(value, isTop, isBottom, defaultValue)

  def removeVariable(variable: Identifier) = remove(variable)

  def createVariable(variable: Identifier, typ: Type) =
    add(variable, defaultValue.top())

  def setToTop(variable: Identifier) =
    add(variable, defaultValue.top())

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = ???

  def access(field: Identifier) = ???

  def assume(expr: Expression) = this

  def setArgument(variable: Identifier, expr: Expression) = ???

  def assign(variable: Identifier, expr: Expression) = expr match {
    case (expr: PredicateDefinition) => add(variable, expr)
  }

  def backwardAssign(oldPreState: PredicateDefinitionsDomain, variable: Identifier, expr: Expression) = ???

  def backwardAccess(field: Identifier) = ???

  override def merge(r: Replacement): PredicateDefinitionsDomain = {
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

      result = result.copy(map = result.map.mapValues(predDef => {
        predDef.copy(refFieldPerms = predDef.refFieldPerms.copy(
          map = predDef.refFieldPerms.map.mapValues(nestedPredDefIds => {
            var newValue = nestedPredDefIds.value -- fromSet
            if (newValue.size < nestedPredDefIds.value.size) {
              newValue = newValue ++ toSet
            }
            nestedPredDefIds.copy(value = newValue)
          })
        ))
      }))

      val newDef = Lattice.bigLub(fromSet.map(result.map.apply))

      result = fromSet.foldLeft(result)(_.removeVariable(_))
      result = result.lub(result.assign(toSet.head, newDef))

      // TODO: Should also replace any other occurrences

      result
    }
  }
}

case class PredicateDefinition(
    valFieldPerms: ValFieldPermDomain = ValFieldPermDomain().top(),
    refFieldPerms: RefFieldPermDomain = RefFieldPermDomain().top())
  extends CartesianProductDomain[
    ValFieldPermDomain,
    RefFieldPermDomain,
    PredicateDefinition]
  with Expression {

  def factory(a: ValFieldPermDomain, b: RefFieldPermDomain) =
    PredicateDefinition(a, b)

  def _1 = valFieldPerms

  def _2 = refFieldPerms

  def addValFieldPerm(field: Identifier): PredicateDefinition =
    copy(valFieldPerms = valFieldPerms.add(field))

  def addRefFieldPerm(field: Identifier, symbolicPredicateIdOption: Option[Identifier]) = {
    // TODO: What should happen if there is already an ID?
    var  newSymbolicPredicateIds = refFieldPerms.get(field)
    if (symbolicPredicateIdOption.isDefined) {
      newSymbolicPredicateIds = newSymbolicPredicateIds.add(symbolicPredicateIdOption.get)
    }
    copy(refFieldPerms = refFieldPerms.add(field, newSymbolicPredicateIds))
  }

  def setRefFieldPerm(field: Identifier, nestedPredDefIds: Set[Identifier]) = {
    copy(refFieldPerms = refFieldPerms.add(field, NestedPredDefDomain(nestedPredDefIds, isTop = nestedPredDefIds.isEmpty)))
  }

  def transform(f: (Expression) => Expression) = this

  def ids = refFieldPerms.map.values.flatMap(_.value).toSet

  def pp = DummyProgramPoint

  def typ = PredType

  def isTop: Boolean =
    valFieldPerms.isTop && refFieldPerms.isTop

  def isBottom: Boolean =
    valFieldPerms.isBottom || refFieldPerms.isBottom

  /** Returns true if there are no nested predicate instances */
  def isShallow: Boolean =
    refFieldPerms.map.values.forall(_.value.isEmpty)

  /** Returns whether this definition contains permission to the given field. */
  def hasPerm(field: Identifier): Boolean =
    if (field.typ.isObject) refFieldPerms.map.contains(field)
    else valFieldPerms.value.contains(field)

  override def toString = {
    if (isBottom) "⊥"
    else if (isTop) "⊤"
    else (valFieldPerms.value.toList ++ refFieldPerms.map.map({
      case (field, nestedPredDefIds) =>
        field + " → " + nestedPredDefIds
    })).mkString(", ")
  }
}

object PredicateDefinition {
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

final case class ValFieldPermDomain(
    value: Set[Identifier] = Set.empty[Identifier],
    isTop: Boolean = true,
    isBottom: Boolean = false)
  extends InverseSetDomain[Identifier, ValFieldPermDomain]
  with Lattice.Must[ValFieldPermDomain] {

  def setFactory(value: Set[Identifier], isTop: Boolean, isBottom: Boolean) =
    ValFieldPermDomain(value, isTop, isBottom)
}

final case class RefFieldPermDomain(
    map: Map[Identifier, NestedPredDefDomain] = Map.empty[Identifier, NestedPredDefDomain],
    isTop: Boolean = false,
    override val isBottom: Boolean = false,
    defaultValue: NestedPredDefDomain = NestedPredDefDomain().top())
  extends FunctionalDomain[Identifier, NestedPredDefDomain, RefFieldPermDomain]
  with Lattice.Must[RefFieldPermDomain] {

  def get(key: Identifier) = map.getOrElse(key, defaultValue)

  def functionalFactory(
      value: Map[Identifier, NestedPredDefDomain],
      isBottom: Boolean,
      isTop: Boolean) =
    RefFieldPermDomain(value, isTop, isBottom, defaultValue)
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