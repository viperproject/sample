package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{Type, DummyProgramPoint}
import ch.ethz.inf.pm.sample.oorepresentation.sil.{AbstractType, Constants, DefaultSampleConverter}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.ToStringUtilities

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

  def toSilPredicates(receiverName: String = "this"): Seq[sil.Predicate] = {
    val predMap = map.keys.map(predDefId => {
      val formalArg = sil.LocalVarDecl(receiverName, sil.Ref)()
      predDefId.asInstanceOf[VariableIdentifier] -> sil.Predicate(predDefId.getName, Seq(formalArg), null)()
    }).toMap

    predMap.foreach({
      case (predDefId, pred) =>
        pred.body = map.get(predDefId).get.toSilPredicateBody(receiverName, predMap)
    })

    predMap.values.toSeq
  }

  /** @todo Does not detect mutually recursive predicate definitions. */
  def nonRecursiveIds: Set[Identifier] = {
    map.filterNot({
      case (id, predDef) =>
        val nestedIds = predDef.refFieldPerms.map.values.map(_.value).flatten.toSet
        nestedIds.contains(id)
    }).keySet
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

  import PredicateDrivenHeapState._

  if (refFieldPerms.map.values.exists(_.value.size > 1)) {
    println("there is a predicate definition with ambiguous nested predicate")
  }

  def factory(a: ValFieldPermDomain, b: RefFieldPermDomain) =
    PredicateDefinition(a, b)

  def _1 = valFieldPerms

  def _2 = refFieldPerms

  def addValFieldPerm(field: String): PredicateDefinition =
    copy(valFieldPerms = valFieldPerms.add(field))

  def addRefFieldPerm(field: String, symbolicPredicateIdOption: Option[Identifier]) = {
    // TODO: What should happen if there is already an ID?
    var  newSymbolicPredicateIds = refFieldPerms.get(field)
    if (symbolicPredicateIdOption.isDefined) {
      newSymbolicPredicateIds = newSymbolicPredicateIds.add(symbolicPredicateIdOption.get)
    }
    copy(refFieldPerms = refFieldPerms.add(field, newSymbolicPredicateIds))
  }

  def setRefFieldPerm(field: String, nestedPredDefIds: Set[Identifier]) = {
    copy(refFieldPerms = refFieldPerms.add(field, InverseSetDomain.Must(nestedPredDefIds, isTop = nestedPredDefIds.isEmpty)))
  }

  def transform(f: (Expression) => Expression) = this

  def ids = refFieldPerms.map.values.flatMap(_.value).toSet

  def pp = DummyProgramPoint

  def typ = PredicateDefinitionType

  def isTop: Boolean =
    valFieldPerms.isTop && refFieldPerms.isTop

  def isBottom: Boolean =
    valFieldPerms.isBottom || refFieldPerms.isBottom

  override def toString = {
    if (isBottom) "⊥"
    else if (isTop) "⊤"
    else (valFieldPerms.value.toList ++ refFieldPerms.map.map({
      case (field, nestedPredDefIds) =>
        field + " → " + nestedPredDefIds
    })).mkString(", ")
  }

  def toSilPredicateBody(receiverName: String = "this", predMap: Map[VariableIdentifier, sil.Predicate]): sil.Exp = {
    if (isTop)
      sil.TrueLit()()
    else if (isBottom)
      sil.FalseLit()()
    else {
      def toFieldAccessPred(fieldName: String): sil.FieldAccessPredicate = {
        val fieldId = refType.fields.find(_.getName == fieldName).get
        val accPathId = AccessPathIdentifier(List(receiverName), fieldId)
        val fieldAccess = DefaultSampleConverter.convert(accPathId).asInstanceOf[sil.FieldAccess]
        sil.FieldAccessPredicate(fieldAccess, sil.FullPerm()())()
      }

      val valAccessPreds = valFieldPerms.value.map(toFieldAccessPred)

      val refAccessPreds = refFieldPerms.map.map({
        case (fieldName, predDefIds) =>
          val predDefId = predDefIds.value.head.asInstanceOf[VariableIdentifier]
          val refFieldAccessPred = toFieldAccessPred(fieldName)

          val nonNullnessCond = sil.NeCmp(refFieldAccessPred.loc, sil.NullLit()())()

          val fieldId = refType.fields.find(_.getName == fieldName).get
          val accPathId = AccessPathIdentifier(List(receiverName), fieldId)
          val fieldAccess = DefaultSampleConverter.convert(accPathId).asInstanceOf[sil.FieldAccess]
          val pred = predMap(predDefId)
          val predAccessPred = sil.PredicateAccessPredicate(sil.PredicateAccess(Seq(fieldAccess), pred)(), sil.FullPerm()())()
          val condPredAccessPred = sil.Implies(nonNullnessCond, predAccessPred)()

          sil.And(refFieldAccessPred, condPredAccessPred)()
      })

      val preds = valAccessPreds.toList ++ refAccessPreds.toList

      preds.reduceLeft[sil.Exp](sil.And(_, _)())
    }
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
    VariableIdentifier(Constants.GhostSymbolPrefix + "p" + id)(PredicateDefinitionType)
  }

  implicit class ExtendedVariableIdentifier(variableId: VariableIdentifier) {
    def toPredDefId: VariableIdentifier = {
      require(variableId.typ == PredicateInstanceType)
      variableId.copy()(typ = PredicateDefinitionType, pp = DummyProgramPoint)
    }

    def toPredInstId: VariableIdentifier = {
      require(variableId.typ == PredicateDefinitionType)
      variableId.copy()(typ = PredicateInstanceType, pp = DummyProgramPoint)
    }
  }

  def extractPredInstId(id: Identifier): VariableIdentifier = id match {
    case EdgeLocalIdentifier(accPath, field) => field.asInstanceOf[VariableIdentifier]
    case id @ AccessPathIdentifier(path) =>
      VariableIdentifier(path.last)(id.typ)
  }
}

final case class ValFieldPermDomain(
    value: Set[String] = Set.empty[String],
    isTop: Boolean = false,
    isBottom: Boolean = false)
  extends InverseSetDomain[String, ValFieldPermDomain] with Lattice.Must[ValFieldPermDomain] {

  def setFactory(value: Set[String], isTop: Boolean, isBottom: Boolean) =
    ValFieldPermDomain(value, isTop, isBottom)
}

final case class RefFieldPermDomain(
    map: Map[String, InverseSetDomain.Must[Identifier]] = Map.empty[String, InverseSetDomain.Must[Identifier]],
    isTop: Boolean = false,
    override val isBottom: Boolean = false,
    defaultValue: InverseSetDomain.Must[Identifier] = InverseSetDomain.Must[Identifier]().top())
  extends FunctionalDomain[String, InverseSetDomain.Must[Identifier], RefFieldPermDomain]
  with Lattice.Must[RefFieldPermDomain] {

  def get(key: String) = map.getOrElse(key, defaultValue)

  def functionalFactory(value: Map[String, InverseSetDomain.Must[Identifier]], isBottom: Boolean, isTop: Boolean) =
    RefFieldPermDomain(value, isTop, isBottom, defaultValue)
}

case object PredicateDefinitionType extends AbstractType("Pred") {
  def isNumericalType = true
}