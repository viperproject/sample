package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import ch.ethz.inf.pm.sample.abstractdomain._
import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDefinitionsDomain

trait PredicateBuilder {
  def refType: RefType

  def formalArgName: String = "this"

  def build(predDefs: PredicateDefinitionsDomain): Map[Identifier, sil.Predicate] = {
    val predMap = predDefs.map.keys.map(predDefId => {
      val formalArg = sil.LocalVarDecl(formalArgName, sil.Ref)()
      val predName = buildName(predDefId)
      predDefId -> sil.Predicate(predName, Seq(formalArg), null)()
    }).toMap

    predMap.foreach({
      case (predDefId, silPred) =>
        val predDef = predDefs.map(predDefId)
        silPred.body = buildBody(predDef, predMap)
    })

    predMap
  }

  protected def buildName(predDefId: Identifier): String = {
    val name = predDefId.getName
    val nameWithoutPrefix = name.replace(Constants.GhostSymbolPrefix, "")
    nameWithoutPrefix
  }

  protected def buildBody(
      predDef: PredicateDefinition,
      predMap: Map[Identifier, sil.Predicate]): sil.Exp = {
    if (predDef.isTop)
      sil.TrueLit()()
    else if (predDef.isBottom)
      sil.FalseLit()()
    else {
      val valAccessPreds = predDef.valFieldPerms.value.map(buildFieldAccessPred)

      val refAccessPreds = predDef.refFieldPerms.map.map({
        case (fieldName, nestedPredDefIds) =>
          val refFieldAccessPred = buildFieldAccessPred(fieldName)

          assert(!nestedPredDefIds.isBottom,
            "set of nested predicate definitions must not be bottom")

          nestedPredDefIds.value.toSeq  match {
            case nestedPredDefId :: Nil =>
              val nonNullnessCond = sil.NeCmp(refFieldAccessPred.loc, sil.NullLit()())()
              val fieldId = refType.fields.find(_.getName == fieldName).get
              val accPathId = AccessPathIdentifier(List(formalArgName), fieldId)
              val fieldAccess = DefaultSampleConverter.convert(accPathId).asInstanceOf[sil.FieldAccess]

              val pred = predMap(nestedPredDefId)
              val predAccessPred = sil.PredicateAccessPredicate(
                sil.PredicateAccess(Seq(fieldAccess), pred)(), sil.FullPerm()())()
              val condPredAccessPred = sil.Implies(nonNullnessCond, predAccessPred)()

              sil.And(refFieldAccessPred, condPredAccessPred)()
            case Nil =>
              refFieldAccessPred
          }
      })

      val preds = valAccessPreds.toList ++ refAccessPreds.toList
      preds.reduceLeft[sil.Exp](sil.And(_, _)())
    }
  }

  def buildFieldAccessPred(fieldName: String): sil.FieldAccessPredicate = {
    val fieldId = refType.fields.find(_.getName == fieldName).get
    val accPathId = AccessPathIdentifier(List(formalArgName), fieldId)
    val fieldAccess = DefaultSampleConverter.convert(accPathId).asInstanceOf[sil.FieldAccess]
    sil.FieldAccessPredicate(fieldAccess, sil.FullPerm()())()
  }
}

case class DefaultPredicateBuilder(refType: RefType) extends PredicateBuilder {}

case class AssertionExtractor[S <: SemanticDomain[S]]
    (condHeapGraph: CondHeapGraph[PredicateDrivenHeapState.EdgeStateDomain[S]])
    (implicit val predicateBuilder: PredicateBuilder) {

  import PredicateDrivenHeapState._

  type StateType = PredicateDrivenHeapState.EdgeStateDomain[S]

  def extractPredicates(): Iterable[sil.Predicate] = {
    predicateBuilder.build(condHeapGraph.cond.defs).values
  }

  def extract(): Set[sil.Exp] = {
    Set.empty
  }
}