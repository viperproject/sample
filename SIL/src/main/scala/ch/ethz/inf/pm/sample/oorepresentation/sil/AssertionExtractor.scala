package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import ch.ethz.inf.pm.sample.abstractdomain._
import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.vdha.Edge
import ch.ethz.inf.pm.sample.abstractdomain.vdha.HeapGraph
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDefinitionsDomain

trait PredicateBuilder {
  def refType: RefType

  def formalArgName: String = "this"

  def formalArgDecl: sil.LocalVarDecl =
    sil.LocalVarDecl(formalArgName, sil.Ref)()

  def build(predDefs: PredicateDefinitionsDomain): Map[Identifier, sil.Predicate] = {
    val predMap = predDefs.map.keys.map(predDefId => {
      val predName = buildName(predDefId)
      predDefId -> sil.Predicate(predName, Seq(formalArgDecl), null)()
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
              val fieldAccess = sil.FieldAccess(formalArgDecl.localVar, sil.Field(fieldName, sil.Ref)())()

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

case class AssertionExtractor[S <: SemanticDomain[S]](
    condHeapGraph: CondHeapGraph[PredicateDrivenHeapState.EdgeStateDomain[S]])(
    implicit val predicateBuilder: PredicateBuilder,
    val oldSureEdges: Set[Edge[PredicateDrivenHeapState.EdgeStateDomain[S]]] =
      Set.empty[Edge[PredicateDrivenHeapState.EdgeStateDomain[S]]],
    val onlyRecursivePredicates: Boolean = true) {

  import PredicateDrivenHeapState._
  import PredicateDefinition._

  type StateType = PredicateDrivenHeapState.EdgeStateDomain[S]

  def heap: HeapGraph[StateType] = condHeapGraph.heap

  def predicates: Iterable[sil.Predicate] =
    predicateMap.values

  lazy val predicateMap: Map[Identifier, sil.Predicate] =
    predicateBuilder.build(condHeapGraph.cond.defs)

  lazy val assertions: Set[sil.Exp] = {
    val sureAssertions = newSureEdges.flatMap(assertionsForSureEdge)

    val condAssertions = nextAmbigLocalVarVertex match {
      case Some(ambigLocalVarVertex) =>
        val ambigEdges = heap.outEdges(ambigLocalVarVertex)
        val suffConds = sufficientConditions(ambigEdges)
        val accPathId = AccessPathIdentifier(List(ambigLocalVarVertex.name))(refType)
        condHeapGraph.evalAccessPathId(accPathId).apply().prune.condHeaps.flatMap(condSubHeap => {
          val edge = condSubHeap.takenPath(accPathId.path).edges.head
          val suffCond = suffConds(edge)
          val subExtractor = copy(condHeapGraph = condSubHeap)(
            predicateBuilder = predicateBuilder,
            oldSureEdges = newSureEdges,
            onlyRecursivePredicates = onlyRecursivePredicates)
          val rhsAssertions = subExtractor.assertions

          if (rhsAssertions.isEmpty) {
            Set.empty[sil.Exp]
          } else {
            val subExp = rhsAssertions.reduceLeft[sil.Exp](sil.And(_, _)())
            Set[sil.Exp](sil.Implies(suffCond, subExp)())
          }
        }).toSet
      case None => Set.empty
    }

    sureAssertions ++ condAssertions
  }

  private def newSureEdges: Set[Edge[StateType]] =
    sureEdges -- oldSureEdges

  private def sureEdges: Set[Edge[StateType]] =
    heap.localVarVertices.map(heap.outEdges).filter(_.size == 1).map(_.head)

  private def assertionsForSureEdge(edge: Edge[StateType]): Set[sil.Exp] = {
    require(edge.source.isInstanceOf[LocalVariableVertex])

    val localVarVertex = edge.source.asInstanceOf[LocalVariableVertex]
    val foldedPredInstIds = edge.state.insts.foldedPredInstIds
    foldedPredInstIds.map(predInstId => {
      val localVar = sil.LocalVar(localVarVertex.name)(sil.Ref)
      val pred = predicateMap(predInstId.toPredDefId)
      val predAccess = sil.PredicateAccess(Seq(localVar), pred)()
      sil.PredicateAccessPredicate(predAccess, sil.FullPerm()())().asInstanceOf[sil.Exp]
    })
  }

  private def nextAmbigLocalVarVertex: Option[LocalVariableVertex] = {
    heap.localVarVertices.find(heap.outEdges(_).size > 1)
  }

  private def sufficientConditions(ambigEdges: Set[Edge[StateType]]):
    Map[Edge[StateType], sil.Exp] = {

    require(ambigEdges.map(_.source).toSet.size == 1,
      "the edges do not have the same source vertex")
    require(ambigEdges.map(_.target).toSet.size == ambigEdges.size,
      "the edge targets are not unique")
    require(ambigEdges.size == 2,
      "at the moment, only two ambiguous edges are supported")
    require(ambigEdges.exists(_.target == NullVertex),
      "at the moment, one of the targets needs to be null")

    // TODO: Refactor
    var List(e1, e2) = ambigEdges.toList
    if (e2.target == NullVertex) {
      val tmp = e2
      e2 = e1
      e1 = tmp
    }

    require(ambigEdges.forall(_.source.isInstanceOf[LocalVariableVertex]),
      "the edges must have a local variable vertex as their source")

    val localVar = sil.LocalVar(e1.source.name)(sil.Ref)
    val nullnessCond = sil.EqCmp(localVar, sil.NullLit()())()
    val nonNullnessCond = sil.NeCmp(localVar, sil.NullLit()())()

    Map(e1 -> nullnessCond, e2 -> nonNullnessCond)
  }
}