package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import ch.ethz.inf.pm.sample.abstractdomain._
import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.vdha.Edge
import ch.ethz.inf.pm.sample.abstractdomain.vdha.HeapGraph
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDefinitionsDomain
import semper.sil.ast.utility.Transformer
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{ApronInterface, ApronInterfaceTranslator}

trait PredicateBuilder {
  import DefaultSampleConverter._

  /** Name of the predicate definition parameter. */
  def formalArgName: String

  /** Declaration of the predicate definition parameter. */
  def formalArgDecl: sil.LocalVarDecl =
    sil.LocalVarDecl(formalArgName, sil.Ref)()

  def build(predDefs: PredicateDefinitionsDomain): Map[Identifier, sil.Predicate] = {
    // Build a predicate with an empty body for every ID
    val predMap = predDefs.map.keys.map(predId => {
      val predName = buildName(predId)
      predId -> sil.Predicate(predName, Seq(formalArgDecl), null)()
    }).toMap

    predMap.foreach({
      case (predDefId, silPred) =>
        val predDef = predDefs.map(predDefId)
        silPred.body = buildBody(predDef, predMap)
    })

    predMap
  }

  protected def buildName(predId: Identifier): String = {
    predId.getName.replace(Constants.GhostSymbolPrefix, "")
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
        case (field, nestedPredDefIds) =>
          val refFieldAccessPred = buildFieldAccessPred(field)

          assert(!nestedPredDefIds.isBottom,
            "set of nested predicate definitions must not be bottom")

          nestedPredDefIds.value.toSeq match {
            case nestedPredDefId :: Nil =>
              val nonNullnessCond = sil.NeCmp(refFieldAccessPred.loc, sil.NullLit()())()
              val pred = predMap(nestedPredDefId)
              val predAccessPred = sil.PredicateAccessPredicate(
                sil.PredicateAccess(Seq(buildFieldAccess(field)), pred)(), sil.FullPerm()())()

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

  def buildFieldAccessPred(fieldId: Identifier): sil.FieldAccessPredicate = {
    sil.FieldAccessPredicate(buildFieldAccess(fieldId), sil.FullPerm()())()
  }

  def buildFieldAccess(fieldId: Identifier): sil.FieldAccess = {
    val field = sil.Field(fieldId.getName, convert(fieldId.typ))()
    sil.FieldAccess(formalArgDecl.localVar, field)()
  }
}

case class DefaultPredicateBuilder(formalArgName: String = "this")
  extends PredicateBuilder {}

case class AssertionTree(
    exps: Set[sil.Exp] = Set.empty,
    children: Map[sil.Exp, AssertionTree] = Map.empty) {

  def isEmpty: Boolean =
    exps.isEmpty && children.isEmpty

  /** Removes an expression from this node and all children nodes. */
  def remove(exp: sil.Exp): AssertionTree = {
    copy(exps - exp, children.mapValues(_.remove(exp)))
  }

  def simplify: AssertionTree = {
    val newExps = exps
      .map(Transformer.simplify)
      .filterNot(_.isInstanceOf[sil.TrueLit])

    val newChildren = children
      .map({  case (cond, child) => cond -> child.remove(cond).simplify })
      .filterNot({ case (cond, child) => child.isEmpty })

    AssertionTree(newExps, newChildren)
  }

  def unconditionalExps: Seq[sil.Exp] = exps.toSeq

  def conditionalExps: Seq[sil.Exp] =
    children.map({
      case (sil.TrueLit(), tree) => tree.toExp
      case (cond, tree) =>
        sil.Implies(cond, tree.toExp)()
    }).toSeq

  def toExp: sil.Exp = {
    val allExps = exps.toList ++ conditionalExps.toList
    if (allExps.isEmpty) sil.TrueLit()()
    else allExps.reduceLeft[sil.Exp](sil.And(_, _)())
  }

  def toExps: Seq[sil.Exp] =
    unconditionalExps ++ conditionalExps
}

case class AssertionExtractor[S <: ApronInterface[S]](
    condHeapGraph: CondHeapGraph[PredicateDrivenHeapState.EdgeStateDomain[S]],
    onlyNonRecursivePredicates: Boolean = true) {

  import PredicateDrivenHeapState._

  type StateType = PredicateDrivenHeapState.EdgeStateDomain[S]

  def heap: HeapGraph[StateType] = condHeapGraph.heap

  def predicates: Iterable[sil.Predicate] =
    predicateMap.values

  def predicateMap: Map[Identifier, sil.Predicate] =
    DefaultPredicateBuilder().build(condHeapGraph.cond.predDefs)

  def assertion: sil.Exp = assertionTree.toExp

  def assertionTree: AssertionTree = {
    nextAmbigLocalVarVertex match {
      case Some(ambigLocalVarVertex) =>
        val ambigEdges = heap.outEdges(ambigLocalVarVertex)
        val suffConds = sufficientConditions(ambigEdges)
        val accPathId = AccessPathIdentifier(List(ambigLocalVarVertex.name))(refType)
        val condHeaps = condHeapGraph.evalAccessPathId(accPathId).apply().prune.condHeaps
        val children = condHeaps.map(condSubHeap => {
          val edge = condSubHeap.takenPath(accPathId.path).edges.head
          val suffCond = suffConds(edge)
          val subExtractor = copy(condHeapGraph = condSubHeap)

          suffCond -> subExtractor.assertionTree
        }).toMap
        AssertionTree(children = children)
      case None =>
        val valAssertions = buildValueAssertions()
        val refEqualities = buildReferenceEqualities()
        val accPreds = buildAccessPredicates()
        AssertionTree(exps = accPreds, Map(sil.TrueLit()() ->
          AssertionTree(valAssertions ++ refEqualities)
        ))
    }
  }

  /** Extract numerical value assertions. */
  private def buildValueAssertions(): Set[sil.Exp] = {
    var cond = condHeapGraph.cond.valueState.valueState

    // Rename value heap identifiers to access path identifiers,
    // e.g. n0.val --> this.val
    heap.localVarEdges.groupBy(_.target).foreach({
      case (target: DefiniteHeapVertex, localVarEdges) =>
        val localVarNames = localVarEdges.map(_.source.name)
        for (valueHeapId: ValueHeapIdentifier <- target.valueHeapIds) {
          val field = valueHeapId.field
          val accPathIds = localVarNames.map(localVarName =>
            AccessPathIdentifier(List(localVarName), field))
          val replacement = new Replacement()
          replacement.value += Set[Identifier](valueHeapId) -> accPathIds.toSet
          cond = cond.merge(replacement)
        }
      case (target: SummaryHeapVertex, localVarEdges) =>
        cond = cond.removeVariables(target.valueHeapIds)
      case _ =>
    })

    // Only keep variable identifiers in the state
    val sampleExps = ApronInterfaceTranslator.translate(cond)
    val exps = sampleExps.map(DefaultSampleConverter.convert)
    exps
  }

  /** Extract equalities and inequalities of reference local variables,
    * including nullness and non-nullness expressions.
    */
  private def buildReferenceEqualities(): Set[sil.Exp] = {
      heap.localVarEdges.flatMap(localVarEdge => {
      val localVar = sil.LocalVar(localVarEdge.source.name)(sil.Ref)
      if (localVarEdge.target == NullVertex) {
        val nullnessExp = sil.EqCmp(localVar, sil.NullLit()())()
        Set[sil.Exp](nullnessExp)
      } else {
        val nonNullnessExp = sil.NeCmp(localVar, sil.NullLit()())()
        val inEqualityExps: Set[sil.Exp] = heap.localVarEdges.flatMap(otherLocalVarEdge => {
          // Do not generate useless equalities such as `r == r`
          if (localVarEdge != otherLocalVarEdge) {
            val otherLocalVar = sil.LocalVar(otherLocalVarEdge.source.name)(sil.Ref)
            if (localVarEdge.target == otherLocalVarEdge.target)
              Set[sil.Exp](sil.EqCmp(localVar, otherLocalVar)())
            else
              Set[sil.Exp](sil.NeCmp(localVar, otherLocalVar)())
          } else {
            Set.empty[sil.Exp]
          }
        })

        inEqualityExps ++ Set[sil.Exp](nonNullnessExp)
      }
    })
  }

  private def buildAccessPredicates(): Set[sil.Exp] = {
    // When there are no more local variable vertices with ambiguous
    // out-going edges, output the access predicates
    heap.possibleTargetVertices.flatMap(targetVertex => {
      val inEdges = heap.localVarEdges.filter(_.target == targetVertex)
      targetVertex match {
        case DefiniteHeapVertex(_) =>
          // For definite vertices, only output permissions for one
          // of the in-coming edges
          buildAccessPredicates(inEdges.head)
        case _ =>
          // TODO: Should not unconditionally output permissions for each
          // of the incoming edges. Check that they are not equal?
          inEdges.flatMap(buildAccessPredicates)
      }
    })
  }

  private def buildAccessPredicates(edge: Edge[StateType]): Set[sil.Exp] = {
    require(edge.source.isInstanceOf[LocalVariableVertex])

    val localVarVertex = edge.source.asInstanceOf[LocalVariableVertex]
    val foldedPredInstIds = edge.state.predInsts.foldedIds
    foldedPredInstIds.flatMap(predId => {
      if (onlyNonRecursivePredicates) {
        // val localVar = sil.LocalVar(localVarVertex.name)(sil.Ref)
        // val pred = predicateMap(predInstId.toPredDefId)
        // val predAccess = sil.PredicateAccess(Seq(localVar), pred)()
        // Set(sil.PredicateAccessPredicate(predAccess, sil.FullPerm()())().asInstanceOf[sil.Exp])

        // Exploit PredicateBuilder to directly use the body of the predicate
        val customPredBuilder = DefaultPredicateBuilder(
          formalArgName = localVarVertex.name)
        val customPredMap = customPredBuilder.build(edge.state.predDefs)
        Set(customPredMap(predId).body)
      } else {
        println("currently cannot handle")
        Set.empty[sil.Exp]
      }
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