package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import ch.ethz.inf.pm.sample.abstractdomain._
import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.abstractdomain.vdha.Edge
import ch.ethz.inf.pm.sample.abstractdomain.vdha.HeapGraph
import semper.sil.ast.utility.Transformer
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{ApronInterface, ApronInterfaceTranslator}
import semper.sil.ast.{PredicateAccessPredicate, Predicate, Exp}
import com.weiglewilczek.slf4s.Logging

trait PredicateRegistry {
  def accessPredicates(
      variableId: sample.Identifier,
      predId: sample.PredicateIdentifier): sil.Exp

  def predAccessPred(
      variableId: sample.Identifier,
      predId: sample.PredicateIdentifier): Option[sil.PredicateAccessPredicate]

  def predicates: Seq[sil.Predicate]

  def hideShallowPredicates: Boolean
}

case class DefaultPredicateRegistry(
    map: Map[sample.PredicateIdentifier, (sample.PredicateBody, sil.Predicate)],
    hideShallowPredicates: Boolean = true)
  extends PredicateRegistry {

  def accessPredicates(
      variableId: sample.Identifier,
      predId: sample.PredicateIdentifier): sil.Exp = {
    val (samplePredBody, silPred) = map(predId)
    val localVar = DefaultSampleConverter.convert(variableId)

    if (samplePredBody.isShallow && hideShallowPredicates) {
      val formalPredArg = silPred.formalArgs.head.localVar
      silPred.body.transform()(post = {
        case rcv: sil.LocalVar if formalPredArg == rcv => localVar
      })
    } else {
      val predAccess = sil.PredicateAccess(Seq(localVar), silPred)()
      sil.PredicateAccessPredicate(predAccess, sil.FullPerm()())()
    }
  }

  def predAccessPred(
      variableId: sample.Identifier,
      predId: sample.PredicateIdentifier): Option[sil.PredicateAccessPredicate] = {
    val (samplePredBody, silPred) = map(predId)
    val localVar = DefaultSampleConverter.convert(variableId)

    if (samplePredBody.isShallow && hideShallowPredicates) {
      None
    } else {
      val predAccess = sil.PredicateAccess(Seq(localVar), silPred)()
      Some(sil.PredicateAccessPredicate(predAccess, sil.FullPerm()())())
    }
  }

  def predicates: Seq[Predicate] = {
    // Output all predicates, even the ones with a shallow body
    // The problem is that other predicates may have a nested predicate
    // with a shallow body
    // TODO: Should check if a shallow predicate is used inside of some
    // other predicate
    map.values.map(_._2).toSeq
  }
}

/** Builds a `PredicateRegistry`.
  *
  * @param formalArgName argument name of generated predicates
  */
case class PredicateRegistryBuilder(
    formalArgName: String = "this",
    hideShallowPredicates: Boolean = true) {

  def build(
      extractedPreds: sample.PredicatesDomain,
      existingSilPreds: Seq[sil.Predicate] = Seq.empty): PredicateRegistry = {
    buildImpl(extractedPreds.removeNestedTopPredicates(), existingSilPreds)
  }

  private def buildImpl(
      extractedPreds: sample.PredicatesDomain,
      existingSilPreds: Seq[sil.Predicate] = Seq.empty): PredicateRegistry = {
    val existingPreds = DefaultSilConverter.convert(existingSilPreds)

    val predMap: Map[sample.PredicateIdentifier, (sample.PredicateBody, sil.Predicate)] = extractedPreds.map.map({
      case (predId, predBody) =>
        existingPreds.findEqual(predId, predBody) match {
          case Some(existingPredId) =>
            predId -> (predBody, existingSilPreds.find(_.name == existingPredId.getName).get)
          case None =>
            val predName = buildName(predId)
            predId -> (predBody, sil.Predicate(predName, Seq(formalArgDecl), null)())
        }
    })

    predMap.foreach({
      case (predId, (samplePred, silPred)) =>
        val predBody = extractedPreds.map(predId)
        if (silPred.body == null) {
          silPred.body = buildBody(predBody, predMap)
        }
    })

    DefaultPredicateRegistry(predMap, hideShallowPredicates)
  }

  protected def buildName(predId: Identifier): String = {
    predId.getName.replace(Constants.GhostSymbolPrefix, "")
  }

  protected def buildBody(
      body: PredicateBody,
      predMap: Map[sample.PredicateIdentifier, (sample.PredicateBody, sil.Predicate)]): sil.Exp = {
    if (body.isTop)
      sil.TrueLit()()
    else if (body.isBottom)
      sil.FalseLit()()
    else {
      val accessPreds = body.map.map({
        case (field, nestedPredIds) =>
          val fieldAccessPred = buildFieldAccessPred(field)

          assert(!nestedPredIds.isBottom,
            "set of nested predicate definitions must not be bottom")

          nestedPredIds.value.toList match {
            case nestedPredId :: Nil =>
              val nonNullnessCond = sil.NeCmp(fieldAccessPred.loc, sil.NullLit()())()
              val pred = predMap(nestedPredId)._2
              val predAccessPred = sil.PredicateAccessPredicate(
                sil.PredicateAccess(Seq(buildFieldAccess(field)), pred)(), sil.FullPerm()())()

              val condPredAccessPred = sil.Implies(nonNullnessCond, predAccessPred)()

              sil.And(fieldAccessPred, condPredAccessPred)()
            case Nil =>
              fieldAccessPred
          }
      })

      accessPreds.reduceLeft[sil.Exp](sil.And(_, _)())
    }
  }

  protected def buildFieldAccessPred(fieldId: Identifier): sil.FieldAccessPredicate = {
    sil.FieldAccessPredicate(buildFieldAccess(fieldId), sil.FullPerm()())()
  }

  protected def buildFieldAccess(fieldId: Identifier): sil.FieldAccess = {
    val field = sil.Field(fieldId.getName, DefaultSampleConverter.convert(fieldId.typ))()
    sil.FieldAccess(formalArgDecl.localVar, field)()
  }

  /** Declaration of the predicate body parameter. */
  protected def formalArgDecl: sil.LocalVarDecl =
    sil.LocalVarDecl(formalArgName, sil.Ref)()
}

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
    var newExps = exps
    var newChildren = children

    newExps = newExps
      .map(Transformer.simplify)
      .filterNot(_.isInstanceOf[sil.TrueLit])

    newChildren = newChildren
      .map({ case (cond, child) => cond -> child.remove(cond).simplify })
      .filterNot({ case (cond, child) => child.isEmpty })

    // Pull up expressions that occur unconditionally in all children
    val commonExps = newChildren.values.map(_.exps).foldLeft(Set.empty[sil.Exp])(_ intersect _)
    newExps = newExps ++ commonExps
    newChildren = newChildren.mapValues(child =>
      child.copy(exps = child.exps -- commonExps))

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
    predRegistry: PredicateRegistry)
  extends Logging {

  import PredicateDrivenHeapState._

  type StateType = PredicateDrivenHeapState.EdgeStateDomain[S]

  def heap: HeapGraph[StateType] = condHeapGraph.heap

  def assertion: sil.Exp = assertionTree.toExp

  def assertionTree: AssertionTree = {
    nextAmbigLocalVarVertex match {
      case Some(ambigLocalVarVertex) =>
        val ambigEdges = heap.outEdges(ambigLocalVarVertex)
        val suffConds = sufficientConditions(ambigEdges)

        if (suffConds.isEmpty) {
          // It was not possible to find a sufficient condition
          // Remove the variable so we don't accidentally talk about
          // it in the extracted assertion
          // TODO: Find a less drastic way
          val prunedCondHeap = condHeapGraph.copy(heap = heap.removeVertices(Set(ambigLocalVarVertex)))
          // TODO: Should also remove the corresponding value heap identifier
          val subExtractor = copy(condHeapGraph = prunedCondHeap)
          subExtractor.assertionTree
        } else {
          val accPathId = AccessPathIdentifier(ambigLocalVarVertex.variable)
          val condHeaps = condHeapGraph.evalAccessPathId(accPathId).apply().prune.condHeaps
          val children = condHeaps.map(condSubHeap => {
            val edge = condSubHeap.takenPath(accPathId.stringPath).edges.head
            val suffCond = suffConds(edge)
            val subExtractor = copy(condHeapGraph = condSubHeap)

            suffCond -> subExtractor.assertionTree
          }).toMap
          AssertionTree(children = children)
        }
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

    require(heap.localVarEdges.groupBy(_.source).forall(_._2.size == 1),
      "at this point, we assume that each local variable has a single out-going edge")

    // We extract the value assertions only from the general value state.
    // At this point, the states of all local variable edges should
    // be applied to the general value state anyway.

    // We only extract value field assertions for cases where we have a
    // folded, shallow predicate definition on the edge of the local variable
    // In such cases, introduce an alias such as 'this.val' for a value
    // heap identifier such as 'n0.val'.

    val repl = new Replacement()
    for (edge <- heap.localVarEdges.filter(_.target.isInstanceOf[HeapVertex])) {
      val target = edge.target.asInstanceOf[HeapVertex]
      val localVarVertex = edge.source.asInstanceOf[LocalVariableVertex]

      for (predId <- edge.state.predInsts.foldedIds) {
        val predBody = edge.state.preds.get(predId)

        if (predRegistry.hideShallowPredicates && predBody.isShallow) {
          val fieldsWithPerm = predBody.map.keySet

          for (field <- fieldsWithPerm) {
            val valHeapId = ValueHeapIdentifier(target, field)
            val accPathId = AccessPathIdentifier(List(localVarVertex.variable), field)
            repl.value += Set[Identifier](valHeapId) -> Set[Identifier](accPathId)
          }
        }
      }
    }

    cond = cond.merge(repl)

    // Now remove all value heap ids
    cond = cond.removeVariables(cond.valueHeapIds)

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
            else {
              // For the moment, do not extract reference *inequalities*.
              // Silicon may fail to show that they hold.
              // Set[sil.Exp](sil.NeCmp(localVar, otherLocalVar)())
              Set.empty[sil.Exp]
            }
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
      if (inEdges.isEmpty) Set.empty[sil.Exp]
      else targetVertex match {
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
      Set(predRegistry.accessPredicates(localVarVertex.variable, predId))
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

    if (ambigEdges.size != 2) {
      logger.warn("Cannot find sufficient conditions " +
        "for more than two ambiguous out-going edges")
      return Map.empty
    }

    if (ambigEdges.forall(_.target != NullVertex)) {
      logger.warn("Cannot find sufficient conditions other than nullness")
      return Map.empty
    }

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