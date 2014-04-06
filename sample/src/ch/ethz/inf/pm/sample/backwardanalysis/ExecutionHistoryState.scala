package ch.ethz.inf.pm.sample.backwardanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Assignable, Identifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation._

object ExecutionHistoryState {
  def apply[S <: State[S]](currentState:S): ExecutionHistoryState[S] = apply(currentState, NilNode[S](currentState))
}

/**
 * A special abstract state recording all operations executed on it in a history.
 * After the forward execution, the history may be executed backward.
 *
 * Was introduced as a workaround to reuse forward native method semantics by saving
 * all individual operations that are not recorded explicitly in the CFG
 *
 * @param currentState The actual inner states that is wrapped
 * @param history The recorded history of operations
 */
case class ExecutionHistoryState[S <: State[S]](currentState: S, history: ExecutionHistory[S]) extends State[ExecutionHistoryState[S]] {

  /** Executes the recorded history backward from the given post state. */
  def executeHistoryBackward(post: S): S = {

    def safeGlb(a: S, b: S): S = a.strictGlb(b.setExpression(a.expr))

    def execute(history: ExecutionHistory[S], post: S): S = {
      history match {
        case OpNode(pre, op, predecessor) =>
          var refinedPre = op.backward(post, pre)
          refinedPre = safeGlb(pre, refinedPre)
          execute(predecessor, refinedPre)

        case CondNode(pre, cond, trueBranchHistory, afterTrueBranch, falseBranchHistory, afterFalseBranch, beforeCondHistory) =>
          val trueRefinedPre = execute(trueBranchHistory, safeGlb(afterTrueBranch, post))
          val falseRefinedPre = execute(falseBranchHistory, safeGlb(afterFalseBranch, post))
          val joined = trueRefinedPre.lub(falseRefinedPre)
          var refinedPre = joined.setExpression(pre.expr)
          refinedPre = safeGlb(pre, refinedPre)
          execute(beforeCondHistory, refinedPre)

        case NilNode(pre) =>
          var refinedPre = post
          refinedPre = safeGlb(pre, refinedPre)
          refinedPre
      }
    }

    execute(history, post)
  }

  def applySemanticOp(op: SemanticOp[S]): ExecutionHistoryState[S] = {
    val forwardEval = op.forward(currentState)
    ExecutionHistoryState(forwardEval, OpNode(currentState, op, history))
  }

  override def condBranch(expr: ExpressionSet, Then: ExecutionHistoryState[S] => ExecutionHistoryState[S], Else: ExecutionHistoryState[S] => ExecutionHistoryState[S]): ExecutionHistoryState[S] = {
    var thenState = ExecutionHistoryState(currentState.assume(expr))
    var elseState = ExecutionHistoryState(currentState.assume(expr.not()))

    if (thenState.lessEqual(this.bottom())){
      elseState = Else(elseState)
    } else if (elseState.lessEqual(this.bottom())) {
      thenState = Then(thenState)
    } else {
      thenState = Then(thenState)
      elseState = Else(elseState)
    }
    val lubState = elseState.currentState.lub(thenState.currentState)
    val newHistory = CondNode(
      currentState,
      expr,
      thenState.history,
      thenState.currentState,
      elseState.history,
      elseState.currentState,
      history)
    ExecutionHistoryState(lubState, newHistory)
  }

  def createObject(typ: Type, pp: ProgramPoint, fields: Option[Set[Identifier]]): ExecutionHistoryState[S] = {
    val op = CreateObjOp[S](typ, pp, fields)
    applySemanticOp(op)
   }


  def createVariable(x: ExpressionSet, typ: Type, pp: ProgramPoint): ExecutionHistoryState[S] = {
    val op = CreateVarOp[S](x, typ, pp)
    applySemanticOp(op)
  }




  def assignVariable(x: ExpressionSet, right: ExpressionSet): ExecutionHistoryState[S] = {
    val op = AssignVarOp[S](x, right)
    applySemanticOp(op)
  }




  def assignField(obj: ExpressionSet, field: String, right: ExpressionSet): ExecutionHistoryState[S] = {
    val op = AssignFieldOp[S](obj, field, right)
    applySemanticOp(op)
  }





  def getFieldValue(obj: ExpressionSet, field: String, typ: Type): ExecutionHistoryState[S] = {
    val res = currentState.getFieldValue(obj, field, typ)
    this.copy(currentState = res)
  }

  def createNonDeterminismSource(typ: Type, pp: ProgramPoint, summary: Boolean): ExecutionHistoryState[S] = {
    val res = currentState.createNonDeterminismSource(typ, pp, summary)
    this.copy(currentState = res)
  }

  def nonDeterminismSourceAt(pp: ProgramPoint, typ: Type): ExecutionHistoryState[S] = {
    val res = currentState.nonDeterminismSourceAt(pp, typ)
    this.copy(currentState = res)
  }

  def assume(cond: ExpressionSet): ExecutionHistoryState[S] = this.copy(currentState = currentState.assume(cond))

  def testTrue(): ExecutionHistoryState[S] = this.copy(currentState = currentState.testTrue())

  def testFalse(): ExecutionHistoryState[S] = this.copy(currentState = currentState.testFalse())

  def expr: ExpressionSet = currentState.expr

  def setExpression(expr: ExpressionSet): ExecutionHistoryState[S] = this.copy(currentState = currentState.setExpression(expr))

  def removeExpression(): ExecutionHistoryState[S] = this.copy(currentState = currentState.removeExpression())


  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, keyCollectionTyp: Option[Type], tpp: ProgramPoint, fields: Option[Set[Identifier]]): ExecutionHistoryState[S] = {
    val op = CreateCollectionOp[S](collTyp, keyTyp, valueTyp, lengthTyp, keyCollectionTyp, tpp, fields)
    applySemanticOp(op)
  }

  def getSummaryCollectionIfExists(collectionSet: ExpressionSet): ExecutionHistoryState[S] = {
    val newState = currentState.getSummaryCollectionIfExists(collectionSet)
    this.copy(currentState = newState)
  }

  def insertCollectionTopElement(collectionSet: ExpressionSet, keyTop: ExpressionSet, valueTop: ExpressionSet, pp: ProgramPoint): ExecutionHistoryState[S] = {
    val op = InsertCollectionTopElemOp[S](collectionSet, keyTop, valueTop, pp)
    applySemanticOp(op)
  }

  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): ExecutionHistoryState[S] = {
    val newState = currentState.getCollectionValueByKey(collectionSet, keySet)
    this.copy(currentState = newState)
  }


  def getOriginalCollection(collectionSet: ExpressionSet): ExecutionHistoryState[S] = {
    val op = GetOriginalCollectionOp[S](collectionSet)
    applySemanticOp(op)
  }

  def insertCollectionElement(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet, pp: ProgramPoint): ExecutionHistoryState[S] = {
    val op = InsertCollectionElemOp[S](collectionSet, keySet, rightSet, pp)
    applySemanticOp(op)
  }

  def removeFirstCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet): ExecutionHistoryState[S] = {
    val op = RemoveFirstCollectionValueByValueOp[S](collectionSet, valueSet)
    applySemanticOp(op)
  }

  def assignAllCollectionKeys(collectionSet: ExpressionSet, valueSet: ExpressionSet): ExecutionHistoryState[S] = {
    val op = AssignAllCollectionKeysOp[S](collectionSet, valueSet)
    applySemanticOp(op)
  }

  def collectionContainsValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint): ExecutionHistoryState[S] = {
    val op = CollectionContainsValueOp[S](collectionSet, valueSet, booleanTyp, pp)
    applySemanticOp(op)
  }

  def getCollectionLength(collectionSet: ExpressionSet): ExecutionHistoryState[S] = {
    val op = CollectionLengthOp[S](collectionSet)
    applySemanticOp(op)
  }

  def copyCollection(fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet): ExecutionHistoryState[S] = {
    val op = CollectionCopyOp[S](fromCollectionSet, toCollectionSet)
    applySemanticOp(op)
  }

  def collectionContainsKey(collectionSet: ExpressionSet, keySet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint): ExecutionHistoryState[S] = {
    val op = CollectionContainsKeyOp[S](collectionSet, keySet, booleanTyp, pp)
    applySemanticOp(op)
  }


  def isSummaryCollection(collectionSet: ExpressionSet): Boolean = currentState.isSummaryCollection(collectionSet)

  def pruneVariables(filter: (Identifier) => Boolean): ExecutionHistoryState[S] = {
    val op = PruneVarOp[S](filter)
    applySemanticOp(op)
  }

  def optimizeSummaryNodes(): ExecutionHistoryState[S] = {
    val op = OptimizeSummaryOp[S]()
    applySemanticOp(op)
  }

  def lessEqual(r: ExecutionHistoryState[S]): Boolean = currentState.lessEqual(r.currentState)

  def factory(): ExecutionHistoryState[S] = this.copy(currentState = currentState.factory())

  def top(): ExecutionHistoryState[S] = this.copy(currentState = currentState.top())

  def bottom(): ExecutionHistoryState[S] = this.copy(currentState = currentState.bottom())

  def lub(that: ExecutionHistoryState[S]): ExecutionHistoryState[S] =
    sys.error("ExecutionHistoryState cannot take lub of arbitrary states since they may not have common history")

  def glb(that: ExecutionHistoryState[S]): ExecutionHistoryState[S] =
    sys.error("ExecutionHistoryState cannot take glb of arbitrary states since they may not have common history")

  def widening(that: ExecutionHistoryState[S]): ExecutionHistoryState[S] =
    sys.error("ExecutionHistoryState cannot perform widening on arbitrary states since they may not have common history")


  /*
   * NOTE:
   * The following methods are currently not supported but can be
   * implemented by adding new semantic ops with forward and backward transformers.
   *
   * Strategy: Implement them when they are invoked for the first time on a test.
   */

  def clearCollection(collectionSet: ExpressionSet): ExecutionHistoryState[S] = ???

  def pruneUnreachableHeap(): ExecutionHistoryState[S] = ???

  def removeObject(oldPreState: ExecutionHistoryState[S], obj: ExpressionSet, fields: Option[Set[Identifier]]): ExecutionHistoryState[S] = ???

  def backwardAssignField(oldPreState: ExecutionHistoryState[S], obj: ExpressionSet, field: String, right: ExpressionSet): ExecutionHistoryState[S] = ???

  def setCollectionToTop(collectionSet: ExpressionSet): ExecutionHistoryState[S] = ???

  def undoPruneVariables(unprunedPreState: ExecutionHistoryState[S], filter: (Identifier) => Boolean): ExecutionHistoryState[S] = ???

  def undoPruneUnreachableHeap(preState: ExecutionHistoryState[S]): ExecutionHistoryState[S] = ???

  def removeCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): ExecutionHistoryState[S] = ???

  def getKeysCollection(collectionSet: ExpressionSet): ExecutionHistoryState[S] = ???

  def extractCollectionKeys(fromCollectionSet: ExpressionSet, newKeyValueSet: ExpressionSet, fromCollectionTyp: Type, collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, pp: ProgramPoint): ExecutionHistoryState[S] = ???

  def removeCollectionKeyConnection(origCollectionSet: ExpressionSet, keyCollectionSet: ExpressionSet): ExecutionHistoryState[S] = ???

  def getCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet): ExecutionHistoryState[S] = ???

  def getCollectionValue(valueIds: ExpressionSet): ExecutionHistoryState[S] = ???

  def getCollectionKeyByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): ExecutionHistoryState[S] = ???

  def before(pp: ProgramPoint): ExecutionHistoryState[S] = ???

  def assignArrayCell(obj: ExpressionSet, index: ExpressionSet, right: ExpressionSet, typ: Type): ExecutionHistoryState[S] = ???

  def setArgument(x: ExpressionSet, right: ExpressionSet): ExecutionHistoryState[S] = ???

  def setVariableToTop(x: ExpressionSet): ExecutionHistoryState[S] = ???

  def removeVariable(x: ExpressionSet): ExecutionHistoryState[S] = ???

  def throws(t: ExpressionSet): ExecutionHistoryState[S] = ???

  def getVariableValue(id: Assignable): ExecutionHistoryState[S] = ???

  def createVariableForArgument(x: ExpressionSet, typ: Type): ExecutionHistoryState[S] = ???

  def getArrayCell(obj: ExpressionSet, index: ExpressionSet, typ: Type): ExecutionHistoryState[S] = ???

  def getArrayLength(array: ExpressionSet): ExecutionHistoryState[S] = ???

  def backwardGetVariableValue(id: Assignable): ExecutionHistoryState[S] = ???

  def backwardGetFieldValue(objs: ExpressionSet, field: String, typ: Type): ExecutionHistoryState[S] = ???

  def backwardAssignVariable(oldPreState: ExecutionHistoryState[S], x: ExpressionSet, right: ExpressionSet): ExecutionHistoryState[S] = ???

  def evalConstant(value: String, typ: Type, pp: ProgramPoint): ExecutionHistoryState[S] = ???

  def createArray(length: ExpressionSet, typ: Type, pp: ProgramPoint): ExecutionHistoryState[S] = ???

}