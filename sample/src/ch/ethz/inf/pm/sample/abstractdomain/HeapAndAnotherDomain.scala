package ch.ethz.inf.pm.sample.abstractdomain


import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._
import util.HeapIdSetFunctionalLifting

/**
 * A domain that combines a heap and another semantic domain.
 * The intuition is that the heap domain takes care of approximating the heap structure, while the
 * semantic domain has to manage the information of its interest without taking care of field accesses
 * and object creation, but dealing only with identifiers (of variables or of heap nodes).
 *
 * TODO:
 *  Rewrite most methods. Contains lots of (cosmetically cleaned up) legacy code. Especially,
 *  remove the timing code as it is a separate concern (use adapter to wrap domains?)
 */
class HeapAndAnotherDomain[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](val semantic: N, val  heap: H)
  extends Lattice[HeapAndAnotherDomain[N, H, I]]
  with LatticeWithReplacement[HeapAndAnotherDomain[N,H,I]] {

  type T = HeapAndAnotherDomain[N, H, I]

  override def toString : String = "Heap state:\n" +
    ToStringUtilities.indent(heap.toString) +
    "\nSemantic state:\n" +
    ToStringUtilities.indent(semantic.toString)

  def _1 = semantic
  def _2 = heap

  def getStringOfId(id : Identifier) : String = semantic.getStringOfId(id)

  def ids = _1.ids ++ _2.ids

  def factory(semantic: N, heap: H): T = new HeapAndAnotherDomain[N, H, I](semantic, heap)

  def factory(): T = top()

  def createVariableForArgument(variable : Assignable, typ : Type, path : List[String]) = {
    SystemParameters.heapTimer.start()
    val (newHeap, ids, r) = heap.createVariableForArgument(variable, typ, path)
    SystemParameters.heapTimer.stop()
    var newSemantic = semantic
    newSemantic=applyToAssignable[N](variable, newSemantic, _.createVariableForArgument(_, typ, path)._1)
    variable match {
      case x : VariableIdentifier =>
        newSemantic=newSemantic.createVariableForArgument(x, typ, path)._1
      case x : HeapIdSetDomain[I] =>
        var first : Boolean = true
        for(singleid <- x.value)
          if(first) {
            first=false
            newSemantic=newSemantic.createVariableForArgument(singleid, typ, path)._1
          }
          else
            newSemantic=x.combinator(newSemantic, newSemantic.createVariableForArgument(singleid, typ, path)._1)
    }
    //We recursively create the entry state for all the entry abstract nodes.
    SystemParameters.domainTimer.start()
    newSemantic=newSemantic.merge(r)
    for(id <- ids.keys)
      if(!id.equals(variable))
        newSemantic=newSemantic.createVariableForArgument(id, typ, ids.apply(id))._1
    SystemParameters.domainTimer.stop()
    (factory(newSemantic, newHeap), ids)
  }

  def setToTop(variable : Assignable) : T = {
    SystemParameters.heapTimer.start()
    val (newHeap, r) =heap.setToTop(variable)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    var newSemantic= semantic.merge(r)
    newSemantic = applyToAssignable[N](variable, newSemantic, _.setToTop(_))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, newHeap)
  }

  def assign(variable : Assignable, expr : Expression) : T = expr match {
    case CollectionContainsExpression(collection, key, value, returnTyp, pp) =>

      def assignContains(initialState: T, value: Expression, returnTyp: Type, pp: ProgramPoint)(collection: Assignable) = {

        val containsValue = initialState.collectionContainsValue(collection, value)

        var expression: Expression = new BinaryNondeterministicExpression(Constant("true", returnTyp, pp), Constant("false", returnTyp, pp), NondeterministicOperator.or, returnTyp)

        if (containsValue.equals(BooleanDomain.domTrue)) {
          expression = Constant("true", returnTyp, pp)
        }
        else if (containsValue.equals(BooleanDomain.domFalse)) {
          expression = Constant("false", returnTyp, pp)
        }

        initialState.assign(variable, expression)
      }

      collection match {
        case id: I => assignContains(this, value, returnTyp, pp)(id)
        case id: VariableIdentifier =>  assignContains(this, value, returnTyp, pp)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, assignContains(this, value, returnTyp, pp))
      }

    case _ =>
      SystemParameters.heapTimer.start()
      val (newHeap, r) = heap.assign(variable, expr, semantic)
      val (newHeap2, r1) = newHeap.endOfAssignment()
      SystemParameters.heapTimer.stop()
      SystemParameters.domainTimer.start()
      var newSemantic = semantic.merge(r).merge(r1)
      newSemantic = applyToAssignable[N](variable, newSemantic, _.assign(_, expr))
      SystemParameters.domainTimer.stop()
      factory(newSemantic, newHeap2)
  }

  def assignField(variable : Assignable, field : String, expr : Expression, typ : Type, pp : ProgramPoint) : T= {
    SystemParameters.heapTimer.start()
    val (newHeap, r2) = heap.assignField(variable, field, expr)
    val (id, newHeap2, r1) = newHeap.getFieldIdentifier(variable, field, typ, pp)
    val (newHeap3, r3) = newHeap2.endOfAssignment()
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = semantic.merge(r2).merge(r1).merge(r3)
    var newSemanticOpt : Option[N]= None
    if(id.isTop)
      newSemanticOpt = Some(newSemantic.top())
    else
      for(singleheapid <- id.value) {
        if(newSemanticOpt==None)
          newSemanticOpt=Some(newSemantic.assign(singleheapid, expr))
        else newSemanticOpt=Some(id.combinator(newSemanticOpt.get, newSemantic.assign(singleheapid, expr)))
      }
    val newSemanticResult =
      if(newSemanticOpt!=None)
        newSemanticOpt.get //throw new SemanticException("You should assign to something")
      else newSemantic
    SystemParameters.domainTimer.stop()
    factory(newSemanticResult, newHeap3)
  }

  def backwardAssignField(oldPreState: T, variable : Assignable, field : String, expr : Expression, typ : Type, pp : ProgramPoint) : T= {
    SystemParameters.heapTimer.start()
    val (newHeap, r2) = heap.backwardAssignField(oldPreState.heap, variable, field, expr)
    val (id, newHeap2, r1) = newHeap.getFieldIdentifier(variable, field, typ, pp)
    val (newHeap3, r3)= newHeap2.endOfAssignment()
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = semantic.merge(r2).merge(r1).merge(r3)
    var newSemanticOpt : Option[N]= None
    if(id.isTop)
      newSemanticOpt = Some(newSemantic.top())
    else
      for(singleheapid <- id.value) {
        if(newSemanticOpt==None)
          newSemanticOpt=Some(newSemantic.backwardAssign(oldPreState.semantic, singleheapid, expr))
        else newSemanticOpt=Some(id.combinator(newSemanticOpt.get, newSemantic.backwardAssign(oldPreState.semantic, singleheapid, expr)))
      }
    val newSemanticResult = newSemanticOpt.getOrElse(newSemantic)
    SystemParameters.domainTimer.stop()
    factory(newSemanticResult, newHeap3)
  }

  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, originalCollectionType: Option[Type], keyCollectionType: Option[Type], pp: ProgramPoint) : (HeapIdSetDomain[I], T, Replacement) = {

    val (collectionIds, newHeap, rep) = heap.createEmptyCollection(collTyp, keyTyp, valueTyp, lengthTyp, originalCollectionType, keyCollectionType, pp)
    var result = factory(semantic.merge(rep), newHeap)

    def setCollectionLength(initial: T, lengthTyp: Type)(collection:Assignable) = {
      var result = initial
      val collectionLength = result.heap.getCollectionLength(collection)

      def createVariable(state: T, lengthTyp:Type)(variable: Assignable) = {
        applyToAssignable[T](variable, state, _.createVariable(_, lengthTyp))
      }

      result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), collectionLength, createVariable(result, lengthTyp))
      result = setCollectionLengthToZero(result, lengthTyp)(collection)
      result
    }

    result = HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), collectionIds, setCollectionLength(result, lengthTyp))

    (collectionIds, result, rep)
  }

  def getCollectionValue(valueId: Assignable) = valueId match {
    case id:I => (this, heap.get(id))
    case _ => throw new SemanticException("This is not a collection tuple value identifier")
  }

  def getSummaryCollectionIfExists(collection: Assignable) = {
    val ids = heap.getSummaryCollectionIfExists(collection)
    (this, ids)
  }

  def insertCollectionTopElement(collection: Assignable, keyTop: Expression, valueTop: Expression, pp: ProgramPoint) = {

    val (tupleIds, newHeap, rep) = heap.insertCollectionTopElement(collection, pp)
    var result = factory(semantic.merge(rep), newHeap)

    for (tupleId <- tupleIds.value) {
      result = assignToCollectionTuple(tupleId, keyTop, valueTop, result)
    }

    result
  }

  def insertCollectionElement(collection: Assignable, key: Expression, value: Expression, pp: ProgramPoint) = {
    val (tupleIds, newHeap, rep) = heap.insertCollectionElement(collection, pp)
    var result = factory(semantic.merge(rep), newHeap)

    for (tupleId <- tupleIds.value) {
      result = assignToCollectionTuple(tupleId, key, value, result)
    }

    (result, rep)
  }

  def removeCollectionElementByKey(collection: Assignable, key: Expression): T = {
    def removeKey(initialState: T, key: Expression, id: I) = {
      val (idNotEqualsKey, _) = initialState.assume(NegatedBooleanExpression(BinaryArithmeticExpression(id, key, ArithmeticOperator.==, null)))
      if (idNotEqualsKey.lessEqual(idNotEqualsKey.bottom())){
        initialState.removeCollectionElementByTuple(initialState.heap.getCollectionTupleByKey(id))
      } else {
        idNotEqualsKey
      }
    }

    var result = this

    val mustKeyIds = result.getCollectionMustKeysWithMayBeKey(collection, key)
    for (mustKeyId <- mustKeyIds.value) {
      result = result.removeCollectionElementByTuple(heap.getCollectionTupleByKey(mustKeyId))
    }

    // Removing an element from the over-approximation if the collection is a summary node would be unsound
    // since it is only removed from one collection and not from all represented by the summary collection.
    if (this.isSummaryCollection(collection)) return result

    val keyIds = result.getCollectionMayKeysByKey(collection, key)
    for (keyId <- keyIds.value) {
      result = removeKey(result, key, keyId)
    }

    result
  }

  def setCollectionToTop(collection: Assignable): T = {
    val newHeap = heap.setCollectionToTop(collection)
    factory(semantic, newHeap)
  }

  def removeFirstCollectionElementByValue(collection: Assignable, value: Expression): T = {
    // Don't remove any value from the over-approximation (since we don't know which is the first one)
    var result = this

    val mustValueIds = result.getCollectionMustValuesWithMayBeValue(collection, value)
    for (valueId <- mustValueIds.value) {
      result = result.removeCollectionElementByTuple(heap.getCollectionTupleByValue(valueId))
    }

    result
  }

  def collectionContainsKey(collection: Assignable, key: Expression): BooleanDomain = {
    val mustKeys = getCollectionMustKeysByKey(collection, key)
    val mustContainKey = !mustKeys.isBottom

    val mayKeys = getCollectionMayKeysByKey(collection, key)
    val mayContainKey = !mayKeys.isBottom

    if (mustContainKey) {
      BooleanDomain.domTrue
    } else if (!mayContainKey) {
      BooleanDomain.domFalse
    } else {
      BooleanDomain.domTop
    }
  }

  def collectionContainsValue(collection: Assignable, value: Expression) : BooleanDomain = {
    val mustValues = getCollectionMustValuesByValue(collection, value)
    val mustContainValue = !mustValues.isBottom

    val mayValues = getCollectionMayValuesByValue(collection, value)
    val mayContainValue = !mayValues.isBottom

    if (mustContainValue) {
      BooleanDomain.domTrue
    } else if (!mayContainValue) {
      BooleanDomain.domFalse
    } else {
      BooleanDomain.domTop
    }
  }

  def getCollectionKeyByKey(collection: Assignable, key: Expression): (T, HeapIdSetDomain[I]) = {
    val mayKeys = this.getCollectionMayKeysByKey(collection, key)
    //val mustKeys = this.getCollectionMustKeysByKey(collection, key)

    (this, mayKeys)//.lub(mayKeys, mustKeys))
  }

  def getCollectionValueByKey(collection: Assignable, key: Expression): (T, HeapIdSetDomain[I]) = {
    val mayValues = this.getCollectionMayValuesByKey(collection, key)
    //val mustValues = this.getCollectionMustValuesByKey(collection, key)

    (this, mayValues)//.lub(mayValues, mustValues))
  }

  def getCollectionValueByValue(collection: Assignable, value: Expression): (T, HeapIdSetDomain[I]) = {
    val mayValues = this.getCollectionMayValuesByValue(collection, value)
    val mustValues = this.getCollectionMustValuesByValue(collection, value)
    (this, mayValues.lub(mustValues))
  }

  def clearCollection(collection:Assignable) : T = {
    def clearSingleCollection(initial:T)(collectionId: Assignable) = {
      var result: T = initial

      val overApproxIds = result.heap.getCollectionOverApproximation(collectionId)
      val underApproxIds = result.heap.getCollectionUnderApproximation(collectionId)

      // If the collection is a summary collection it would be unsound to clear the over-approximation
      // since only one of the represented collections is cleared.
      val approxIds = if (this.isSummaryCollection(collection)) underApproxIds
                      else overApproxIds.lub(underApproxIds)

      for (approxId <- approxIds.value) {
        val tupleIds = result.heap.getCollectionTuples(approxId)
        for (tupleId <- tupleIds.value) {
          val keyId = result.heap.getCollectionKeyByTuple(tupleId)
          result = result.removeVariable(keyId)

          val valueId = result.heap.getCollectionValueByTuple(tupleId)
          result = result.removeVariable(valueId)

          result = result.removeVariable(tupleId)
        }
        result = result.removeVariable(approxId)
      }

      result
    }

    // TODO: Is this distinction really needed?
    collection match {
      case id: Assignable => clearSingleCollection(this)(id)
      case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, clearSingleCollection(this))
    }
  }

  def extractCollectionKeys(fromCollection: Assignable, newKeyValue: Expression, fromCollectionTyp: Type, collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint): (T, HeapIdSetDomain[I], Replacement) = {
    var resultRep = new Replacement()
    def insertKeyAsValue(keyId:I, key: Expression, result: T)(collectionApprox: Assignable) = {
      val (res, rep) = result.insertCollectionElementToApprox(collectionApprox, key, keyId, keyId.pp)
      resultRep = resultRep ++ rep
      res
    }

    var result = this.factory()
    val (toCollectionIds, newHeap, rep) = heap.createEmptyCollection(collTyp, keyTyp, valueTyp, lengthTyp, Some(fromCollectionTyp), None, pp)
    result = factory(semantic.merge(rep), newHeap)

    val toCollectionOverApproxIds = HeapIdSetFunctionalLifting.applyToSetHeapId(new MaybeHeapIdSetDomain[I](), toCollectionIds, heap.getCollectionOverApproximation)
    val overApproxIds = heap.getCollectionOverApproximation(fromCollection)
    for (overApproxId <- overApproxIds.value) {
      val keyIds = heap.getCollectionKeys(overApproxId)
      for (keyId <- keyIds.value) {
        result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionOverApproxIds, insertKeyAsValue(keyId, newKeyValue, result))
      }
    }

    val toCollectionUnderApproxIds = HeapIdSetFunctionalLifting.applyToSetHeapId(new MaybeHeapIdSetDomain[I](), toCollectionIds, heap.getCollectionUnderApproximation)
    val underApproxIds = heap.getCollectionUnderApproximation(fromCollection)
    for (underApproxId <- underApproxIds.value) {
      val keyIds = heap.getCollectionKeys(underApproxId)
      for (keyId <- keyIds.value) {
        result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionUnderApproxIds, insertKeyAsValue(keyId, newKeyValue, result))
      }
    }

    result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionIds, setCollectionLengthFromCollection(result, fromCollection))

    (result, toCollectionIds, resultRep)
  }

  def getOriginalCollection(collection: Assignable): (T, HeapIdSetDomain[I]) = {
    (this, heap.getOriginalCollection(collection))
  }

  def getKeysCollection(collection: Assignable): (T, HeapIdSetDomain[I]) = {
    (this, heap.getKeysCollection(collection))
  }

  def removeCollectionKeyConnection(origCollection: Assignable, keysCollection: Assignable): T = {
    var result = this.factory()
    val (collectionKeysIds, newHeap,_) = heap.getFieldIdentifier(origCollection, "keys", keysCollection.typ, keysCollection.pp)
    result = factory(semantic, newHeap)
    for (collectionKeysId <- collectionKeysIds.value) {
      val newSemantic = result.semantic.removeVariable(collectionKeysId)
      val (newHeap, _) = result.heap.removeVariable(collectionKeysId)
      result = factory(newSemantic, newHeap)
    }

    val (collectionOriginalIds, newHeap2, _) = result.heap.getFieldIdentifier(keysCollection, "orig", origCollection.typ, origCollection.pp)
    result = factory(result.semantic, newHeap2)
    for (collectionOriginalId <- collectionOriginalIds.value) {
      val newSemantic2 = result.semantic.removeVariable(collectionOriginalId)
      val (newHeap3, _) = result.heap.removeVariable(collectionOriginalId)
      result = factory(newSemantic2, newHeap3)
    }
    result
  }

  def copyCollection(fromCollection: Assignable, toCollection: Assignable) : (T, Replacement) = {
    var result = this

    var resultRep = new Replacement()

    def insertToApprox(result: T, key:Expression, value:Expression, pp:ProgramPoint)(approx: Assignable): T = {
      val (res, rep) = result.insertCollectionElementToApprox(approx, key, value, pp)
      resultRep = resultRep ++ rep
      res
    }

    val overApproxIds = heap.getCollectionOverApproximation(fromCollection)
    val toCollectionOverApproxIds = heap.getCollectionOverApproximation(toCollection)
    for (overApproxId <- overApproxIds.value) {
      val tuples = heap.getCollectionTuples(overApproxId)
      for (tuple <- tuples.value) {
        val key = heap.getCollectionKeyByTuple(tuple).asInstanceOf[I]
        val value = heap.getCollectionValueByTuple(tuple).asInstanceOf[I]
        result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionOverApproxIds, insertToApprox(result, key, value, tuple.pp))
      }
    }

    val underApproxIds = heap.getCollectionUnderApproximation(fromCollection)
    val toCollectionUnderApproxIds = heap.getCollectionUnderApproximation(toCollection)
    for (underApproxId <- underApproxIds.value) {
      val tuples = heap.getCollectionTuples(underApproxId)
      for (tuple <- tuples.value) {
        val key = heap.getCollectionKeyByTuple(tuple).asInstanceOf[I]
        val value = heap.getCollectionValueByTuple(tuple).asInstanceOf[I]
        result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionUnderApproxIds, insertToApprox(result, key, value, tuple.pp))
      }
    }

    result = setCollectionLengthFromCollection(result, fromCollection)(toCollection)
    (result, resultRep)
  }

  def assignAllCollectionKeys(collection: Assignable, value: Expression) : T = {
    var result = this

    val overApproxIds = result.heap.getCollectionOverApproximation(collection)
    val underApproxIds = result.heap.getCollectionUnderApproximation(collection)
    val approxIds = overApproxIds.value ++ underApproxIds.value
    for (approxId <- approxIds) {
      val keyIds = result.heap.getCollectionKeys(approxId)
      if (! keyIds.isBottom) {
        result = factory(assignSemanticValue(keyIds, value, result.semantic), result.heap)
      }
    }

    result
  }

  def isSummaryCollection(collection: Assignable): Boolean = {
    heap.isSummaryCollection(collection)
  }

  def optimizeSummaryNodes(): (T, Replacement) = {
    val (newHeap,rep) = heap.optimizeSummaryNodes
    (factory(semantic.merge(rep), newHeap), rep)
  }

   private def getCollectionMustValuesByValue(collection: Assignable, value: Expression): HeapIdSetDomain[I] = {
    var matchedIds = new MaybeHeapIdSetDomain[I]().bottom()
    val underApproxIds = heap.getCollectionUnderApproximation(collection)
    for (underApproxId <- underApproxIds.value) {
      val valueIds = heap.getCollectionValues(underApproxId)
      for (valueId <- valueIds.value) {
        if (this.areEqual(valueId, value).equals(BooleanDomain.domTrue)) {
          matchedIds = matchedIds.add(valueId)
        }
      }
    }

    matchedIds
  }

  private def getCollectionMayValuesByValue(collection: Assignable, value: Expression): HeapIdSetDomain[I] = {
    var matchedIds = new MaybeHeapIdSetDomain[I]().bottom()
    val overApproxIds = heap.getCollectionOverApproximation(collection)
    for (overApproxId <- overApproxIds.value) {
      val valueIds = heap.getCollectionValues(overApproxId)
      for (valueId <- valueIds.value) {
        if (BooleanDomain.domTrue.lessEqual(this.areEqual(valueId, value))) {
          matchedIds = matchedIds.add(valueId)
        }
      }
    }

    matchedIds
  }

  private def getCollectionMustValuesWithMayBeValue(collection: Assignable, value: Expression): HeapIdSetDomain[I] ={
    var matchedIds = new MaybeHeapIdSetDomain[I]().bottom()
    val underApproxIds = heap.getCollectionUnderApproximation(collection)
    for (underApproxId <- underApproxIds.value) {
      val valueIds = heap.getCollectionValues(underApproxId)
      for (valueId <- valueIds.value) {
        if (BooleanDomain.domTrue.lessEqual(this.areEqual(valueId, value))) {
          matchedIds = matchedIds.add(valueId)
        }
      }
    }

    matchedIds
  }

  private def getCollectionMayValuesByKey(collection: Assignable, key: Expression): HeapIdSetDomain[I] = {
    var matchedIds = new MaybeHeapIdSetDomain[I]().bottom()

    val overApproxIds = heap.getCollectionOverApproximation(collection)
    for (overApproxId <- overApproxIds.value) {
      val tupleIds = heap.getCollectionTuples(overApproxId)
      for (tupleId <- tupleIds.value) {
        val keyId = heap.getCollectionKeyByTuple(tupleId).asInstanceOf[I]
        val valueId = heap.getCollectionValueByTuple(tupleId).asInstanceOf[I]
        if (BooleanDomain.domTrue.lessEqual(this.areEqual(keyId, key))) {
          matchedIds = matchedIds.add(valueId)
        }
      }
    }

    matchedIds
  }

  private def getCollectionMustKeysByKey(collection: Assignable, key: Expression): HeapIdSetDomain[I] = {
    var matchedIds = new MaybeHeapIdSetDomain[I]().bottom()
    val underApproxIds = heap.getCollectionUnderApproximation(collection)
    for (underApproxId <- underApproxIds.value) {
      val keyIds = heap.getCollectionKeys(underApproxId)
      for (keyId <- keyIds.value) {
        if (this.areEqual(keyId, key).equals(BooleanDomain.domTrue)) {
          matchedIds = matchedIds.add(keyId)
        }
      }
    }

    matchedIds
  }

  private def getCollectionMayKeysByKey(collection: Assignable, key: Expression): HeapIdSetDomain[I] = {
    var matchedIds = new MaybeHeapIdSetDomain[I]().bottom()
    val overApproxIds = heap.getCollectionOverApproximation(collection)
    for (overApproxId <- overApproxIds.value) {
      val keyIds = heap.getCollectionKeys(overApproxId)
      for (keyId <- keyIds.value) {
        if (BooleanDomain.domTrue.lessEqual(this.areEqual(keyId, key))) {
          matchedIds = matchedIds.add(keyId)
        }
      }
    }

    matchedIds
  }

  private def getCollectionMustKeysWithMayBeKey(collection: Assignable, key: Expression): HeapIdSetDomain[I] ={
    var matchedIds = new MaybeHeapIdSetDomain[I]().bottom()
    val underApproxIds = heap.getCollectionUnderApproximation(collection)
    for (underApproxId <- underApproxIds.value) {
      val keyIds = heap.getCollectionKeys(underApproxId)
      for (keyId <- keyIds.value) {
        if (BooleanDomain.domTrue.lessEqual(this.areEqual(keyId, key))) {
          matchedIds = matchedIds.add(keyId)
        }
      }
    }

    matchedIds
  }

  private def removeCollectionElementByTuple(tuple: Assignable) = tuple match {
    case tupleId: I =>
      val key = heap.getCollectionKeyByTuple(tupleId)
      var result = this.removeVariable(key)

      val value = heap.getCollectionValueByTuple(tupleId)
      result = result.removeVariable(value)

      result = result.removeVariable(tupleId)
      result = factory(result.semantic, result.heap.removeCollectionElement(tupleId))

      result
    case _ =>
      throw new SemanticException("This is not a tuple identifier " + tuple.toString)
  }

  private def increaseCollectionLength(collection: Assignable) = {
    def setCollectionLength(initial: T)(lengthId: Assignable) = {
      val result = applyToAssignable[T](lengthId, initial, _.assign(_, BinaryArithmeticExpression(lengthId.asInstanceOf[I], Constant("1", lengthId.typ, null), ArithmeticOperator.+, lengthId.typ)))

      if (isSummaryCollection(collection)) {
        result.lub(initial)
      } else {
        result
      }
    }
    val lengthIds = heap.getCollectionLength(collection)
    HeapIdSetFunctionalLifting.applyToSetHeapId(this, lengthIds, setCollectionLength(this))
  }

  private def decreaseCollectionLength(collection: Assignable) = {
    def setCollectionLength(initial: T)(lengthId: Assignable) = {
      var result = applyToAssignable[T](lengthId, initial, _.assign(_, BinaryArithmeticExpression(lengthId.asInstanceOf[I], Constant("1", lengthId.typ, null), ArithmeticOperator.-, lengthId.typ)))
      val (res, _) = result.assume(BinaryArithmeticExpression(lengthId.asInstanceOf[I], Constant("0", lengthId.typ, null), ArithmeticOperator.>=, lengthId.typ))
      result = res

      if(isSummaryCollection(collection)) {
        result.lub(initial)
      } else {
        result
      }
    }
    val lengthIds = heap.getCollectionLength(collection)
    HeapIdSetFunctionalLifting.applyToSetHeapId(this, lengthIds, setCollectionLength(this))
  }

  private def insertCollectionElementToApprox(collectionApprox: Assignable, key: Expression, value: Expression, pp: ProgramPoint) = {
    val (tupleIds, newHeap, rep) = heap.insertCollectionElementToApprox(collectionApprox, pp)
    val newSemantic = semantic.merge(rep)
    var result = factory(newSemantic, newHeap)

    for (tupleId <- tupleIds.value) {
      result = assignToCollectionTuple(tupleId, key, value, result)
    }

    (result, rep)
  }

  private def setCollectionLengthFromCollection(initial: T, fromCollection: Assignable)(toCollection: Assignable) = {
    def setCollectionLength(initial: T, toCollection: Assignable)(a: Assignable) = a match{
      case lengthId: I =>
        def assignLength(state: T, lengthId: I)(variable: Assignable) = {
          var result = state.createVariable(variable, lengthId.typ)
          result = applyToAssignable[T](variable, result, _.assign(_, lengthId))

          if(isSummaryCollection(toCollection)) {
            result.lub(state)
          } else {
            result
          }
        }

        val result = initial
        val lengthIds = result.heap.getCollectionLength(toCollection)
        HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), lengthIds, assignLength(result, lengthId))
      case _ =>
        throw new SemanticException("This is not a collection length " + a.toString)
    }

    val result = initial

    val lengthIds = result.heap.getCollectionLength(fromCollection)
    HeapIdSetFunctionalLifting.applyToSetHeapId(initial.factory(), lengthIds, setCollectionLength(result, toCollection))
  }

  private def setCollectionLengthToZero(initial: T, lengthTyp:Type)(collection: Assignable) : T = {
    var result = initial

    val lengthIds = result.heap.getCollectionLength(collection)
    result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), lengthIds, result.createVariable(_, lengthTyp))

    def setToZero(initialState:N)(a:Assignable) = applyToAssignable[N](a, initialState, _.assign(_, Constant("0", a.typ, null)))
    val newSemantic = HeapIdSetFunctionalLifting.applyToSetHeapId(result.semantic.factory(), lengthIds, setToZero(result.semantic))
    result = factory(newSemantic, result.heap)

    if (result.isSummaryCollection(collection)) {
      result.lub(initial)
    } else {
      result
    }
  }

  private def assignSemanticValue(ids: HeapIdSetDomain[I], value: Expression, initial: N) : N = {
    def assignValueTo(initialState: N, value: Expression)(a: Assignable) = {
      applyToAssignable[N](a, initialState, _.assign(_, value))
    }

    if(ids.isTop){
      initial.top()
    } else {
      HeapIdSetFunctionalLifting.applyToSetHeapId(initial.factory(), ids, assignValueTo(initial, value))
    }
  }

  private def assignToCollectionTuple(tupleId: Assignable, key: Expression, value: Expression, initial:HeapAndAnotherDomain[N,H,I]) = {
    var result = initial
    val keyId = result.heap.getCollectionKeyByTuple(tupleId)
    val valueId = result.heap.getCollectionValueByTuple(tupleId)

    if (!result.ids.contains(keyId.asInstanceOf[Identifier]))
      result = applyToAssignable[T](keyId, result, _.createVariable(_, keyId.typ))
    result = applyToAssignable[T](keyId, result, _.assign(_, key))

    if (!result.ids.contains(valueId.asInstanceOf[Identifier]))
      result = applyToAssignable[T](valueId, result, _.createVariable(_, valueId.typ))
    result = applyToAssignable[T](valueId, result, _.assign(_, value))

    result
  }

  def setArgument(variable : Assignable, expr : Expression) : T= {
    SystemParameters.heapTimer.start()
    val (newHeap, r) = heap.setArgument(variable, expr)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    var newSemantic = semantic.merge(r)
    newSemantic = applyToAssignable[N](variable, newSemantic, _.setArgument(_, expr))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, newHeap)
  }

  def assume(expr : Expression) : (T, Replacement) = expr match {
    case CollectionContainsExpression(collection, key, value, returnTyp, pp) =>
      var resultRep = new Replacement()
      def insertCollectionElement(initialState:T , key: Expression, value: Expression, pp: ProgramPoint)(collection: Assignable): T = {
        if (initialState.collectionContainsKey(collection, key).equals(BooleanDomain.domFalse)) return initialState.bottom()
        if (initialState.collectionContainsValue(collection, value).equals(BooleanDomain.domFalse)) return initialState.bottom()

        val (inserted, rep) = initialState.insertCollectionElement(collection, key, value, pp)
        resultRep = resultRep ++ rep
        inserted.increaseCollectionLength(collection)
      }

      val result = collection match {
        case id: I => insertCollectionElement(this, key, value, pp)(id)
        case id: VariableIdentifier => insertCollectionElement(this, key, value, pp)(id)
        case set: HeapIdSetDomain[I] =>  HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, insertCollectionElement(this, key, value, pp))
      }

      (result, resultRep)
    case NegatedBooleanExpression(CollectionContainsExpression(collection, key, value, _, _)) =>
      def removeCollectionElement(initialState: T, key: Expression, value: Expression)(collection: Assignable): T = {
        if (initialState.collectionContainsKey(collection, key).equals(BooleanDomain.domTrue)) return initialState.bottom()
        if (initialState.collectionContainsValue(collection, value).equals(BooleanDomain.domTrue)) return initialState.bottom()

        val removed = initialState.removeCollectionElementByKey(collection, key)
        removed.decreaseCollectionLength(collection)
      }

      val result = collection match {
        case id: I => removeCollectionElement(this, key, value)(id)
        case id: VariableIdentifier => removeCollectionElement(this, key, value)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, removeCollectionElement(this, key, value))
      }
      (result, new Replacement)
    case BinaryBooleanExpression(_, _, BooleanOperator.&&,  _) =>
      val binaryBoolExpr = expr.asInstanceOf[BinaryBooleanExpression]
      val (result, rep1) = this.assume(binaryBoolExpr.left)
      val (result2, rep2) = result.assume(binaryBoolExpr.right)
      (result2, rep1 ++ rep2)
    case _ =>
      SystemParameters.heapTimer.start()
      val (newHeap, r) = heap.assume(expr)
      SystemParameters.heapTimer.stop()
      SystemParameters.domainTimer.start()
      val newSemantic = semantic.merge(r).assume(expr)
      SystemParameters.domainTimer.stop()
      (factory(newSemantic, newHeap), r)
  }

  def areEqual(left: Expression, right: Expression) : BooleanDomain = {
    val equalsExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.==, null)

    val (leftEqualsRight, _) = this.assume(equalsExpression)
    val (leftNotEqualsRight, _) = this.assume(NegatedBooleanExpression(equalsExpression))

    if (!leftEqualsRight.lessEqual(this.bottom()) && leftNotEqualsRight.lessEqual(this.bottom())) {
      // must be equal
      return BooleanDomain.domTrue
    } else if (leftEqualsRight.lessEqual(this.bottom())) {
      // must be not equal
      return BooleanDomain.domFalse
    }

    BooleanDomain.domTop
  }

  def createVariable(variable : Assignable, typ : Type): T = {
    SystemParameters.heapTimer.start()
    val (newHeap, r) = heap.createVariable(variable, typ)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = applyToAssignable[N](variable, semantic.merge(r), _.createVariable(_, typ))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, newHeap)
  }
  def removeVariable(variable : Assignable) : T= {
    SystemParameters.heapTimer.start()
    val (newHeap, r) = heap.removeVariable(variable)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = applyToAssignable[N](variable, semantic.merge(r), _.removeVariable(_))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, newHeap)
  }

  def access(field: Assignable): T = {
    //Access does not change the state of the heap domain
    val newSemantic = applyToAssignable[N](field, semantic, _.access(_))
    factory(newSemantic, heap)
  }

  def backwardAccess(field : Assignable) : T= {
    SystemParameters.domainTimer.start()
    val newSemantic = applyToAssignable[N](field, semantic, _.backwardAccess(_))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, heap)
  }
  def backwardAssign(oldPreState: T, variable : Assignable, expr : Expression): T = {
    SystemParameters.heapTimer.start()
    val (newHeap, r) = heap.backwardAssign(oldPreState.heap, variable, expr)
    val (newHeap2, _) = oldPreState.heap.glbWithReplacement(newHeap)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    var newSemantic = semantic.merge(r)
    newSemantic = applyToAssignable[N](variable, newSemantic, _.backwardAssign(oldPreState.semantic, _, expr))
    SystemParameters.domainTimer.stop()
    factory(newSemantic, newHeap2)
  }

  override def top(): T = factory(semantic.top(), heap.bottom())

  override def bottom(): T = factory(semantic.bottom(), heap.bottom())

  override def lubWithReplacement(other: T): (T, Replacement) = {
    SystemParameters.heapTimer.start()
    val (newHeap, rep) = heap.lubWithReplacement(other.heap)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = semantic.merge(rep).lub(other.semantic.merge(rep))
    SystemParameters.domainTimer.stop()
    (factory(newSemantic, newHeap), rep)
  }

  override def lub(other: T): T = lubWithReplacement(other)._1

  override def glbWithReplacement(other: T): (T, Replacement) = {
    SystemParameters.heapTimer.start()
    val (newHeap, rep) = heap.glbWithReplacement(other.heap)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = semantic.merge(rep).glb(other.semantic.merge(rep))
    SystemParameters.domainTimer.stop()
    (factory(newSemantic, newHeap), rep)
  }

  override def glb(other: T): T = glbWithReplacement(other)._1

  override def wideningWithReplacement(other: T): (T, Replacement) = {
    SystemParameters.heapTimer.start()
    val (newHeap, rep) = heap.wideningWithReplacement(other.heap)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val newSemantic = semantic.merge(rep).widening(other.semantic.merge(rep))
    SystemParameters.domainTimer.stop()
    (factory(newSemantic, newHeap), rep)
  }

  override def widening(other: T): T = wideningWithReplacement(other)._1

  override def lessEqual(other : T) : Boolean = {
    if (semantic.lessEqual(semantic.bottom())) return true
    if (other.semantic.lessEqual(other.semantic.bottom())) return false
    SystemParameters.heapTimer.start()
    var b = heap.lessEqual(other.heap)
    SystemParameters.heapTimer.stop()
    if(! b) return false
    SystemParameters.domainTimer.start()
    b = semantic.lessEqual(other.semantic)
    SystemParameters.domainTimer.stop()
    b
  }

  private def applyToAssignable[L <: Lattice[L]](variable : Assignable, state : L, functor : (L, Identifier) => L) : L = {
    variable match {
      case x : VariableIdentifier => functor(state, x)
      case x :I => functor(state, x): L
    }
  }


}