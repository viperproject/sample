package ch.ethz.inf.pm.sample.abstractdomain


import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._
import util.HeapIdSetFunctionalLifting

/**
 * An abstract semantic domain that combines and heap and another semantic domain.
 * The intuition is that the heap domain takes care of approximating the heap structure, while the
 * semantic domain has to manage the information of its interest without taking care of field accesses
 * and object creation, but dealing only with identifiers (of variables or of heap nodes).
 *
 * @param <N> The semantic domain
 * @param <H> The heap analysis
 * @param <I> The heap identifiers
 * @author Pietro Ferrara
 * @since 0.1
 */
class HeapAndAnotherDomain[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](private var d1 : N, private var d2 : H)
  extends Lattice[HeapAndAnotherDomain[N, H, I]]
  with LatticeWithReplacement[HeapAndAnotherDomain[N,H,I]] {

  override def toString() : String = "Heap state:\n"+ToStringUtilities.indent(d2.toString())+"\nSemantic state:\n"+ToStringUtilities.indent(d1.toString())

  type T = HeapAndAnotherDomain[N, H, I];

  def _1 = d1;
  def _2 = d2;

  def merge(r : Replacement) : T = if(r.isEmpty) return this; else throw new SemanticException("Merge not yet implemented");

  def getStringOfId(id : Identifier) : String = d1.getStringOfId(id)

  def getHeap() : H = return d2;
  def getSemanticDomain() : N = return d1;

  def getIds() = this._1.getIds()++this._2.getIds();

  def factory() = {
    //    assert(d1 != null);
    //    assert(d2 != null);
    new HeapAndAnotherDomain[N, H, I](d1.factory(), d2.factory());
  }

  def createVariableForArgument(variable : Assignable, typ : Type, path : List[String]) = {
    SystemParameters.heapTimer.start();
    val (s2, ids, r) = this.d2.createVariableForArgument(variable, typ, path);
    SystemParameters.heapTimer.stop();
    var s1 = this.d1;
    s1=applyToAssignable[N](variable, s1, _.createVariableForArgument(_, typ, path)._1);
    variable match {
      case x : VariableIdentifier =>
        s1=s1.createVariableForArgument(x, typ, path)._1
      case x : HeapIdSetDomain[I] =>
        var first : Boolean = true;
        for(singleid <- x.value)
          if(first) {
            first=false;
            s1=s1.createVariableForArgument(singleid, typ, path)._1;
          }
          else
            s1=x.combinator(s1, s1.createVariableForArgument(singleid, typ, path)._1);
    }
    //We recursively create the entry state for all the entry abstract nodes.
    SystemParameters.domainTimer.start();
    s1=s1.merge(r)
    for(id <- ids.keys)
      if(!id.equals(variable))
        s1=s1.createVariableForArgument(id, typ, ids.apply(id))._1;
    SystemParameters.domainTimer.stop();
    (new HeapAndAnotherDomain[N, H, I](s1, s2), ids)
  }

  def setToTop(variable : Assignable) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.setToTop(variable)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r)
    result.d1=applyToAssignable[N](variable, result.d1, _.setToTop(_));
    SystemParameters.domainTimer.stop();
    result
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
      var result = this.factory()
      result.d1 = this.d1
      result.d2 = this.d2

      collection match {
        case id: I => result = assignContains(result, value, returnTyp, pp)(id)
        case id: VariableIdentifier => result = assignContains(result, value, returnTyp, pp)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, assignContains(result, value, returnTyp, pp))
      }
      result

    case _ =>
      val result : T = this.factory()
      SystemParameters.heapTimer.start();
      val (d,r)=d2.assign(variable, expr, d1)
      val (d3, r1) = d.endOfAssignment();
      result.d2=d3;
      SystemParameters.heapTimer.stop();
      SystemParameters.domainTimer.start();
      result.d1=d1.merge(r).merge(r1)
      result.d1=applyToAssignable[N](variable, result.d1, _.assign(_, expr));
      SystemParameters.domainTimer.stop();
      result
  }

  def assignField(variable : Assignable, field : String, expr : Expression, typ : Type, pp : ProgramPoint) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (h2,r2)=d2.assignField(variable, field, expr)
    val (id, h, r1) = h2.getFieldIdentifier(variable, field, typ, pp)
    val (h3, r3)= h.endOfAssignment();
    result.d2=h3;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r2).merge(r1).merge(r3);
    var newd1 : Option[N]= None;
    if(id.isTop)
      newd1 = Some(result.d1.top());
    else
      for(singleheapid <- id.value) {
        if(newd1==None)
          newd1=Some(result.d1.assign(singleheapid, expr))
        else newd1=Some(id.combinator(newd1.get, result.d1.assign(singleheapid, expr)))
      }
    if(newd1!=None)
      result.d1=newd1.get; //throw new SemanticException("You should assign to something")
    else result.d1=result.d1;
    SystemParameters.domainTimer.stop();
    result
  }

  def assignArrayCell(variable : Assignable, index : Expression, expr : Expression, typ : Type) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (h2,r2)=d2.assignArrayCell(variable, index, expr, d1)
    val (id, h, r1) = h2.getArrayCell(variable, index, d1, typ)
    val (h3, r3)= h.endOfAssignment();
    result.d2=h3;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r2).merge(r1).merge(r3);
    var newd1 : Option[N]= None;
    if(id.isTop)
      newd1 = Some(result.d1.top());
    else
      for(singleheapid <- id.value) {
        if(newd1==None)
          newd1=Some(result.d1.assign(singleheapid, expr))
        else newd1=Some(id.combinator(newd1.get, result.d1.assign(singleheapid, expr)))
      }
    if(newd1!=None)
      result.d1=newd1.get; //throw new SemanticException("You should assign to something")
    else result.d1=result.d1;
    SystemParameters.domainTimer.stop();
    result
  }

  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, originalCollectionType: Option[Type], keyCollectionType: Option[Type], pp: ProgramPoint) : (HeapIdSetDomain[I], HeapAndAnotherDomain[N,H,I], Replacement) = {

    val (collectionIds, newHeap, rep) = this.d2.createEmptyCollection(collTyp, keyTyp, valueTyp, lengthTyp, originalCollectionType, keyCollectionType, pp)
    var result = new HeapAndAnotherDomain[N,H,I](this.d1.merge(rep), newHeap)

    def setCollectionLength(initial: HeapAndAnotherDomain[N,H,I], lengthTyp: Type)(collection:Assignable) = {
      var result = initial
      val collectionLength = result.d2.getCollectionLength(collection)

      def createVariable(state: HeapAndAnotherDomain[N,H,I], lengthTyp:Type)(variable: Assignable) = {
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
    case id:I => (this, this.d2.get(id))
    case _ => throw new SemanticException("This is not a collection tuple value identifier")
  }

  def getSummaryCollectionIfExists(collection: Assignable) = {
    val ids = this.d2.getSummaryCollectionIfExists(collection)
    (this, ids)
  }

  def insertCollectionTopElement(collection: Assignable, keyTop: Expression, valueTop: Expression, pp: ProgramPoint) = {
    var result = this.factory()
    val (tupleIds, newHeap, rep) = this.d2.insertCollectionTopElement(collection, pp)
    result.d1 = this.d1.merge(rep)
    result.d2 = newHeap

    for (tupleId <- tupleIds.value) {
      result = assignToCollectionTuple(tupleId, keyTop, valueTop, result)
    }

    result
  }

  def insertCollectionElement(collection: Assignable, key: Expression, value: Expression, pp: ProgramPoint) = {
    var result: T = this.factory()
    val (tupleIds, newHeap, rep) = this.d2.insertCollectionElement(collection, pp)
    result.d1 = this.d1.merge(rep)
    result.d2 = newHeap

    for (tupleId <- tupleIds.value) {
      result = assignToCollectionTuple(tupleId, key, value, result)
    }

    (result, rep)
  }

  def removeCollectionElementByKey(collection: Assignable, key: Expression): T = {
    def removeKey(initialState: T, key: Expression, id: I) = {
      val (idNotEqualsKey, _) = initialState.assume(NegatedBooleanExpression(BinaryArithmeticExpression(id, key, ArithmeticOperator.==, null)))
      if (idNotEqualsKey.lessEqual(idNotEqualsKey.bottom())){
        initialState.removeCollectionElementByTuple(initialState.d2.getCollectionTupleByKey(id))
      } else {
        idNotEqualsKey
      }
    }

    var result = this.factory()
    result.d1 = this.d1
    result.d2 = this.d2

    val mustKeyIds = result.getCollectionMustKeysWithMayBeKey(collection, key)
    for (mustKeyId <- mustKeyIds.value) {
      result = result.removeCollectionElementByTuple(this.d2.getCollectionTupleByKey(mustKeyId))
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

  def removeFirstCollectionElementByValue(collection: Assignable, value: Expression): T = {
    // Don't remove any value from the over-approximation (since we don't know which is the first one)
    var result = this.factory()
    result.d1 = this.d1
    result.d2 = this.d2

    val mustValueIds = result.getCollectionMustValuesWithMayBeValue(collection, value)
    for (valueId <- mustValueIds.value) {
      result = result.removeCollectionElementByTuple(this.d2.getCollectionTupleByValue(valueId))
    }

    result
  }

  def collectionContainsKey(collection: Assignable, key: Expression): BooleanDomain = {
    val mustKeys = getCollectionMustKeysByKey(collection, key)
    val mustContainKey = !mustKeys.isBottom

    val mayKeys = getCollectionMayKeysByKey(collection, key)
    val mayContainKey = !mayKeys.isBottom

    if (mustContainKey) {
      return BooleanDomain.domTrue
    } else if (!mayContainKey) {
      return BooleanDomain.domFalse
    } else {
      return BooleanDomain.domTop
    }
  }

  def collectionContainsValue(collection: Assignable, value: Expression) : BooleanDomain = {
    val mustValues = getCollectionMustValuesByValue(collection, value)
    val mustContainValue = !mustValues.isBottom

    val mayValues = getCollectionMayValuesByValue(collection, value)
    val mayContainValue = !mayValues.isBottom

    if (mustContainValue) {
      return BooleanDomain.domTrue
    } else if (!mayContainValue) {
      return BooleanDomain.domFalse
    } else {
      return BooleanDomain.domTop
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
    (this, mayValues.lub(mayValues, mustValues))
  }

  def clearCollection(collection:Assignable) : T = {
    def clearSingleCollection(initial:T)(collectionId: Assignable) = {
      var result: T = initial

      val overApproxIds = result.d2.getCollectionOverApproximation(collectionId)
      val underApproxIds = result.d2.getCollectionUnderApproximation(collectionId)

      // If the collection is a summary collection it would be unsound to clear the over-approximation
      // since only one of the represented collections is cleared.
      val approxIds = if (this.isSummaryCollection(collection)) underApproxIds
                      else overApproxIds.lub(overApproxIds, underApproxIds)

      for (approxId <- approxIds.value) {
        val tupleIds = result.d2.getCollectionTuples(approxId)
        for (tupleId <- tupleIds.value) {
          val keyId = result.d2.getCollectionKeyByTuple(tupleId)
          result = result.removeVariable(keyId)

          val valueId = result.d2.getCollectionValueByTuple(tupleId)
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
      val (res, rep) = result.insertCollectionElementToApprox(collectionApprox, key, keyId, keyId.getProgramPoint())
      resultRep = resultRep ++ rep
      res
    }

    var result = this.factory()
    val (toCollectionIds, newHeap, rep) = this.d2.createEmptyCollection(collTyp, keyTyp, valueTyp, lengthTyp, Some(fromCollectionTyp), None, pp)
    result.d1 = this.d1.merge(rep)
    result.d2 = newHeap

    val toCollectionOverApproxIds = HeapIdSetFunctionalLifting.applyToSetHeapId(new MaybeHeapIdSetDomain[I](), toCollectionIds, this.d2.getCollectionOverApproximation(_))
    val overApproxIds = this.d2.getCollectionOverApproximation(fromCollection)
    for (overApproxId <- overApproxIds.value) {
      val keyIds = this.d2.getCollectionKeys(overApproxId)
      for (keyId <- keyIds.value) {
        result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionOverApproxIds, insertKeyAsValue(keyId, newKeyValue, result))
      }
    }

    val toCollectionUnderApproxIds = HeapIdSetFunctionalLifting.applyToSetHeapId(new MaybeHeapIdSetDomain[I](), toCollectionIds, this.d2.getCollectionUnderApproximation(_))
    val underApproxIds = this.d2.getCollectionUnderApproximation(fromCollection)
    for (underApproxId <- underApproxIds.value) {
      val keyIds = this.d2.getCollectionKeys(underApproxId)
      for (keyId <- keyIds.value) {
        result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionUnderApproxIds, insertKeyAsValue(keyId, newKeyValue, result))
      }
    }

    result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionIds, setCollectionLengthFromCollection(result, fromCollection))

    (result, toCollectionIds, resultRep)
  }

  def getOriginalCollection(collection: Assignable): (T, HeapIdSetDomain[I]) = {
    (this, this.d2.getOriginalCollection(collection))
  }

  def getKeysCollection(collection: Assignable): (T, HeapIdSetDomain[I]) = {
    (this, this.d2.getKeysCollection(collection))
  }

  def removeCollectionKeyConnection(origCollection: Assignable, keysCollection: Assignable): T = {
    val result = this.factory()
    val (collectionKeysIds, newHeap,_) = this.d2.getFieldIdentifier(origCollection, "keys", keysCollection.getType(), keysCollection.getProgramPoint())
    result.d1 = this.d1
    result.d2 = newHeap
    for (collectionKeysId <- collectionKeysIds.value) {
      result.d1 = result.d1.removeVariable(collectionKeysId)
      val (h, _) = result.d2.removeVariable(collectionKeysId)
      result.d2 = h
    }

    val (collectionOriginalIds, newHeap2, _) = result.d2.getFieldIdentifier(keysCollection, "orig", origCollection.getType(), origCollection.getProgramPoint())
    result.d2 = newHeap2
    for (collectionOriginalId <- collectionOriginalIds.value) {
      result.d1 = result.d1.removeVariable(collectionOriginalId)
      val (h, _) = result.d2.removeVariable(collectionOriginalId)
      result.d2 = h
    }
    result
  }

  def copyCollection(fromCollection: Assignable, toCollection: Assignable) : (T, Replacement) = {
    var result = this.factory()
    result.d1 = this.d1
    result.d2 = this.d2

    var resultRep = new Replacement()

    def insertToApprox(result:HeapAndAnotherDomain[N, H, I], key:Expression, value:Expression, pp:ProgramPoint)(approx: Assignable): HeapAndAnotherDomain[N, H, I] = {
      val (res, rep) = result.insertCollectionElementToApprox(approx, key, value, pp)
      resultRep = resultRep ++ rep
      res
    }

    val overApproxIds = this.d2.getCollectionOverApproximation(fromCollection)
    val toCollectionOverApproxIds = this.d2.getCollectionOverApproximation(toCollection)
    for (overApproxId <- overApproxIds.value) {
      val tuples = this.d2.getCollectionTuples(overApproxId)
      for (tuple <- tuples.value) {
        val key = this.d2.getCollectionKeyByTuple(tuple).asInstanceOf[I]
        val value = this.d2.getCollectionValueByTuple(tuple).asInstanceOf[I]
        result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionOverApproxIds, insertToApprox(result, key, value, tuple.getProgramPoint()))
      }
    }

    val underApproxIds = this.d2.getCollectionUnderApproximation(fromCollection)
    val toCollectionUnderApproxIds = this.d2.getCollectionUnderApproximation(toCollection)
    for (underApproxId <- underApproxIds.value) {
      val tuples = this.d2.getCollectionTuples(underApproxId)
      for (tuple <- tuples.value) {
        val key = this.d2.getCollectionKeyByTuple(tuple).asInstanceOf[I]
        val value = this.d2.getCollectionValueByTuple(tuple).asInstanceOf[I]
        result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionUnderApproxIds, insertToApprox(result, key, value, tuple.getProgramPoint()))
      }
    }

    result = setCollectionLengthFromCollection(result, fromCollection)(toCollection)
    (result, resultRep)
  }

  def assignAllCollectionKeys(collection: Assignable, value: Expression) : T = {
    val result = this.factory()
    result.d1 = this.d1
    result.d2 = this.d2

    val overApproxIds = result.d2.getCollectionOverApproximation(collection)
    val underApproxIds = result.d2.getCollectionUnderApproximation(collection)
    val approxIds = overApproxIds.value ++ underApproxIds.value
    for (approxId <- approxIds) {
      val keyIds = result.d2.getCollectionKeys(approxId)
      if (! keyIds.isBottom) {
        result.d1 = assignSemanticValue(keyIds, value, result.d1)
      }
    }

    result
  }

  def isSummaryCollection(collection: Assignable): Boolean = {
    return this.d2.isSummaryCollection(collection)
  }

  def optimizeSummaryNodes(): (HeapAndAnotherDomain[N,H,I],Replacement) = {
    val (heap,rep) = this.d2.optimizeSummaryNodes
    (new HeapAndAnotherDomain[N,H,I](d1.merge(rep),heap),rep)
  }

   private def getCollectionMustValuesByValue(collection: Assignable, value: Expression): HeapIdSetDomain[I] = {
    var matchedIds = (new MaybeHeapIdSetDomain[I]()).bottom()
    val underApproxIds = this.d2.getCollectionUnderApproximation(collection)
    for (underApproxId <- underApproxIds.value) {
      val valueIds = this.d2.getCollectionValues(underApproxId)
      for (valueId <- valueIds.value) {
        if (this.areEqual(valueId, value).equals(BooleanDomain.domTrue)) {
          matchedIds = matchedIds.add(valueId)
        }
      }
    }

    return matchedIds
  }

  private def getCollectionMayValuesByValue(collection: Assignable, value: Expression): HeapIdSetDomain[I] = {
    var matchedIds = (new MaybeHeapIdSetDomain[I]()).bottom()
    val overApproxIds = this.d2.getCollectionOverApproximation(collection)
    for (overApproxId <- overApproxIds.value) {
      val valueIds = this.d2.getCollectionValues(overApproxId)
      for (valueId <- valueIds.value) {
        if (BooleanDomain.domTrue.lessEqual(this.areEqual(valueId, value))) {
          matchedIds = matchedIds.add(valueId)
        }
      }
    }

    return matchedIds
  }

  private def getCollectionMustValuesWithMayBeValue(collection: Assignable, value: Expression): HeapIdSetDomain[I] ={
    var matchedIds = (new MaybeHeapIdSetDomain[I]()).bottom()
    val underApproxIds = this.d2.getCollectionUnderApproximation(collection)
    for (underApproxId <- underApproxIds.value) {
      val valueIds = this.d2.getCollectionValues(underApproxId)
      for (valueId <- valueIds.value) {
        if (BooleanDomain.domTrue.lessEqual(this.areEqual(valueId, value))) {
          matchedIds = matchedIds.add(valueId)
        }
      }
    }

    matchedIds
  }

  private def getCollectionMustValuesByKey(collection: Assignable, key: Expression): HeapIdSetDomain[I] = {
    var matchedIds = (new MaybeHeapIdSetDomain[I]()).bottom()

    val underApproxIds = this.d2.getCollectionUnderApproximation(collection)
    for (underApproxId <- underApproxIds.value) {
      val tupleIds = this.d2.getCollectionTuples(underApproxId)
      for (tupleId <- tupleIds.value) {
        val keyId = this.d2.getCollectionKeyByTuple(tupleId).asInstanceOf[I]
        val valueId = this.d2.getCollectionValueByTuple(tupleId).asInstanceOf[I]
        if (this.areEqual(keyId, key).equals(BooleanDomain.domTrue)) {
          matchedIds = matchedIds.add(valueId)
        }
      }
    }

    return matchedIds
  }

  private def getCollectionMayValuesByKey(collection: Assignable, key: Expression): HeapIdSetDomain[I] = {
    var matchedIds = (new MaybeHeapIdSetDomain[I]()).bottom()

    val overApproxIds = this.d2.getCollectionOverApproximation(collection)
    for (overApproxId <- overApproxIds.value) {
      val tupleIds = this.d2.getCollectionTuples(overApproxId)
      for (tupleId <- tupleIds.value) {
        val keyId = this.d2.getCollectionKeyByTuple(tupleId).asInstanceOf[I]
        val valueId = this.d2.getCollectionValueByTuple(tupleId).asInstanceOf[I]
        if (BooleanDomain.domTrue.lessEqual(this.areEqual(keyId, key))) {
          matchedIds = matchedIds.add(valueId)
        }
      }
    }

    return matchedIds
  }

  private def getCollectionMustKeysByKey(collection: Assignable, key: Expression): HeapIdSetDomain[I] = {
    var matchedIds = (new MaybeHeapIdSetDomain[I]()).bottom()
    val underApproxIds = this.d2.getCollectionUnderApproximation(collection)
    for (underApproxId <- underApproxIds.value) {
      val keyIds = this.d2.getCollectionKeys(underApproxId)
      for (keyId <- keyIds.value) {
        if (this.areEqual(keyId, key).equals(BooleanDomain.domTrue)) {
          matchedIds = matchedIds.add(keyId)
        }
      }
    }

    matchedIds
  }

  private def getCollectionMayKeysByKey(collection: Assignable, key: Expression): HeapIdSetDomain[I] = {
    var matchedIds = (new MaybeHeapIdSetDomain[I]()).bottom()
    val overApproxIds = this.d2.getCollectionOverApproximation(collection)
    for (overApproxId <- overApproxIds.value) {
      val keyIds = this.d2.getCollectionKeys(overApproxId)
      for (keyId <- keyIds.value) {
        if (BooleanDomain.domTrue.lessEqual(this.areEqual(keyId, key))) {
          matchedIds = matchedIds.add(keyId)
        }
      }
    }

    matchedIds
  }

  private def getCollectionMustKeysWithMayBeKey(collection: Assignable, key: Expression): HeapIdSetDomain[I] ={
    var matchedIds = (new MaybeHeapIdSetDomain[I]()).bottom()
    val underApproxIds = this.d2.getCollectionUnderApproximation(collection)
    for (underApproxId <- underApproxIds.value) {
      val keyIds = this.d2.getCollectionKeys(underApproxId)
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
      val key = this.d2.getCollectionKeyByTuple(tupleId)
      var result = this.removeVariable(key)

      val value = this.d2.getCollectionValueByTuple(tupleId)
      result = result.removeVariable(value)

      result = result.removeVariable(tupleId)
      result.d2 = result.d2.removeCollectionElement(tupleId)

      result
    case _ =>
      throw new SemanticException("This is not a tuple identifier " + tuple.toString)
  }

  private def increaseCollectionLength(collection: Assignable) = {
    def setCollectionLength(initial: T)(lengthId: Assignable) = {
      val result = applyToAssignable[T](lengthId, initial, _.assign(_, BinaryArithmeticExpression(lengthId.asInstanceOf[I], Constant("1", lengthId.getType(), null), ArithmeticOperator.+, lengthId.getType())))

      if (isSummaryCollection(collection)) {
        result.lub(result, initial)
      } else {
        result
      }
    }
    val lengthIds = this.d2.getCollectionLength(collection)
    HeapIdSetFunctionalLifting.applyToSetHeapId(this, lengthIds, setCollectionLength(this))
  }

  private def decreaseCollectionLength(collection: Assignable) = {
    def setCollectionLength(initial: T)(lengthId: Assignable) = {
      var result = applyToAssignable[T](lengthId, initial, _.assign(_, BinaryArithmeticExpression(lengthId.asInstanceOf[I], Constant("1", lengthId.getType(), null), ArithmeticOperator.-, lengthId.getType())))
      val (res, _) = result.assume(BinaryArithmeticExpression(lengthId.asInstanceOf[I], Constant("0", lengthId.getType(), null), ArithmeticOperator.>=, lengthId.getType()))
      result = res

      if(isSummaryCollection(collection)) {
        result.lub(result, initial)
      } else {
        result
      }
    }
    val lengthIds = this.d2.getCollectionLength(collection)
    HeapIdSetFunctionalLifting.applyToSetHeapId(this, lengthIds, setCollectionLength(this))
  }

  private def insertCollectionElementToApprox(collectionApprox: Assignable, key: Expression, value: Expression, pp: ProgramPoint) = {
    var result: T = this.factory()
    val (tupleIds, newHeap, rep) = this.d2.insertCollectionElementToApprox(collectionApprox, pp)
    result.d1 = this.d1.merge(rep)
    result.d2 = newHeap

    for (tupleId <- tupleIds.value) {
      result = assignToCollectionTuple(tupleId, key, value, result)
    }

    (result, rep)
  }

  private def setCollectionLengthFromCollection(initial: T, fromCollection: Assignable)(toCollection: Assignable) = {
    def setCollectionLength(initial: T, toCollection: Assignable)(a: Assignable) = a match{
      case lengthId: I =>
        def assignLength(state: T, lengthId: I)(variable: Assignable) = {
          var result = state.createVariable(variable, lengthId.getType())
          result = applyToAssignable[T](variable, result, _.assign(_, lengthId))

          if(isSummaryCollection(toCollection)) {
            result.lub(result, state)
          } else {
            result
          }
        }

        val result = initial
        val lengthIds = result.d2.getCollectionLength(toCollection)
        HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), lengthIds, assignLength(result, lengthId))
      case _ =>
        throw new SemanticException("This is not a collection length " + a.toString)
    }

    val result = initial

    val lengthIds = result.d2.getCollectionLength(fromCollection)
    HeapIdSetFunctionalLifting.applyToSetHeapId(initial.factory(), lengthIds, setCollectionLength(result, toCollection))
  }

  private def setCollectionLengthToZero(initial:HeapAndAnotherDomain[N,H,I], lengthTyp:Type)(collection: Assignable) : HeapAndAnotherDomain[N,H,I] = {
    var result = initial

    val lengthIds = result.d2.getCollectionLength(collection)
    result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), lengthIds, result.createVariable(_, lengthTyp))

    def setToZero(initialState:N)(a:Assignable) = applyToAssignable[N](a, initialState, _.assign(_, Constant("0", a.getType(), null)))
    result.d1 = HeapIdSetFunctionalLifting.applyToSetHeapId(result.d1.factory(), lengthIds, setToZero(result.d1))

    if (result.isSummaryCollection(collection)) {
      result.lub(result, initial)
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
    val keyId = result.d2.getCollectionKeyByTuple(tupleId)
    val valueId = result.d2.getCollectionValueByTuple(tupleId)

    if (!result.getIds().contains(keyId.asInstanceOf[Identifier]))
      result = applyToAssignable[T](keyId, result, _.createVariable(_, keyId.getType()))
    result = applyToAssignable[T](keyId, result, _.assign(_, key))

    if (!result.getIds().contains(valueId.asInstanceOf[Identifier]))
      result = applyToAssignable[T](valueId, result, _.createVariable(_, valueId.getType()))
    result = applyToAssignable[T](valueId, result, _.assign(_, value))

    result
  }

  def setArgument(variable : Assignable, expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.setArgument(variable, expr)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r)
    result.d1=applyToAssignable[N](variable, result.d1, _.setArgument(_, expr));
    SystemParameters.domainTimer.stop();
    result
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
      val result : T = this.factory();
      SystemParameters.heapTimer.start();
      val (d, r) =d2.assume(expr)
      result.d2=d;
      SystemParameters.heapTimer.stop();
      SystemParameters.domainTimer.start();
      result.d1=d1.merge(r).assume(expr)
      SystemParameters.domainTimer.stop();
      (result, r)
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

    return BooleanDomain.domTop
  }

  def createVariable(variable : Assignable, typ : Type) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.createVariable(variable, typ)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=applyToAssignable[N](variable, this.d1, _.createVariable(_, typ));
    SystemParameters.domainTimer.stop();
    result
  }
  def removeVariable(variable : Assignable) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.removeVariable(variable)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=applyToAssignable[N](variable, this.d1, _.removeVariable(_));
    SystemParameters.domainTimer.stop();
    result
  }
  def access(field : Assignable) : T= {
    val result : T = this.factory();
    SystemParameters.domainTimer.start();
    result.d1=applyToAssignable[N](field, this.d1, _.access(_));
    SystemParameters.domainTimer.stop();
    SystemParameters.heapTimer.start();
    result.d2=d2//Access does not change the state of the heap domain
    SystemParameters.heapTimer.stop();
    result
  }
  def backwardAccess(field : Assignable) : T= {
    val result : T = this.factory();
    SystemParameters.domainTimer.start();
    result.d1=applyToAssignable[N](field, this.d1, _.backwardAccess(_));
    SystemParameters.domainTimer.stop();
    SystemParameters.heapTimer.start();
    result.d2 =d2 //Backward access does not change the state of the heap domain
    SystemParameters.heapTimer.stop();
    result
  }
  def backwardAssign(variable : Assignable, expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.backwardAssign(variable, expr)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r);
    result.d1=applyToAssignable[N](variable, result.d1, _.backwardAssign(_, expr));
    SystemParameters.domainTimer.stop();
    result
  }

  override def top() : T = {
    val result : T = this.factory();
    SystemParameters.domainTimer.start();
    result.d1=d1.top()
    SystemParameters.domainTimer.stop();
    SystemParameters.heapTimer.start();
    result.d2=d2.top()
    SystemParameters.heapTimer.stop();
    result
  }

  override def bottom() : T = {
    val result : T = this.factory();
    SystemParameters.domainTimer.start()
    result.d1=d1.bottom()
    SystemParameters.domainTimer.stop()
    SystemParameters.heapTimer.start()
    result.d2=d2.bottom()
    SystemParameters.heapTimer.stop()
    result
  }

  override def lubWithReplacement(l : T, r : T) : (T,Replacement) = {
    val result : T = this.factory()

    SystemParameters.domainTimer.start()
    val s = d1.lub(l.d1, r.d1)
    SystemParameters.domainTimer.stop()

    SystemParameters.heapTimer.start()
    val (d, rep) =d2.lubWithReplacement(l.d2, r.d2, s)
    result.d2=d
    SystemParameters.heapTimer.stop()

    SystemParameters.domainTimer.start()
    result.d1 = s.merge(rep)
    SystemParameters.domainTimer.stop()
    (result,rep)
  }

  override def lub(l : T, r : T) : T = lubWithReplacement(l,r)._1

  override def glbWithReplacement(l : T, r : T) : (T,Replacement) = {
    val result : T = this.factory()
    SystemParameters.heapTimer.start()
    val (d, rep) =d2.glbWithReplacement(l.d2, r.d2)
    result.d2=d
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val s = d1.glb(l.d1, r.d1)
    result.d1= s.merge(rep)
    SystemParameters.domainTimer.stop()
    (result,rep)
  }

  override def glb(l : T, r : T) : T = glbWithReplacement(l,r)._1

  override def wideningWithReplacement(l : T, r : T) : (T,Replacement) = {
    val result : T = this.factory()
    SystemParameters.heapTimer.start()
    val (d, rep) =d2.wideningWithReplacement(l.d2, r.d2)
    result.d2=d
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val s = d1.widening(l.d1, r.d1)
    result.d1= s.merge(rep)
    SystemParameters.domainTimer.stop()
    (result,rep)
  }

  override def widening(l : T, r : T) : T = wideningWithReplacement(l,r)._1

  override def lessEqual(r : T) : Boolean = {
    if(this.d1.lessEqual(this.d1.bottom())) return true;
    if(r.d1.lessEqual(r.d1.bottom())) return false;
    SystemParameters.heapTimer.start();
    var b = d2.lessEqual(r.d2)
    SystemParameters.heapTimer.stop();
    if(! b) return false;
    SystemParameters.domainTimer.start();
    b = d1.lessEqual(r.d1)
    SystemParameters.domainTimer.stop();
    return b;
  }

  private def applyToAssignable[L <: Lattice[L]](variable : Assignable, state : L, functor : (L, Identifier) => L) : L = {
    variable match {
      case x : VariableIdentifier =>
        return functor(state, x)
      case x : I =>
        var result : L = functor(state, x)
        return result;
    }
  }


}