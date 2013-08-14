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

  def assign(variable : Assignable, expr : Expression) : T= {
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

  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, pp: ProgramPoint) : (HeapIdSetDomain[I], HeapAndAnotherDomain[N,H,I], Replacement) = {
    var result = this.factory()
    result.d1 = this.d1
    val (collectionIds, newHeap, rep) = this.d2.createEmptyCollection(collTyp, keyTyp, valueTyp, lengthTyp, pp)
    result.d2 = newHeap

    def setCollectionLength(initial: HeapAndAnotherDomain[N,H,I], lengthTyp: Type)(collection:Assignable) = {
      var result = initial
      val collectionLength = result.d2.getCollectionLength(collection)

      def createVariable(state: HeapAndAnotherDomain[N,H,I], lengthTyp:Type)(variable: Assignable) = {
        applyToAssignable[T](variable, state, _.createVariable(_, lengthTyp))
      }

      result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), collectionLength, createVariable(result, lengthTyp))
      result = setCollectionLengthToZero(result)(collection)
      result
    }

    result = HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), collectionIds, setCollectionLength(result, lengthTyp))

    (collectionIds, result, rep)
  }

  def getCollectionValue(valueId: Assignable) = valueId match {
    case id:I => (this, this.d2.get(id))
    case _ => throw new SemanticException("This is not a collection tuple value identifier")
  }

  def insertCollectionElement(collection: Assignable, key: Expression, value: Expression, pp: ProgramPoint) = {
    def assignToElement(initialState:T, key: Expression, value: Expression)(tupleId: Assignable) = {
      var result = initialState
      val keyId = result.d2.getCollectionKeyByTuple(tupleId, key.getType())
      val valueId = result.d2.getCollectionValueByTuple(tupleId, value.getType())
      result = applyToAssignable[T](keyId, result, _.createVariable(_, keyId.getType()))
      result = applyToAssignable[T](keyId, result, _.assign(_, key))
      result = applyToAssignable[T](valueId, result, _.createVariable(_, valueId.getType()))
      result = applyToAssignable[T](valueId, result, _.assign(_, value))
      result.lub(initialState, result)
    }

    var result: T = this.factory()
    result.d1 = this.d1
    result.d2 = this.d2

    val overApproxIds = this.d2.getCollectionOverApproximation(collection)
    for (overApproxId <- overApproxIds.value) {
      val (tupleIds, newHeap) = result.d2.insertCollectionElement(overApproxId, pp)
      result.d2 = newHeap
      result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), tupleIds, assignToElement(result, key, value))
    }

    result
  }

  def removeCollectionElementByKey(collection: Assignable, key: Expression, valueTyp: Type): T = {
    //if (this.d2.isSummaryCollection(collection)) return this

    def removeKey(initialState: T, key: Expression, valueTyp: Type, id: I) = {
      val idNotEqualsKey = initialState.assume(NegatedBooleanExpression(BinaryArithmeticExpression(id, key, ArithmeticOperator.==, null)))
      if (idNotEqualsKey.lessEqual(idNotEqualsKey.bottom())){
        initialState.removeCollectionElementByTuple(initialState.d2.getCollectionTupleByKey(id), key.getType(), valueTyp)
      } else {
        idNotEqualsKey
      }
    }

    var result = this.factory()
    result.d1 = this.d1
    result.d2 = this.d2
    val keyIds = result.getCollectionKeysByKey(collection, key)

    for (keyId <- keyIds.value) {
      result = removeKey(result, key, valueTyp, keyId)
    }

    result
  }

  def removeCollectionElementByValue(collection: Assignable, value: Expression, keyTyp: Type): T = {
    //if (this.d2.isSummaryCollection(collection)) return this

    def removeValue(initialState: T, value: Expression, keyTyp: Type, id:I) = {
      val idNotEqualsValue = initialState.assume(NegatedBooleanExpression(BinaryArithmeticExpression(id, value, ArithmeticOperator.==, null)))
      if (idNotEqualsValue.lessEqual(idNotEqualsValue.bottom())){
        initialState.removeCollectionElementByTuple(initialState.d2.getCollectionTupleByValue(id), keyTyp, value.getType())
      } else {
        idNotEqualsValue
      }
    }

    var result = this.factory()
    result.d1 = this.d1
    result.d2 = this.d2
    val valueIds = result.getCollectionValuesByValue(collection, value)

    for (valueId <- valueIds.value) {
      result = removeValue(result, value, keyTyp, valueId)
    }

    result
  }

  def getCollectionKeyByKey(collection: Assignable, key: Expression): (T, HeapIdSetDomain[I]) = {
    (this, this.getCollectionKeysByKey(collection, key))
  }

  def getCollectionValueByKey(collection: Assignable, key: Expression, valueTyp: Type): (T, HeapIdSetDomain[I]) = {
    (this, this.getCollectionValuesByKey(collection, key, valueTyp))
  }

  def getCollectionValueByValue(collection: Assignable, value: Expression): (T, HeapIdSetDomain[I]) = {
    (this, this.getCollectionValuesByValue(collection, value))
  }

  def clearCollection(collection:Assignable, keyTyp: Type, valueTyp: Type) : T = {
    if (this.d2.isSummaryCollection(collection)) return this

    def clearSingleCollection(initial:T, keyTyp: Type, valueTyp: Type)(collectionId: Assignable) = {
      var result: T = initial

      val overApproxIds = result.d2.getCollectionOverApproximation(collectionId)
      for (overApproxId <- overApproxIds.value) {
        val tupleIds = result.d2.getCollectionTuples(overApproxId)
        for (tupleId <- tupleIds.value) {
          val keyId = result.d2.getCollectionKeyByTuple(tupleId, keyTyp)
          result = result.removeVariable(keyId)

          val valueId = result.d2.getCollectionValueByTuple(tupleId, valueTyp)
          result = result.removeVariable(valueId)

          result = result.removeVariable(tupleId)
        }
        result = result.removeVariable(overApproxId)
      }

      result
    }

    // TODO: Is this distinction really needed?
    collection match {
      case id: Assignable => clearSingleCollection(this, keyTyp, valueTyp)(id)
      case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, clearSingleCollection(this, keyTyp, valueTyp))
    }
  }

  def extractCollectionKeys(fromCollection: Assignable, newKeyValue: Expression, collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint): (T, HeapIdSetDomain[I]) = {
    def insertKeyAsValue(keyId:I, key: Expression, result: T)(toCollection: Assignable) = {
      result.insertCollectionElement(toCollection, key, keyId, keyId.getProgramPoint())
    }

    var result = this.factory()
    val (toCollectionIds, newHeap, rep) = this.d2.createEmptyCollection(collTyp, keyTyp, valueTyp, lengthTyp, pp)
    result.d1 = this.d1.merge(rep)
    result.d2 = newHeap

    val overApproxIds = this.d2.getCollectionOverApproximation(fromCollection)
    for (overApproxId <- overApproxIds.value) {
      val keyIds = this.d2.getCollectionKeys(overApproxId, valueTyp)
      for (keyId <- keyIds.value) {
        result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionIds, insertKeyAsValue(keyId, newKeyValue, result))
      }
    }

    result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionIds, setCollectionLengthFromCollection(result, fromCollection))

    (result, toCollectionIds)
  }

  def copyCollection(fromCollection: Assignable, toCollection: Assignable, keyTyp:Type, valueTyp:Type) : T = {
    var result = this.factory()
    result.d1 = this.d1
    result.d2 = this.d2

    val overApproxIds = this.d2.getCollectionOverApproximation(fromCollection)
    for (overApproxId <- overApproxIds.value) {
      val tuples = this.d2.getCollectionTuples(overApproxId)

      for (tuple <- tuples.value) {
        val key = this.d2.getCollectionKeyByTuple(tuple, keyTyp).asInstanceOf[I]
        val value = this.d2.getCollectionValueByTuple(tuple, valueTyp).asInstanceOf[I]
        val inserted = this.insertCollectionElement(toCollection, key, value, tuple.getProgramPoint())
        result = result.lub(result, inserted)
      }
    }

    result
  }

  def assignAllCollectionKeys(collection: Assignable, value: Expression, keyTyp: Type) : T = {
    val result = this.factory()
    result.d1 = this.d1
    result.d2 = this.d2

    val overApproxIds = result.d2.getCollectionOverApproximation(collection)
    for (overApproxId <- overApproxIds.value) {
      val keyIds = result.d2.getCollectionKeys(overApproxId, keyTyp)
      result.d1 = assignSemanticValue(keyIds, value, result.d1)
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

  private def getCollectionValuesByValue(collection: Assignable, value: Expression): HeapIdSetDomain[I] = {
    var matchedIds = (new MaybeHeapIdSetDomain[I]()).bottom()
    val overApproxIds = this.d2.getCollectionOverApproximation(collection)
    for (overApproxId <- overApproxIds.value) {
      val valueIds = this.d2.getCollectionValues(overApproxId, value.getType())
      for (valueId <- valueIds.value) {
        val assumed = this.assume(BinaryArithmeticExpression(valueId, value, ArithmeticOperator.==, null))
        if (!assumed.lessEqual(assumed.bottom())) {
          matchedIds = matchedIds.add(valueId)
        }
      }
    }

    return matchedIds
  }

  private def getCollectionValuesByKey(collection: Assignable, key: Expression, valueTyp: Type): HeapIdSetDomain[I] = {
    var matchedIds = (new MaybeHeapIdSetDomain[I]()).bottom()

    val overApproxIds = this.d2.getCollectionOverApproximation(collection)
    for (overApproxId <- overApproxIds.value) {
      val tupleIds = this.d2.getCollectionTuples(overApproxId)
      for (tupleId <- tupleIds.value) {
        val keyId = this.d2.getCollectionKeyByTuple(tupleId, key.getType()).asInstanceOf[I]
        val valueId = this.d2.getCollectionValueByTuple(tupleId, valueTyp).asInstanceOf[I]
        val assumed = this.assume(BinaryArithmeticExpression(keyId, key, ArithmeticOperator.==, null))
        if (!assumed.lessEqual(assumed.bottom())) {
          matchedIds = matchedIds.add(valueId)
        }
      }
    }

    return matchedIds
  }

  private def getCollectionKeysByKey(collection: Assignable, key: Expression): HeapIdSetDomain[I] = {
    var matchedIds = (new MaybeHeapIdSetDomain[I]()).bottom()
    val overApproxIds = this.d2.getCollectionOverApproximation(collection)
    for (overApproxId <- overApproxIds.value) {
      val keyIds = this.d2.getCollectionKeys(overApproxId, key.getType())
      for (keyId <- keyIds.value) {
        val assumed = this.assume(BinaryArithmeticExpression(keyId, key, ArithmeticOperator.==, null))
        if (!assumed.lessEqual(assumed.bottom())) {
          matchedIds = matchedIds.add(keyId)
        }
      }
    }

    return matchedIds
  }

  private def removeCollectionElementByTuple(tuple: Assignable, keyTyp: Type, valueTyp: Type) = tuple match {
    case tupleId: I =>
      val result = this

      val key = result.d2.getCollectionKeyByTuple(tupleId, keyTyp)
      key match {
        case keyId: I => result.d1 = result.d1.removeVariable(keyId)
        case _ => throw new SemanticException("This is not an Identifier " + key)
      }

      val value = result.d2.getCollectionValueByTuple(tupleId, valueTyp)
      value match {
        case valueId: I => result.d1 = result.d1.removeVariable(valueId)
        case _ => throw new SemanticException("This is not an Identifier " + key)
      }

      result.d1 = result.d1.removeVariable(tupleId)

      result.d2 = result.d2.removeCollectionElement(tupleId, keyTyp, valueTyp)

      result
    case _ =>
      throw new SemanticException("This is not a tuple identifier " + tuple.toString)
  }

  private def setCollectionLengthFromCollection(initial: T, fromCollection: Assignable)(toCollection: Assignable) = {
    def setCollectionLength(initial: T, toCollection: Assignable)(a: Assignable) = a match{
      case lengthId: I =>
        def assignLength(state: T, lengthId: I)(variable: Assignable) = {
          val result = state.createVariable(variable, lengthId.getType())
          applyToAssignable[T](variable, result, _.assign(_, lengthId))
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

  private def setCollectionLengthToZero(initial:HeapAndAnotherDomain[N,H,I])(collection: Assignable) : HeapAndAnotherDomain[N,H,I] = {
    val result = initial

    val lengthIds = result.d2.getCollectionLength(collection)

    def setToZero(initialState:N)(a:Assignable) = applyToAssignable[N](a, initialState, _.assign(_, Constant("0", a.getType(), null)))
    result.d1 = HeapIdSetFunctionalLifting.applyToSetHeapId(result.d1.factory(), lengthIds, setToZero(result.d1))

    result
//    if (result.d2.isSummaryCollection(collection)) {
//      result.lub(result, initial)
//    } else {
//      result
//    }
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

  def getSummaryCollectionIfExists(collection: Assignable) = {
    val ids = this.d2.getSummaryCollectionIfExists(collection)
    (this, ids)
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
  def assume(expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.assume(expr)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r).assume(expr)
    SystemParameters.domainTimer.stop();
    result
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
    SystemParameters.heapTimer.start()
    val (d, rep) =d2.lubWithReplacement(l.d2, r.d2)
    result.d2=d
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    val s = d1.lub(l.d1, r.d1)
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