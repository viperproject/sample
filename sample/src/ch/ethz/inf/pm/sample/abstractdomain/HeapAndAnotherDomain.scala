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
class HeapAndAnotherDomain[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](private var d1 : N, private var d2 : H) extends Lattice[HeapAndAnotherDomain[N, H, I]]{


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
    val result : T = this.factory();
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

  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, pp: ProgramPoint) : (T, HeapIdSetDomain[I]) = {
    val result = this
    val (collectionIds, newHeap) = result.d2.createEmptyCollection(collTyp, keyTyp, valueTyp, lengthTyp, pp)
    result.d2 = newHeap

    (HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), collectionIds, setCollectionLengthToZero(result)), collectionIds)
  }

  def createTopCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, pp: ProgramPoint) : (T, HeapIdSetDomain[I]) = {

    def setLengthToTop(initialSemanticDomain:N, initialHeap:H)(collectionId:Assignable) = {
      val resultF = this.factory()
      resultF.d1 = initialSemanticDomain
      resultF.d2 = initialHeap

      val (lengthIds, newHeap) = resultF.d2.getCollectionLength(collectionId)
      resultF.d2 = newHeap

      def setToTop(initialState:N)(a:Assignable) = applyToAssignable[N](a, initialState, _.setToTop(_))
      resultF.d1 = HeapIdSetFunctionalLifting.applyToSetHeapId(resultF.d1.factory(), lengthIds, setToTop(resultF.d1))

      resultF
    }

    val (collectionIds, newHeap) = this.d2.createEmptyCollection(collTyp, keyTyp, valueTyp, lengthTyp, pp)
    val result = HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), collectionIds, setLengthToTop(this.d1, newHeap))

    def insertElement(initial:T)(collection:Assignable) = {
      def f(initialState:N)(a: Assignable) = {
        applyToAssignable[N](a, initialState, _.setToTop(_))
      }

      val resultF = initial
      val (keyIds, valueIds, newHeap) = resultF.d2.insertCollectionElement(collection, pp)
      resultF.d2 = newHeap
      resultF.d1 = HeapIdSetFunctionalLifting.applyToSetHeapId(resultF.d1.factory(), keyIds, f(resultF.d1))
      resultF.d1 = HeapIdSetFunctionalLifting.applyToSetHeapId(resultF.d1.factory(), valueIds, f(resultF.d1))

      resultF
    }

    (HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), collectionIds, insertElement(result)), collectionIds)
  }

  def insertCollectionElement(collection: Assignable, key: Expression, value: Expression, pp: ProgramPoint) = {
    val result: T = this
    def f(initialState:N, expr: Expression)(a: Assignable) = {
      initialState.lub(initialState, applyToAssignable[N](a, initialState, _.assign(_, expr)))
    }
    val (keyIds, valueIds, newHeap) = result.d2.insertCollectionElement(collection, pp)
    result.d2 = newHeap
    result.d1 = HeapIdSetFunctionalLifting.applyToSetHeapId(result.d1.factory(), keyIds, f(result.d1, key))
    result.d1 = HeapIdSetFunctionalLifting.applyToSetHeapId(result.d1.factory(), valueIds, f(result.d1, value))

    result
  }

  def removeCollectionElementByKey(collection: Assignable, key: Expression, valueTyp: Type): T = {
    def removeKey(initialState: T, key: Expression, valueTyp: Type, id: I) = {
      val idNotEqualsKey = initialState.d1.assume(NegatedBooleanExpression(BinaryArithmeticExpression(id, key, ArithmeticOperator.==, null)))
      if (idNotEqualsKey.lessEqual(idNotEqualsKey.bottom())){
        initialState.removeCollectionElementByTuple(initialState.d2.getCollectionTupleByKey(id), key.getType(), valueTyp)
      } else {
        new HeapAndAnotherDomain[N, H, I](idNotEqualsKey, initialState.d2)
      }
    }

    var result = this

    val (keyIds, newHeap) = result.d2.getCollectionKey(collection, key, result.d1)
    result.d2 = newHeap

    for (keyId <- keyIds.value) {
      result = removeKey(result, key, valueTyp, keyId)
    }

    result
  }

  def removeCollectionElementByValue(collection: Assignable, value: Expression, keyTyp: Type): T = {
    def removeValue(initialState: T, value: Expression, keyTyp: Type, id:I) = {
      val idNotEqualsValue = initialState.d1.assume(NegatedBooleanExpression(BinaryArithmeticExpression(id, value, ArithmeticOperator.==, null)))
      if (idNotEqualsValue.lessEqual(idNotEqualsValue.bottom())){
        initialState.removeCollectionElementByTuple(initialState.d2.getCollectionTupleByValue(id), keyTyp, value.getType())
      } else {
        new HeapAndAnotherDomain[N, H, I](idNotEqualsValue, initialState.d2)
      }
    }

    var result = this
    val (valueIds, newHeap) = result.d2.getCollectionValueByValue(collection, value, result.d1)
    result.d2 = newHeap
    for (valueId <- valueIds.value) {
      result = removeValue(result, value, keyTyp, valueId)
    }

    result
  }

  def getCollectionKeyByKey(collection: Assignable, key: Expression): (T, HeapIdSetDomain[I]) = {
    val result = this
    val (ids, newHeap) = result.d2.getCollectionKey(collection, key, result.d1)
    result.d2 = newHeap
    (result, ids)
  }

  def getCollectionValueByKey(collection: Assignable, key: Expression): (T, HeapIdSetDomain[I]) = {
    val result: T = this.factory()
    val (ids, newHeap) = this.d2.getCollectionValueByKey(collection, key, this.d1)
    result.d2 = newHeap
    (result, ids)
  }

  def getCollectionValueByValue(collection: Assignable, value: Expression): (T, HeapIdSetDomain[I]) = {
    val result: T = this
    val (ids, newHeap) = result.d2.getCollectionValueByValue(collection, value, result.d1)
    result.d2 = newHeap
    (result, ids)
  }

  def clearCollection(collection:Assignable) : T = {
    def clearSingleCollection(initial:T)(collectionId: Assignable) = {
      val result: T = initial

      def removeVariablesSemantic(initialState:N, ids:HeapIdSetDomain[I]): N = {
        if(ids.isBottom) return initialState

        var result = initialState
        for(variableId <- ids.value){
          result = result.removeVariable(variableId)
        }

        result
      }

      val keys = result.d2.getCollectionKeys(collectionId)
      result.d1 = removeVariablesSemantic(result.d1, keys)

      val values = result.d2.getCollectionValues(collectionId)
      result.d1 = removeVariablesSemantic(result.d1, values)

      val tuples = result.d2.getCollectionTuples(collectionId)
      result.d1 = removeVariablesSemantic(result.d1, tuples)

      result.d2 = result.d2.clearCollection(collectionId)

      setCollectionLengthToZero(result)(collectionId)
    }

    collection match {
      case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, clearSingleCollection(this))
      case id: Assignable => clearSingleCollection(this)(id)
    }
  }

  def extractCollectionKeys(fromCollection: Assignable, collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint): (T, HeapIdSetDomain[I]) = {
    def insertKeyAsValue(keyId:I, result: T)(toCollection: Assignable) = {
      def assignValue(initialState:N, value: Expression)(a: Assignable) = {
        applyToAssignable[N](a, initialState, _.assign(_, value))
      }

      def assignTop(initialState:N)(a: Assignable) = {
        applyToAssignable[N](a, initialState, _.setToTop(_))
      }

      val (keyIdsNew, valueIdsNew, newHeap) = result.d2.insertCollectionElement(toCollection, keyId.getProgramPoint())
      result.d2 = newHeap

      result.d1 = HeapIdSetFunctionalLifting.applyToSetHeapId(result.d1.factory(), keyIdsNew, assignTop(result.d1))
      result.d1 = HeapIdSetFunctionalLifting.applyToSetHeapId(result.d1.factory(), valueIdsNew, assignValue(result.d1, keyId))

      result
    }

    var result = this
    val (toCollectionIds, newHeap) = result.d2.createEmptyCollection(collTyp, keyTyp, valueTyp, lengthTyp, pp)
    result.d2 = newHeap

    val keyIds = this.d2.getCollectionKeys(fromCollection)
    for (keyId <- keyIds.value) {
      result = result.lub(result, HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionIds, insertKeyAsValue(keyId, result)))
    }

    result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), toCollectionIds, setCollectionLengthFromCollection(result, fromCollection))

    (result, toCollectionIds)
  }

  def copyCollection(fromCollection: Assignable, toCollection: Assignable, keyTyp:Type, valueTyp:Type) : T = {
    var result = this
    val tuples = result.d2.getCollectionTuples(fromCollection)

    for (tuple <- tuples.value) {
      val key = result.d2.getCollectionKeyByTuple(tuple, keyTyp).asInstanceOf[I]
      val value = result.d2.getCollectionValueByTuple(tuple, valueTyp).asInstanceOf[I]
      result = result.insertCollectionElement(toCollection, key, value, tuple.getProgramPoint())
    }
    result = setCollectionLengthFromCollection(result, fromCollection)(toCollection)

    result
  }

  def assignAllCollectionKeys(collection: Assignable, value: Expression) : T = {
    val result = this
    val (keyIds) = result.d2.getCollectionKeys(collection)
    result.d1 = assignSemanticValue(keyIds, value, result.d1)
    result
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
    def setCollectionLength(result: T, toCollection: Assignable)(a: Assignable) = a match{
      case lengthId: I =>
        def f(state: N)(variable: Assignable) = applyToAssignable[N](variable, state, _.assign(_, lengthId))

        val (lengthIds, newHeap) = result.d2.getCollectionLength(toCollection)
        result.d2 = newHeap
        result.d1 = HeapIdSetFunctionalLifting.applyToSetHeapId(result.d1.factory(), lengthIds, f(result.d1))

        result
      case _ =>
        throw new SemanticException("This is not a collection length " + a.toString)
    }

    val result = initial

    val (lengthIds, newHeap) = result.d2.getCollectionLength(fromCollection)
    result.d2 = newHeap
    HeapIdSetFunctionalLifting.applyToSetHeapId(initial.factory(), lengthIds, setCollectionLength(result, toCollection))
  }

  private def setCollectionLengthToZero(initial:T)(collection: Assignable) : T = {
    val result = initial

    val (lengthIds, newHeap) = result.d2.getCollectionLength(collection)
    result.d2 = newHeap

    def setToZero(initialState:N)(a:Assignable) = applyToAssignable[N](a, initialState, _.assign(_, Constant("0", a.getType(), null)))
    result.d1 = HeapIdSetFunctionalLifting.applyToSetHeapId(result.d1.factory(), lengthIds, setToZero(result.d1))

    result
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
    SystemParameters.domainTimer.start();
    result.d1=d1.bottom()
    SystemParameters.domainTimer.stop();
    SystemParameters.heapTimer.start();
    result.d2=d2.bottom()
    SystemParameters.heapTimer.stop();
    result
  }

  override def lub(l : T, r : T) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, rep) =d2.lubWithReplacement(l.d2, r.d2, l.d1, r.d1)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    val s = d1.lub(l.d1, r.d1)
    result.d1 = s.merge(rep)
    SystemParameters.domainTimer.stop();
    result
  }

  override def glb(l : T, r : T) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, rep) =d2.glbWithReplacement(l.d2, r.d2, l.d1, r.d1)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    val s = d1.glb(l.d1, r.d1)
   result.d1= s.merge(rep)
    SystemParameters.domainTimer.stop();
    result
  }

  override def widening(l : T, r : T) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, rep) =d2.wideningWithReplacement(l.d2, r.d2, l.d1, r.d1)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    val s = d1.widening(l.d1, r.d1)
    result.d1= s.merge(rep)
    SystemParameters.domainTimer.stop();
    result
  }

  override def lessEqual(r : T) : Boolean = {
    if(this.d1.lessEqual(this.d1.bottom()) || this.d2.lessEqualWithReplacement(this.d2.bottom(), this.d1, this.d1.bottom())._1) return true;
    if(r.d1.lessEqual(r.d1.bottom()) || r.d2.lessEqualWithReplacement(r.d2.bottom(), r.d1, r.d1.bottom())._1) return false;
    SystemParameters.heapTimer.start();
    var (b, rep) = d2.lessEqualWithReplacement(r.d2, this.d1, r.d1)
    SystemParameters.heapTimer.stop();
    if(! b) return false;
    SystemParameters.domainTimer.start();
    b = d1.merge(rep).lessEqual(r.d1.merge(rep));
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