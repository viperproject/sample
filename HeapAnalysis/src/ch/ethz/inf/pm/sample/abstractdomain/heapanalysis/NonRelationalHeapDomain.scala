package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample._
import abstractdomain._
import abstractdomain.BinaryArithmeticExpression
import abstractdomain.Constant
import abstractdomain.VariableIdentifier
import heapanalysis.CollectionIdentifier
import heapanalysis.TopHeapIdentifier
import oorepresentation._
import userinterfaces.ShowGraph
import property.Property
import util.HeapIdSetFunctionalLifting
import scala.{collection, Some}
import scala.collection
import javax.swing.plaf.OptionPaneUI

object NonRelationalHeapDomainSettings {
	var unsoundEntryState : Boolean = true;
	var maxInitialNodes : Int = 5;
}

class InverseHeapEnv[I <: NonRelationalHeapIdentifier[I]](var typ:Type, val dom:InverseHeapIdSetDomain[I]) extends FunctionalDomain[I, InverseHeapIdSetDomain[I], InverseHeapEnv[I]] {
  override def factory(): InverseHeapEnv[I] = new InverseHeapEnv[I](typ, dom)
  def get(key: I): InverseHeapIdSetDomain[I] = this.value.get(key) match {
    case None => dom.top() //TODO: correct?
    case Some(x) => x
  }
}

class HeapEnv[I <: NonRelationalHeapIdentifier[I]](var typ : Type, val dom : HeapIdSetDomain[I]) extends FunctionalDomain[I, HeapIdSetDomain[I], HeapEnv[I]] {
  def getIds = this.getAddresses;
  override def factory() = new HeapEnv(typ, dom)
  private def getAddresses : Set[I] = {
    var result : Set[I] = Set.empty[I] ++ value.keySet;
    val it : Iterator[HeapIdSetDomain[I]] = value.values.iterator;
    for(v <- it) {
      result++=v.value;
    }
    return result;
  }

  override def lub(l : HeapEnv[I], r : HeapEnv[I]) = super.lub(l, r);

  def get(key : I) : HeapIdSetDomain[I] = this.value.get(key) match {
    case None => dom.bottom(); //TODO: This is not sound!!!
    case Some(x) => x
  }
} 

class VariableEnv[I <: NonRelationalHeapIdentifier[I]](var typ : Type, val dom : HeapIdSetDomain[I]) extends FunctionalDomain[VariableIdentifier, HeapIdSetDomain[I], VariableEnv[I]] {
  def getIds : Set[Identifier] = (this.getVariables++this.getAddresses).asInstanceOf[Set[Identifier]];
  override def factory() = new VariableEnv(typ, dom)
  private def getVariables=value.keySet;
  private def getAddresses : Set[I]={
    var result : Set[I] = Set.empty[I];
    val it : Iterator[HeapIdSetDomain[I]] = value.values.iterator;
    for(v <- it) {
      result++=v.value;
    }
    return result;
  }
  def get(key : VariableIdentifier) : HeapIdSetDomain[I] = this.value.get(key) match {
    case None => dom.bottom(); //TODO: This is not sound!!!
    case Some(x) => x
  }
  
}
/*
final class MaybeHeapIdSetDomain[I <: NonRelationalHeapIdentifier[I]](id : I) extends NonRelationalHeapIdentifier[MaybeHeapIdSetDomain[I]](id.getType, id.getProgramPoint) with SetDomain[I, MaybeHeapIdSetDomain[I]] {
  def getField() : Option[String] = if(value.size==1) return value.head.getField() else return None;
  override def getLabel() = id.getLabel;

  override def equals(x : Any) : Boolean = x match {
	  case x : I => if(value.size==1) return x.equals(value.head); else return false;
	  case _ => return super.equals(x);
  }
  
  def getNullNode(p : ProgramPoint) : MaybeHeapIdSetDomain[I] = new MaybeHeapIdSetDomain(id.getNullNode(p));
  
  def convert(add : I) : MaybeHeapIdSetDomain[I] = new MaybeHeapIdSetDomain(add).add(add);
  override def getType() : Type = {
    var res=typ.bottom();
    for(a <- this.value)
      res=res.lub(res, a.getType());
    return res;
  }
  
  def isNormalized() : Boolean = {
    for(add <- this.value)
      if(! add.isNormalized) return false;
    return true;
  }
  
  def add(id : MaybeHeapIdSetDomain[I]) : MaybeHeapIdSetDomain[I]= {
    var initial=this;
    for(add <- id.value)
      initial=initial.add(add);
    return initial;
  }
  def createAddressForParameter(typ : Type, pp : ProgramPoint) : MaybeHeapIdSetDomain[I] = this.factory().add(id.createAddressForParameter(typ, pp))
  def createAddress(typ : Type, pp : ProgramPoint) : MaybeHeapIdSetDomain[I] = this.factory().add(id.createAddress(typ, pp))
  def extractField(obj : MaybeHeapIdSetDomain[I], field : String, typ : Type) : MaybeHeapIdSetDomain[I] = {
      var initial=this.factory();
      for(add <- obj.value) {
        initial=initial.add(id.extractField(add, field, typ));
      }
      return initial;
    }
  def accessStaticObject(typ : Type, pp : ProgramPoint) : MaybeHeapIdSetDomain[I] = this.factory().add(id.accessStaticObject(typ, pp));
  
  def factory() : MaybeHeapIdSetDomain[I]=new MaybeHeapIdSetDomain[I](id);
  
  def representSingleVariable() : Boolean = {
    if(this.value.size==1)
      return this.value.head.representSingleVariable();
    else return false;
  }
  def getName() : String = this.toString();
} 
*/

abstract class NonRelationalHeapIdentifier[I <: NonRelationalHeapIdentifier[I]](typ1 : Type, pp : ProgramPoint) extends HeapIdentifier[I](typ1, pp) {
  def getLabel() : String;
  def createAddress(typ : Type, pp : ProgramPoint) : I;
  def createAddressForArgument(typ : Type, p : ProgramPoint) : I;
  def extractField(obj : I, field : String, typ : Type) : I;
  def getArrayCell(array : Assignable, index : Expression) : I;
  def getArrayLength(array : Assignable) : I;
  def createArray(length : Expression, typ : Type, p : ProgramPoint) : I = this.createAddress(typ, p);
  def accessStaticObject(typ : Type, p : ProgramPoint) : I;
  def getNullNode(p : ProgramPoint) : I;
  def isNormalized() : Boolean;
  def factory() : I;

  def createCollection(collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint): I
  def getCollectionOverApproximation(collection:Assignable): I
  def getCollectionUnderApproximation(collection:Assignable): I
  def createCollectionTuple(collectionApprox:Assignable, pp:ProgramPoint): I
  def getCollectionTupleByKey(collectionKey: Assignable): I
  def getCollectionTupleByValue(collectionValue: Assignable) : I
  def getCollectionLength(collection: Assignable): I
  def getCollectionKey(collectionTuple:Assignable, keyTyp:Type): I
  def getCollectionValue(collectionTuple:Assignable, valueTyp:Type): I
}

//Approximates all the concrete references created at the same point of the program with a unique abstract reference
class NonRelationalHeapDomain[I <: NonRelationalHeapIdentifier[I]](env : VariableEnv[I], heap : HeapEnv[I], val cod : HeapIdSetDomain[I], dom : I)
    extends CartesianProductDomain[VariableEnv[I], HeapEnv[I], NonRelationalHeapDomain[I]](env, heap)
    with HeapDomain[NonRelationalHeapDomain[I], I]
    with HeapAnalysis[NonRelationalHeapDomain[I], I] {

  private def variableEnv = this._1
  private def heapEnv = this._2

  override def endOfAssignment() = (this, new Replacement());

  override def getIds : Set[Identifier] = (this._1.getIds++this._2.getIds).asInstanceOf[Set[Identifier]];
  override def lubWithReplacement[S <: SemanticDomain[S]](left : NonRelationalHeapDomain[I], right : NonRelationalHeapDomain[I], leftSemantic : S, rightSemantic : S) = (this.lub(left, right), new Replacement)
  override def glbWithReplacement[S <: SemanticDomain[S]](left : NonRelationalHeapDomain[I], right : NonRelationalHeapDomain[I], leftSemantic : S, rightSemantic : S) = (this.glb(left, right), new Replacement)
  override def wideningWithReplacement[S <: SemanticDomain[S]](left : NonRelationalHeapDomain[I], right : NonRelationalHeapDomain[I], leftSemantic : S, rightSemantic : S) = (this.widening(left, right), new Replacement)
  override def lessEqualWithReplacement[S <: SemanticDomain[S]](right : NonRelationalHeapDomain[I], thisSemantic : S, rightSemantic : S) = (this.lessEqual(right), new Replacement)
  override def reset() : Unit = {
     if(NonRelationalHeapDomainSettings.unsoundEntryState)
       ParameterIds.reset()
  }
  def setType(t : Type) = {
    env.typ=t;
    heap.typ=t;
    dom.typ=t;
    //cod.typ=t;
  }
  override def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier : Assignable, index : Expression, state : S, typ : Type)
    = (new MaybeHeapIdSetDomain().convert(dom.getArrayCell(arrayIdentifier, index)), this, new Replacement);

  override def createArray[S <: SemanticDomain[S]](length : Expression, typ : Type, pp : ProgramPoint, state : S)
    = (new MaybeHeapIdSetDomain().convert(dom.createArray(length, typ, pp)), this, new Replacement);

  override def getArrayLength(id : Assignable)
    = (new MaybeHeapIdSetDomain().convert(dom.getArrayLength(id)), this, new Replacement);

  def assignArrayCell[S <: SemanticDomain[S]](obj : Assignable, index : Expression, expr : Expression, state : S) = {
    var result=this.bottom();
    val ids = this.getArrayCell(obj, index, state, expr.getType())._1;
    for(id <- ids.value)
      result=result.lub(result, this.assign(id, expr, null)._1);
    (result, new Replacement);
  }

  override def getNativeMethodsSemantics() : List[NativeMethodSemantics] = Nil;
  override def getLabel() : String = "Heap Domain:"+dom.getLabel();
  override def parameters() : List[(String, Any)] = List((("UnsoundEntryState"), true), (("MaxEntryNodes"), 10))
  override def setParameter(label : String, value : Any) : Unit = label match {
    case "UnsoundEntryState" => value match {
      case b : Boolean => NonRelationalHeapDomainSettings.unsoundEntryState=b;
      case s : String => NonRelationalHeapDomainSettings.unsoundEntryState=s.toBoolean;
    }
    case "MaxEntryNodes" => value match {
      case b : Int => NonRelationalHeapDomainSettings.maxInitialNodes=b;
      case s : String => NonRelationalHeapDomainSettings.maxInitialNodes=s.toInt;
    }
  };
  override def getInitialState() = new NonRelationalHeapDomain(new VariableEnv(env.typ, env.dom), new HeapEnv(heap.typ, heap.dom), cod, dom);
  override def getProperties() : Set[Property] = Set.empty+ShowGraph;

  def this(typ : Type, cod : HeapIdSetDomain[I], dom : I) {
    this(new VariableEnv(typ, cod), new HeapEnv(typ, cod), cod, dom)
  }
  def getStringOfId(id : Identifier) : String = id match {
    case x : VariableIdentifier => this.get(x).toString();
    case x : HeapIdSetDomain[I] => this.get(x).toString();
  }

  def factory() = new NonRelationalHeapDomain(d1.factory(), d2.factory(), cod.factory(), dom.factory())
  
  def get(key : VariableIdentifier) : HeapIdSetDomain[I] = this._1.value.get(key) match {
    case None => cod.top();
    case Some(x) => x
  }

  def get(key : HeapIdSetDomain[I]) : HeapIdSetDomain[I] = {
    var result = cod.bottom();
    for(addr <- key.value) 
      this._2.value.get(addr) match {
      	case None => return cod.top();
      	case Some(x) => result=result.lub(result, x)
      }
    result
  }

  def get(key : I) : HeapIdSetDomain[I] = this._2.get(key);

  override def createVariable(variable : Assignable, typ : Type) =  variable match {
    case x : VariableIdentifier => (new NonRelationalHeapDomain(this._1.add(x, cod.bottom()), this._2, cod, dom), new Replacement);
    case x : I => (this, new Replacement)
  }

   override def createVariableForArgument(variable : Assignable, typ : Type, path : List[String])  =  variable match {
    case x : VariableIdentifier =>
      if(typ.isObject) {
	    var (result, r)=this.createVariable(variable, typ); //r will be always empty, so I ignore it
	    var ids : Map[Identifier, List[String]] = Map.empty[Identifier, List[String]];
	    alreadyInitialized = Set.empty[I];
	    this.initializeObject(x, dom.createAddressForArgument(typ, x.getProgramPoint), typ, result, path ::: variable.toString() :: Nil);
      }
      else {
        var result = Map.empty[Identifier, List[String]];
        result=result+((x, variable.toString() :: Nil ))
        (new NonRelationalHeapDomain(this._1.add(x.asInstanceOf[VariableIdentifier], cod.bottom()), this._2, cod, dom), result, new Replacement);
        }
     case x : HeapIdentifier[I] => {throw new Exception("This should not happen!");}
  }
  
  private var alreadyInitialized : Set[I] = Set.empty[I];
  private var fieldsInitialized : Set[I] = Set.empty[I];
  
  private def initializeObject(x : Identifier, obj : I, typ : Type, heap : NonRelationalHeapDomain[I], path : List[String]) : (NonRelationalHeapDomain[I], Map[Identifier, List[String]], Replacement) = {
	  if(/*typ.isObject && */! alreadyInitialized.contains(obj)) {
	  	var result=heap;
	    var ids : Map[Identifier, List[String]] = Map.empty[Identifier, List[String]];
	    ids=ids+((obj, path ));
	    val newAdd=cod.convert(obj);
	    if(! NonRelationalHeapDomainSettings.unsoundEntryState)
		    for(add <- result.getIds)
          if(add.isInstanceOf[I] && add.getType().lessEqual(typ))
		        newAdd.add(add.asInstanceOf[I]);
	    if(x.isInstanceOf[VariableIdentifier])
	    	result=new NonRelationalHeapDomain(result._1.add(x.asInstanceOf[VariableIdentifier], newAdd), result._2, cod, dom);
     
	    alreadyInitialized=alreadyInitialized+obj;
	    val c = typ.getPossibleFields;
	    for(field <- c) {
	      val adds = cod.convert(dom.createAddressForArgument(field.getType(), x.getProgramPoint));
        //I can ignore newHeap since it's equal to initial as it is not changed by getFieldIdentifier
        //in the same way I ignore rep
	      val (fieldAdd, newHeap, rep)=result.getFieldIdentifier(obj, field.getName(), field.getType(), field.getProgramPoint());
	      for(id : I <- fieldAdd.value) {
	    	  result=new NonRelationalHeapDomain(result._1, result._2.add(id, adds), cod, dom);
	    	  ids=ids+((id, path ::: (field.getName()) :: Nil));
	    	  val r=initializeObject(id, id, id.getType, result, path ::: (field.getName()) :: Nil)
	    	  alreadyInitialized=alreadyInitialized+id;
	    	  result=r._1;
	    	  ids=r._2++ids;//This order is quite important: in this way we keep the shortest path to arrive to an abstract node!
	      }
	    }
	    (result, ids, new Replacement)
        }
	  else (heap, Map.empty[Identifier, List[String]], new Replacement);
  }

  override def setArgument(variable : Assignable, expr : Expression) = this.assign(variable, expr, null);
  
  override def backwardAssign(variable : Assignable, expr : Expression) = (this, new Replacement)

  override def assignField(variable : Assignable, s : String, expr : Expression) : (NonRelationalHeapDomain[I], Replacement) = {
    var result=this.bottom();
    val ids = this.getFieldIdentifier(variable, s, expr.getType, variable.getProgramPoint())._1;
    for(id <- ids.value)
      result=result.lub(result, this.assign(id, expr, null)._1);
    (result, new Replacement);
    //We ignore the other parts since getting a field does not modify a non relational heap domain
  }

  override def assign[S <: SemanticDomain[S]](variable : Assignable, expr : Expression, state : S) : (NonRelationalHeapDomain[I], Replacement) = {

    if(! variable.getType().isObject)
      // It does not modify the heap
      return (this, new Replacement)

    variable match {

	    case x : VariableIdentifier =>
        expr match {
          case value : HeapIdSetDomain[I] =>
            var result=this._1
            //for(heapid <- this.normalize(value).value)
            //	if(value.value.size==1)
                result=result.add(x, this.normalize(value))
            //	else initial=initial.add(x, value.lub(this.normalize(value), this._1.get(x)));
            (new NonRelationalHeapDomain(result, this._2, cod, dom), new Replacement)
          case _ =>
            val value=this.eval(expr)
            (new NonRelationalHeapDomain(this._1.add(x, this.normalize(value)), this._2, cod, dom), new Replacement)
        }

	    case x : I =>
	      val value=this.eval(expr)
        // Brutschy: Following my understanding of the weak update implementation, we need the
        //           following distinction between summary nodes and non-summary nodes
	      var result =
          if (x.representSingleVariable())
            this._2.add(x, this.normalize(value))
          else
            this._2.add(x, this.get(x).add(this.normalize(value)))
	      return (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement);

	    case x : HeapIdSetDomain[I] =>
	      if(x.isTop)
	        return (this.top(), new Replacement);
	      var result=this._2;
	      val value=this.eval(expr)
        //TODO:Distinguish between definite and maybe
	      for(addr <- x.value/*this.normalize(x).value*/)
	        result=result.add(addr, value.lub(this.normalize(value), this._2.get(addr)))
	      return (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement);

    }
  }
  
  override def setToTop(variable : Assignable) = variable match  {
    case x : VariableIdentifier => (new NonRelationalHeapDomain(this._1.add(x, cod.top()), this._2, cod, dom), new Replacement)
    case x : HeapIdSetDomain[I] =>;
      var result=this._2;
      for(addr <- x.value)
        result=result.add(addr, cod.top())
      (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement);
    case x : I =>
      (new NonRelationalHeapDomain(this._1, this._2.add(x, cod.top()), cod, dom), new Replacement)
  }  
  
  override def removeVariable(variable : Assignable) = variable match  {
    case x : VariableIdentifier => (new NonRelationalHeapDomain(this._1.remove(x), this._2, cod, dom), new Replacement)
    case x : HeapIdSetDomain[I] =>;
      var result=this._2;
      for(addr <- x.value)
        result=result.remove(addr)
      (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement);
    case x : I =>
      (new NonRelationalHeapDomain(this._1,this._2.remove(x),cod,dom),new Replacement)
  }  
  
  override def createObject(typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], NonRelationalHeapDomain[I], Replacement) = (cod.convert(dom.createAddress(typ, pp)), this, new Replacement);
  
  override def getFieldIdentifier(heapIdentifier : Assignable, name : String, typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], NonRelationalHeapDomain[I], Replacement) = (this.evalFieldAccess(heapIdentifier, name, typ), this, new Replacement);

  override def createEmptyCollection(collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint) = {
    val coll = dom.createCollection(collTyp, keyTyp, valueTyp, lengthTyp, pp)
    val overApprox = dom.getCollectionOverApproximation(coll)
    this.heapEnv.add(overApprox, new MaybeHeapIdSetDomain[I]().bottom())

    (new MaybeHeapIdSetDomain().convert(coll), this)
  }

  override def getCollectionKey[S <: SemanticDomain[S]](collection: Assignable, key: Expression, state:S) =  {

    def f(a:Assignable) = {
      val collectionId = a.asInstanceOf[CollectionIdentifier]
      val collectionApprox = dom.getCollectionOverApproximation(collectionId)

      def getCollectionKey(a: Assignable) = a match {
        case tupleId: CollectionTupleIdentifier => new MaybeHeapIdSetDomain[I]().convert(dom.getCollectionKey(tupleId, key.getType()))
        case _ => throw new SemanticException("This is not a collection tuple " + a.toString)
      }

      HeapIdSetFunctionalLifting.applyToSetHeapId(new MaybeHeapIdSetDomain[I](), getCollectionTuplesByKey(collectionApprox, key, state), getCollectionKey(_))
    }
    (resolveVariables(new MaybeHeapIdSetDomain[I](), collection, f(_)), this)
  }

  override def getCollectionValueByKey[S <: SemanticDomain[S]](collection: Assignable, key: Expression, state: S) = {

    def f(state: S, key: Expression)(a:Assignable) = {
      val collectionId = a.asInstanceOf[CollectionIdentifier]
      val collectionApprox = dom.getCollectionOverApproximation(collectionId)

      def getCollectionValue(valueTyp: Type)(a: Assignable) = a match {
        case tupleId: CollectionTupleIdentifier => new MaybeHeapIdSetDomain[I]().convert(dom.getCollectionValue(tupleId, valueTyp))
        case _ => throw new SemanticException("This is not a collection tuple " + a.toString)
      }

      HeapIdSetFunctionalLifting.applyToSetHeapId(new MaybeHeapIdSetDomain[I](), getCollectionTuplesByKey(collectionApprox, key, state), getCollectionValue(collectionId.valueTyp))
    }
    (resolveVariables(new MaybeHeapIdSetDomain[I](), collection, f(state, key)), this)
  }

  override def getCollectionValueByValue[S <: SemanticDomain[S]](collection: Assignable, value: Expression, state: S) = {
    def f(state: S, value: Expression)(a:Assignable) = {
      val collectionId = a.asInstanceOf[CollectionIdentifier]
      val collectionApprox = dom.getCollectionOverApproximation(collectionId)

      def getCollectionValue(a: Assignable) = a match {
        case tupleId: CollectionTupleIdentifier => new MaybeHeapIdSetDomain[I]().convert(dom.getCollectionValue(tupleId, value.getType()))
        case _ => throw new SemanticException("This is not a collection tuple " + a.toString)
      }

      HeapIdSetFunctionalLifting.applyToSetHeapId(new MaybeHeapIdSetDomain[I](), getCollectionTuplesByValue(collectionApprox, value, state), getCollectionValue(_))
    }
    (resolveVariables(new MaybeHeapIdSetDomain[I](), collection, f(state, value)), this)
  }

  override def getCollectionKeyByTuple(collectionTuple: Assignable, keyTyp: Type): Assignable = {
    return this.dom.getCollectionKey(collectionTuple, keyTyp)
  }

  override def getCollectionValueByTuple(collectionTuple: Assignable, valueTyp: Type): Assignable = {
    return this.dom.getCollectionValue(collectionTuple, valueTyp)
  }

  override def getCollectionTupleByKey(keyId: Assignable) : Assignable = {
    return this.dom.getCollectionTupleByKey(keyId)
  }

  def getCollectionTupleByValue(valueId: Assignable) : Assignable = {
    return this.dom.getCollectionTupleByValue(valueId)
  }

  override def insertCollectionElement(collectionA: Assignable, pp: ProgramPoint) = {

    var keys = new MaybeHeapIdSetDomain[I]().bottom()
    var values = new MaybeHeapIdSetDomain[I]().bottom()

    def insertSingleCollectionElement(initialHeap: NonRelationalHeapDomain[I])(a: Assignable): NonRelationalHeapDomain[I] = a match {
      case collection:CollectionIdentifier =>
        var result = initialHeap

        val overApproxId = dom.getCollectionOverApproximation(collection)
        val tupleId = dom.createCollectionTuple(overApproxId, pp)
        val newIds = result.heapEnv.get(overApproxId).add(tupleId)
        val newHeapEnv = result.heapEnv.remove(overApproxId).add(overApproxId, newIds)
        result = new NonRelationalHeapDomain[I](result.variableEnv, newHeapEnv, result.cod, dom)

        val key = dom.getCollectionKey(tupleId, collection.keyTyp)
        val value = dom.getCollectionValue(tupleId, collection.valueTyp)
        keys = keys.add(key)
        values = values.add(value)

        result

      case _ => initialHeap
    }

    var heap = this.bottom()
    collectionA match {
      case variable: VariableIdentifier => heap = resolveVariables(this.factory(), variable, insertSingleCollectionElement(this))
      case id: CollectionIdentifier => heap = insertSingleCollectionElement(this)(id)
      case set: HeapIdSetDomain[I] => heap = HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, resolveVariables(this.factory(), _, insertSingleCollectionElement(this)))
      case _ => throw new SemanticException("This identifier does not represent a collection " + collectionA.toString)
    }

    (keys, values, heap)
  }

  override def removeCollectionElement(collectionTuple: Assignable, keyTyp: Type, valueTyp: Type) = {
    var result = this

    val key = this.getCollectionKeyByTuple(collectionTuple, keyTyp)
    val (newHeap, _) = result.removeVariable(key)
    result = newHeap

    val value = this.getCollectionValueByTuple(collectionTuple, valueTyp)
    val (newHeap2, _) = result.removeVariable(value)
    result = newHeap2

    val (newHeap3, _) = result.removeVariable(collectionTuple)
    result = newHeap3
    collectionTuple match {
      case id: CollectionTupleIdentifier =>
        val approx = id.collectionApprox.asInstanceOf[I]
        val newTuples = result.d2.get(approx).remove(id.asInstanceOf[I])
        result.d2 = result.d2.remove(approx).add(approx, newTuples)
      case _ => throw new SemanticException("This is not a collection tuple " + collectionTuple.toString)
    }

    result
  }

  override def clearCollection(collection: Assignable) = {
    def clear(initialHeap: NonRelationalHeapDomain[I])(a: Assignable) = a match {
      case collection: CollectionIdentifier =>

        val overApprox = dom.getCollectionOverApproximation(collection)
        var newHeap = initialHeap.removeCollectionTuples(overApprox, collection.keyTyp, collection.valueTyp)
        val (newHeap2, _) = newHeap.removeVariable(overApprox)

        newHeap2
      case _ => initialHeap
    }

    resolveVariables(this.factory(), collection, clear(this))
  }

  override def getCollectionTuples(collection: Assignable): HeapIdSetDomain[I] = {
    def getTuples(a: Assignable): HeapIdSetDomain[I] = a match {
      case collectionId: CollectionIdentifier => this.heapEnv.get(dom.getCollectionOverApproximation(collectionId))
      case _ => throw new SemanticException("This is not a collection " + collection.toString)
    }

    resolveVariables(new MaybeHeapIdSetDomain[I](), collection, getTuples)
  }

  override def getCollectionKeys(collection: Assignable): HeapIdSetDomain[I] = {
    def getKeys(a: Assignable): HeapIdSetDomain[I] = a match {
      case collectionId: CollectionIdentifier =>
        val tupleIds = this.heapEnv.get(dom.getCollectionOverApproximation(collectionId))

        def f(typ: Type)(a:Assignable) = new MaybeHeapIdSetDomain[I]().convert(dom.getCollectionKey(a, typ))
        HeapIdSetFunctionalLifting.applyToSetHeapId(tupleIds, tupleIds, f(collectionId.keyTyp))

      case _ => throw new SemanticException("This is not a collection " + collection.toString)
    }

    resolveVariables(new MaybeHeapIdSetDomain[I](), collection, getKeys)
  }

  override def getCollectionValues(collection: Assignable): HeapIdSetDomain[I] = {
    def getValues(a: Assignable): HeapIdSetDomain[I] = a match {
      case collectionId: CollectionIdentifier =>
        val tupleIds = this.heapEnv.get(dom.getCollectionOverApproximation(collectionId))

        def f(typ: Type)(a:Assignable) = new MaybeHeapIdSetDomain[I]().convert(dom.getCollectionValue(a, typ))
        HeapIdSetFunctionalLifting.applyToSetHeapId(tupleIds, tupleIds, f(collectionId.valueTyp))

      case _ => throw new SemanticException("This is not a collection " + collection.toString)
    }

    resolveVariables(new MaybeHeapIdSetDomain[I](), collection, getValues)
  }

  override def getCollectionLength(collection: Assignable) = {
    def f(a:Assignable):HeapIdSetDomain[I] = new MaybeHeapIdSetDomain().convert(dom.getCollectionLength(a))
    ((resolveVariables(new MaybeHeapIdSetDomain(), collection, f(_))), this)
  }

  private def getCollectionTuplesByKey[S <: SemanticDomain[S]](collectionApprox: Assignable, key: Expression, state: S): HeapIdSetDomain[I] = {
    def filter(state: S, key: Expression)(a:Assignable) = a match {
      case tupleId:CollectionTupleIdentifier =>
        val keyId = dom.getCollectionKey(tupleId, key.getType())
        val r = state.assume(BinaryArithmeticExpression(keyId, key, ArithmeticOperator.==, null))
        !r.lessEqual(state.bottom())
      case _ => throw new SemanticException("This is not a collection tuple " + a.toString)
    }

    getCollectionTuples(collectionApprox, filter(state, key))
  }

  private def getCollectionTuplesByValue[S <: SemanticDomain[S]](collectionApprox: Assignable, value: Expression, state: S): HeapIdSetDomain[I] = {
    def filter(state: S, value: Expression)(a:Assignable): Boolean = a match {
      case tupleId:CollectionTupleIdentifier =>
        val valueId = dom.getCollectionValue(tupleId, value.getType())
        val r = state.assume(BinaryArithmeticExpression(valueId, value, ArithmeticOperator.==, null))
        !r.lessEqual(state.bottom())
      case _ => throw new SemanticException("This is not a collection tuple " + a.toString)
    }

    getCollectionTuples(collectionApprox, filter(state, value))
  }

  private def getCollectionTuples(collectionApprox: Assignable, filter: Assignable => Boolean): HeapIdSetDomain[I] = {
    var result = new MaybeHeapIdSetDomain[I]().bottom()

    collectionApprox match {
      case collectionApproxId:I =>
        for(id <- this.heapEnv.get(collectionApproxId).value) id match{
          case tupleId:CollectionTupleIdentifier =>
            if (filter(tupleId)){
              result = result.lub(result, new MaybeHeapIdSetDomain[I]().convert(id))
            }
          case _ => throw new SemanticException("This is not a collection tuple " + id.toString)
        }
      case _ => throw new SemanticException("This is not a collection approximation " + collectionApprox.toString)
    }
    result
  }

  private def removeCollectionTuples(approx: Assignable, keyTyp: Type, valueTyp: Type) = approx match {
    case id: I =>
      val tupleIds = getCollectionTuples(id)
      var heap = this
      for (tupleId <- tupleIds.value) {
        val (newHeap, _) = heap.removeVariable(dom.getCollectionKey(tupleId, keyTyp))
        heap = newHeap

        val (newHeap2, _) = heap.removeVariable(dom.getCollectionValue(tupleId, valueTyp))
        heap = newHeap2

        val (newHeap3, _) = heap.removeVariable(tupleId)
        heap = newHeap3
      }

      heap
    case _ => throw new SemanticException("This is not a collection approximation " + approx.toString)
  }

  private def removeCollectionTuples[S <: SemanticDomain[S]](approx: Assignable, key: Expression, state: S) = approx match {
    case id: I =>
      val tupleIds = getCollectionTuplesByKey(id, key, state)
      var heap = this
      for (tupleId <- tupleIds.value) {
        val (newHeap, _) = heap.removeVariable(tupleId)
        heap = newHeap
      }

      heap
    case _ => this
  }

  def getUnreachableHeap:Set[I] = {
    heap.getIds.filter(ReachabilityAnalysis.reach(_,env,heap)._2)
  }

  private def resolveVariables[T <: Lattice[T]](fact:T, a: Assignable, f : Assignable => T):T = {
    a match {
      case id:VariableIdentifier => HeapIdSetFunctionalLifting.applyToSetHeapId(fact, this.normalize(d1.get(id)),f)
      case ids:HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(fact, this.normalize(ids),f)
      case id:I => HeapIdSetFunctionalLifting.applyToSetHeapId(fact, this.normalize(new MaybeHeapIdSetDomain().convert(id)),f)
      case _ => f(a)
    }
  }

  private def evalFieldAccess[S <: State[S]](expr : Assignable, field : String, typ : Type) : HeapIdSetDomain[I] = expr match {
    case obj : VariableIdentifier => return extractField(this.get(obj), field, typ)
    
    case obj : HeapIdSetDomain[I] => {
      var result : HeapIdSetDomain[I] = cod.bottom();
      for(simplepp : I <- obj.value)
            result=result.lub(result, extractField(simplepp, field, typ));

      //If I don't have any information, I return a top identifier
      if(result.isBottom)
        return result.top();
      return result;
    }
    //case obj : I => return extractField(this.get(obj.asInstanceOf[I]), field, typ)
      case obj : I => return extractField(obj.asInstanceOf[I], field, typ)

    //case obj : SimpleProgramPointHeapIdentifier => return extractField(obj.asInstanceOf[I], field, typ)

    //case _ => return throw new Exception();//cod.top();
  }
  
  private def extractField(obj : I, field : String, typ : Type) : HeapIdSetDomain[I] = {
    var result : HeapIdSetDomain[I] = cod.bottom();
    for(id <- this.normalize(cod.convert(obj)).value)
      result=result.add(dom.extractField(id, field, typ));
    if(result.isBottom)
      return result.top();
    return result;
  }
  
  
  private def normalize(id : HeapIdSetDomain[I]) : HeapIdSetDomain[I] = {
    var result=id.factory();
    for(add <- id.value)
      if(add.isNormalized)
        result=result.add(add);
      else result=result.add(this._2.get(add));
    return result;
  }
  
  private def extractField(obj : HeapIdSetDomain[I], field : String, typ : Type) : HeapIdSetDomain[I] = {
    var result : HeapIdSetDomain[I] = cod.bottom();
    if(obj.isBottom) return result.bottom();
    if(obj.isTop) {//We manage the case in which we use the object to access a static object -> useful in Scala
      if(typ != null && typ.isStatic) {
    	  typ.isStatic;
        return result.add(dom.accessStaticObject(typ, obj.getProgramPoint));
      }
    }
    val accessed = this.normalize(obj);
    for(node <- accessed.value)
      result=result.add(dom.extractField(node, field, typ));
    //If I don't have any information, I return a top identifier
    if(result.isBottom)
      return result.top();
    return result;
  }
  
  private def eval[S <: State[S]](expr : Expression) : HeapIdSetDomain[I] = expr match {
    case x : VariableIdentifier => this.get(x);
    case x : HeapIdSetDomain[I] => x;                /*
      var initial=cod.bottom();
      for(addr <- x.value)
        initial=initial.lub(initial, this._2.get(addr));
      return initial;                               */
    case Constant("null", typ, pp) => return cod.convert(dom.getNullNode(pp));
    
    case x => return cod.top();
  }
  
  override def assume(expr : Expression) = 
    (this, new Replacement) //TODO: for now there is nothing about the heap structure

}

case class TopHeapIdentifier(typ2 : Type, pp2 : ProgramPoint) extends NonRelationalHeapIdentifier[TopHeapIdentifier](typ2, pp2) {

    override def getArrayCell(array : Assignable, index : Expression) = this;
    override def getArrayLength(array : Assignable) = this;
    override def getLabel() = "Top";
	  override def getNullNode(pp : ProgramPoint) = this
	  override def getField() : Option[String] = None;
	  override def isNormalized() : Boolean = true;
	  override def representSingleVariable()=false;
	  override def getName() = "#abstractReference#"
	  override def equals(o : Any) = o match {
	    case x : TopHeapIdentifier => true
	    case _ => false
	  }
	  override def factory() = this;
      override def createAddress(typ : Type, pp : ProgramPoint)=this;
      override def createAddressForArgument(typ : Type, pp : ProgramPoint)=this;
      override def extractField(obj : TopHeapIdentifier, field : String, typ : Type)=this;
      override def accessStaticObject(typ : Type, pp : ProgramPoint)=this;
	  override def hashCode() : Int = 0;

    def createCollection(collTyp: Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint) = this
    def getCollectionOverApproximation(collection: Assignable) = this
    def getCollectionUnderApproximation(collection: Assignable) = this
    def createCollectionTuple(collectionApprox:Assignable, pp:ProgramPoint) = this
    def getCollectionTupleByKey(collectionKey: Assignable) = this
    def getCollectionTupleByValue(collectionValue: Assignable) = this
    def getCollectionLength(collection: Assignable) = this
    def getCollectionKey(collectionTuple:Assignable, keyTyp:Type) = this
    def getCollectionValue(collectionTuple:Assignable, valueTyp:Type) = this
}
