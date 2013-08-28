package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample._
import abstractdomain._
import abstractdomain.Constant
import abstractdomain.VariableIdentifier
import oorepresentation._
import userinterfaces.ShowGraph
import property.Property
import util.HeapIdSetFunctionalLifting
import scala.Some
import scala.collection.mutable

object NonRelationalHeapDomainSettings {
  var unsoundEntryState : Boolean = true;
  var maxInitialNodes : Int = 5;
}

class HeapEnv[I <: NonRelationalHeapIdentifier[I]](var typ : Type, val dom : HeapIdSetDomain[I]) extends FunctionalDomain[I, HeapIdSetDomain[I], HeapEnv[I]]
    with LatticeWithReplacement[HeapEnv[I]] {
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

  override def lub(l : HeapEnv[I], r : HeapEnv[I]):HeapEnv[I] = throw new UnsupportedOperationException("Use lubWithReplacement")
  override def glb(l : HeapEnv[I], r : HeapEnv[I]):HeapEnv[I] = throw new UnsupportedOperationException("Use glbWithReplacement")
  override def widening(l : HeapEnv[I], r : HeapEnv[I]):HeapEnv[I] = throw new UnsupportedOperationException("Use wideningWithReplacement")

  private def lubMergeSummaries(l: HeapEnv[I], r: HeapEnv[I]): (HeapEnv[I], HeapEnv[I], Replacement) = {
    val leftSummaryNodes = l.getIds collect { case x:I if !x.representSingleVariable() => x }
    val rightSummaryNodes = r.getIds collect { case x:I if !x.representSingleVariable() => x }

    val makeSummaryLeft = (rightSummaryNodes -- leftSummaryNodes).map(_.toNonSummaryNode)
    val makeSummaryRight = (leftSummaryNodes -- rightSummaryNodes).map(_.toNonSummaryNode)

    if (makeSummaryLeft.isEmpty && makeSummaryRight.isEmpty) return (l, r, new Replacement())

    def collectReferences(nodes: Set[I], heapEnv: HeapEnv[I]): Set[I] = {
      if (nodes.isEmpty) return nodes
      val references = heapEnv.getIds collect { case x:I if !x.getReachableFromIds.intersect(nodes).isEmpty => x }
      references ++ collectReferences(references, heapEnv)
    }
    // Also convert nodes that refer to summary nodes (fields of summary nodes, length of summarized collections, collection tuples)
    val makeSummaryLeftRef = collectReferences(makeSummaryLeft, l)
    val makeSummaryRightRef = collectReferences(makeSummaryRight, r)

    val replaceLeft = new Replacement
    for (a <- makeSummaryLeft ++ makeSummaryLeftRef)
      replaceLeft.value += (Set[Identifier](a) -> Set[Identifier](a.toSummaryNode))
    val replaceRight = new Replacement
    for (a <- makeSummaryRight ++ makeSummaryRightRef)
      replaceRight.value += (Set[Identifier](a) -> Set[Identifier](a.toSummaryNode))

    (l.merge(replaceLeft), r.merge(replaceRight), replaceLeft ++ replaceRight)
  }

  override def lubWithReplacement(l : HeapEnv[I], r : HeapEnv[I]):(HeapEnv[I],Replacement) = {
    if (l.isBottom) return (r,new Replacement())
    if (r.isBottom) return (l,new Replacement())

    val (left, right, rep) = lubMergeSummaries(l, r)
    val result = super.lub(left, right)
    (result, rep)
  }

  override def glbWithReplacement(l : HeapEnv[I], r : HeapEnv[I]):(HeapEnv[I],Replacement) = {

    if (l.isBottom) return (l,new Replacement())
    if (r.isBottom) return (r,new Replacement())

    val leftNonSummaryNodes = l.getIds collect
      { case x:I if x.representSingleVariable() => x }
    val rightNonSummaryNodes = r.getIds collect
      { case x:I if x.representSingleVariable() => x }

    val makeNonSummaryLeft = rightNonSummaryNodes -- leftNonSummaryNodes
    val makeNonSummaryRight = leftNonSummaryNodes -- rightNonSummaryNodes

    def collectReferences(nodes: Set[I], heapEnv: HeapEnv[I]): Set[I] = {
      if (nodes.isEmpty) return nodes
      val references = heapEnv.getIds collect { case x:I if !x.getReachableFromIds.intersect(nodes).isEmpty => x }
      references ++ collectReferences(references, heapEnv)
    }
    // Also convert nodes that refer to summary nodes (fields of summary nodes, length of summarized collections, collection tuples)
    val makeNonSummaryLeftRef = collectReferences(makeNonSummaryLeft, l)
    val makeNonSummaryRightRef = collectReferences(makeNonSummaryRight, r)

    val replaceLeft = new Replacement
    for (a <- makeNonSummaryLeft ++ makeNonSummaryLeftRef)
      replaceLeft.value += (Set[Identifier](a.toSummaryNode) -> Set[Identifier](a))
    val replaceRight = new Replacement
    for (a <- makeNonSummaryRight ++ makeNonSummaryRightRef)
      replaceRight.value += (Set[Identifier](a.toSummaryNode) -> Set[Identifier](a))

    val result = super.glb(l.merge(replaceLeft), r.merge(replaceRight))

    (result, replaceLeft ++ replaceRight)
  }

  override def wideningWithReplacement(l : HeapEnv[I], r : HeapEnv[I]):(HeapEnv[I],Replacement) = lubWithReplacement(l,r)

  def get(key : I) : HeapIdSetDomain[I] = this.value.get(key) match {
    case None => dom.bottom(); //TODO: This is not sound!!!
    case Some(x) => x
  }

  def merge(rep:Replacement):HeapEnv[I] = {

    var curVal = value

    // handle keys
    for ( (froms,tos) <- rep.value ) {
      var right = dom.bottom()
      for ( from <- froms ) {
        from match {
          case x:I =>
            curVal.get(x) match {
              case Some(y) =>
                right = dom.lub(right,y)
                curVal = curVal - x
              case None => ()
            }
          case _ => ()
        }
      }
      for ( to <- tos ) {
        to match {
          case x:I =>
            curVal = curVal + (x -> right)
          case _ => ()
        }
      }
    }

    // handle values
    curVal = curVal.mapValues( _.merge(rep) )

    val ret = this.factory()
    ret.value = curVal
    ret

  }

}

class VariableEnv[I <: NonRelationalHeapIdentifier[I]](var typ : Type, val dom : HeapIdSetDomain[I])
    extends FunctionalDomain[VariableIdentifier, HeapIdSetDomain[I], VariableEnv[I]]
    with LatticeWithReplacement[VariableEnv[I]] {

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

  override def lub(l : VariableEnv[I], r : VariableEnv[I]):VariableEnv[I] = throw new UnsupportedOperationException("Use lubWithReplacement")
  override def glb(l : VariableEnv[I], r : VariableEnv[I]):VariableEnv[I] = throw new UnsupportedOperationException("Use glbWithReplacement")
  override def widening(l : VariableEnv[I], r : VariableEnv[I]):VariableEnv[I] = throw new UnsupportedOperationException("Use wideningWithReplacement")

  override def lubWithReplacement(l : VariableEnv[I], r : VariableEnv[I]):(VariableEnv[I],Replacement) = {

    if (l.isBottom) return (r,new Replacement())
    if (r.isBottom) return (l,new Replacement())

    val leftSummaryNodes = l.getIds collect
      { case x:I if !x.representSingleVariable() => x }
    val rightSummaryNodes = r.getIds collect
      { case x:I if !x.representSingleVariable() => x }

    val makeSummaryLeft = rightSummaryNodes -- leftSummaryNodes
    val makeSummaryRight = leftSummaryNodes -- rightSummaryNodes

    def collectReferences(nodes: Set[I], env: VariableEnv[I]): Set[I] = {
      if (nodes.isEmpty) return nodes
      val references = env.getIds collect { case x:I if !x.getReachableFromIds.intersect(nodes).isEmpty => x }
      references ++ collectReferences(references, env)
    }

    // Also convert nodes that refer to summary nodes (fields of summary nodes, length of summarized collections, collection tuples)
    val makeSummaryLeftRef = collectReferences(makeSummaryLeft, l)
    val makeSummaryRightRef = collectReferences(makeSummaryRight, r)

    if (makeSummaryLeft.isEmpty && makeSummaryRight.isEmpty) return (super.lub(l,r),new Replacement())

    val replaceLeft = new Replacement
    for (a <- makeSummaryLeft ++ makeSummaryLeftRef)
      replaceLeft.value += (Set[Identifier](a.toNonSummaryNode) -> Set[Identifier](a))
    val replaceRight = new Replacement
    for (a <- makeSummaryRight ++ makeSummaryRightRef)
      replaceRight.value += (Set[Identifier](a.toNonSummaryNode) -> Set[Identifier](a))

    val result = super.lub(l.merge(replaceLeft), r.merge(replaceRight))

    (result, replaceLeft ++ replaceRight)
  }

  override def glbWithReplacement(l : VariableEnv[I], r : VariableEnv[I]):(VariableEnv[I],Replacement) = {

    if (l.isBottom) return (l,new Replacement())
    if (r.isBottom) return (r,new Replacement())

    val leftNonSummaryNodes = l.getIds collect
      { case x:I if x.representSingleVariable() => x }
    val rightNonSummaryNodes = r.getIds collect
      { case x:I if x.representSingleVariable() => x }

    val makeNonSummaryLeft = rightNonSummaryNodes -- leftNonSummaryNodes
    val makeNonSummaryRight = leftNonSummaryNodes -- rightNonSummaryNodes

    def collectReferences(nodes: Set[I], env: VariableEnv[I]): Set[I] = {
      if (nodes.isEmpty) return nodes
      val references = env.getIds collect { case x:I if !x.getReachableFromIds.intersect(nodes).isEmpty => x }
      references ++ collectReferences(references, env)
    }
    // Also convert nodes that refer to summary nodes (fields of summary nodes, length of summarized collections, collection tuples)
    val makeNonSummaryLeftRef = collectReferences(makeNonSummaryLeft, l)
    val makeNonSummaryRightRef = collectReferences(makeNonSummaryRight, r)

    val replaceLeft = new Replacement
    for (a <- makeNonSummaryLeft ++ makeNonSummaryLeftRef)
      replaceLeft.value += (Set[Identifier](a.toSummaryNode) -> Set[Identifier](a))
    val replaceRight = new Replacement
    for (a <- makeNonSummaryRight ++ makeNonSummaryRightRef)
      replaceRight.value += (Set[Identifier](a.toSummaryNode) -> Set[Identifier](a))

    val result = super.glb(l.merge(replaceLeft), r.merge(replaceRight))

    (result, replaceLeft ++ replaceRight)
  }

  override def wideningWithReplacement(l : VariableEnv[I], r : VariableEnv[I]):(VariableEnv[I],Replacement) = lubWithReplacement(l,r)

  def get(key : VariableIdentifier) : HeapIdSetDomain[I] = this.value.get(key) match {
    case None => dom.bottom(); //TODO: This is not sound!!!
    case Some(x) => x
  }

  def merge(rep:Replacement):VariableEnv[I] = {

    var curVal = value

    // handle keys
    for ( (froms,tos) <- rep.value ) {
      var right = dom.bottom()
      for ( from <- froms ) {
        from match {
          case x:VariableIdentifier =>
            curVal.get(x) match {
              case Some(y) =>
                right = dom.lub(right,y)
                curVal = curVal - x
              case None => ()
            }
          case _ => ()
        }
      }
      for ( to <- tos ) {
        to match {
          case x:VariableIdentifier =>
            curVal = curVal + (x -> right)
          case _ =>
            ()
        }
      }
    }

    curVal = curVal.mapValues( _.merge(rep) )

    val ret = this.factory()
    ret.value = curVal
    ret

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
  def toSummaryNode : I
  def toNonSummaryNode : I
  def getReachableFromIds : Set[I]
  def getCounter : Int
  def setCounter(c:Int) : I

  /**
   * Nodes of collections may have multiple access paths (through differnt keys/indexes).
   * Everything else should return false here
   */
  def hasMultipleAccessPaths : Boolean

  def createCollection(collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint): I
  def getCollectionOverApproximation(collection:Assignable): I
  def getCollectionUnderApproximation(collection:Assignable): I
  def createCollectionTuple(collectionApprox:Assignable, keyTyp: Type, valueTyp: Type, pp:ProgramPoint): I
  def createCollectionTuple(collectionApprox:Assignable, keyTyp: Type, valueTyp: Type, pps:Set[ProgramPoint]): I
  def createCollectionTuple(collectionTuple1: Assignable, collectionTuple2: Assignable): I
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

  override def getIds : Set[Identifier] = (this._1.getIds++this._2.getIds)

  override def lub(l : NonRelationalHeapDomain[I], r : NonRelationalHeapDomain[I]):NonRelationalHeapDomain[I] = throw new UnsupportedOperationException("Use lubWithReplacement")
  override def glb(l : NonRelationalHeapDomain[I], r : NonRelationalHeapDomain[I]):NonRelationalHeapDomain[I] = throw new UnsupportedOperationException("Use glbWithReplacement")
  override def widening(l : NonRelationalHeapDomain[I], r : NonRelationalHeapDomain[I]):NonRelationalHeapDomain[I] = throw new UnsupportedOperationException("Use wideningWithReplacement")

  override def lubWithReplacement(left : NonRelationalHeapDomain[I], right : NonRelationalHeapDomain[I]) = {

    val (res1,rep1) = d1.lubWithReplacement(left._1, right._1)
    val (res2,rep2) = d2.lubWithReplacement(left._2, right._2)

    val result: NonRelationalHeapDomain[I] = this.factory()
    result.d1 = res1
    result.d2 = res2
    result

    (result, rep1 ++ rep2)

  }

  override def glbWithReplacement(left : NonRelationalHeapDomain[I], right : NonRelationalHeapDomain[I]) = {

    val (res1,rep1) = d1.glbWithReplacement(left._1, right._1)
    val (res2,rep2) = d2.glbWithReplacement(left._2, right._2)

    val result: NonRelationalHeapDomain[I] = this.factory()
    result.d1 = res1
    result.d2 = res2
    result

    (result, rep1 ++ rep2)

  }

  override def wideningWithReplacement(left : NonRelationalHeapDomain[I], right : NonRelationalHeapDomain[I]) = {

    val (res1,rep1) = d1.wideningWithReplacement(left._1, right._1)
    val (res2,rep2) = d2.wideningWithReplacement(left._2, right._2)

    val result: NonRelationalHeapDomain[I] = this.factory()
    result.d1 = res1
    result.d2 = res2
    result

    (result, rep1 ++ rep2)

  }

  override def reset() {
    if(NonRelationalHeapDomainSettings.unsoundEntryState)
      ParameterIds.reset()
  }

  override def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier : Assignable, index : Expression, state : S, typ : Type)
  = (new MaybeHeapIdSetDomain().convert(dom.getArrayCell(arrayIdentifier, index)), this, new Replacement);

  override def createArray[S <: SemanticDomain[S]](length : Expression, typ : Type, pp : ProgramPoint, state : S)
  = (new MaybeHeapIdSetDomain().convert(dom.createArray(length, typ, pp)), this, new Replacement);

  override def getArrayLength(id : Assignable)
  = (new MaybeHeapIdSetDomain().convert(dom.getArrayLength(id)), this, new Replacement)

  def assignArrayCell[S <: SemanticDomain[S]](obj : Assignable, index : Expression, expr : Expression, state : S) = {
    var result=this.bottom()
    val ids = this.getArrayCell(obj, index, state, expr.getType())._1
    for(id <- ids.value)
      result=result.lub(result, this.assign(id, expr, null)._1)
    (result, new Replacement)
  }

  override def getNativeMethodsSemantics() : List[NativeMethodSemantics] = Nil;
  override def getLabel() : String = "Heap Domain:"+dom.getLabel()
  override def parameters() : List[(String, Any)] = List((("UnsoundEntryState"), true), (("MaxEntryNodes"), 10))
  override def setParameter(label : String, value : Any) : Unit = label match {
    case "UnsoundEntryState" => value match {
      case b : Boolean => NonRelationalHeapDomainSettings.unsoundEntryState=b
      case s : String => NonRelationalHeapDomainSettings.unsoundEntryState=s.toBoolean
    }
    case "MaxEntryNodes" => value match {
      case b : Int => NonRelationalHeapDomainSettings.maxInitialNodes=b
      case s : String => NonRelationalHeapDomainSettings.maxInitialNodes=s.toInt
    }
  }
  override def getInitialState() = new NonRelationalHeapDomain(new VariableEnv(env.typ, env.dom), new HeapEnv(heap.typ, heap.dom), cod, dom);
  override def getProperties() : List[Property] = List(ShowGraph)

  def this(typ : Type, cod : HeapIdSetDomain[I], dom : I) {
    this(new VariableEnv(typ, cod), new HeapEnv(typ, cod), cod, dom)
  }
  def getStringOfId(id : Identifier) : String = id match {
    case x : VariableIdentifier => this.get(x).toString()
    case x : HeapIdSetDomain[I] => this.get(x).toString()
  }

  def factory() = new NonRelationalHeapDomain(d1.factory(), d2.factory(), cod.factory(), dom.factory())

  def get(key : VariableIdentifier) : HeapIdSetDomain[I] = this._1.value.get(key) match {
    case None => cod.top()
    case Some(x) => x
  }

  def get(key : HeapIdSetDomain[I]) : HeapIdSetDomain[I] = {
    var result = cod.bottom()
    for(addr <- key.value)
      this._2.value.get(addr) match {
        case None => return cod.top()
        case Some(x) => result=result.lub(result, x)
      }
    result
  }

  def get(key : I) : HeapIdSetDomain[I] = this._2.get(key);

  override def createVariable(variable : Assignable, typ : Type) = {
    if (!getIds.contains(variable.asInstanceOf[Identifier])) {
      variable match {
        case x : VariableIdentifier =>
          (new NonRelationalHeapDomain(this._1.add(x, cod.bottom()), this._2, cod, dom), new Replacement)
        case x : I =>
          (new NonRelationalHeapDomain(this._1, this._2.add(x, cod.bottom()), cod, dom), new Replacement)
      }
    } else (this,new Replacement)
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
      var result=heap
      var ids : Map[Identifier, List[String]] = Map.empty[Identifier, List[String]]
      ids=ids+((obj, path ))
      val newAdd=cod.convert(obj)
      if(! NonRelationalHeapDomainSettings.unsoundEntryState)
        for(add <- result.getIds)
          if(add.isInstanceOf[I] && add.getType().lessEqual(typ))
            newAdd.add(add.asInstanceOf[I])
      if(x.isInstanceOf[VariableIdentifier])
        result=new NonRelationalHeapDomain(result._1.add(x.asInstanceOf[VariableIdentifier], newAdd), result._2, cod, dom)

      alreadyInitialized=alreadyInitialized+obj
      val c = typ.getPossibleFields;
      for(field <- c) {
        val adds = cod.convert(dom.createAddressForArgument(field.getType(), x.getProgramPoint))
        //I can ignore newHeap since it's equal to initial as it is not changed by getFieldIdentifier
        //in the same way I ignore rep
        val (fieldAdd, newHeap, rep)=result.getFieldIdentifier(obj, field.getName(), field.getType(), field.getProgramPoint())
        for(id : I <- fieldAdd.value) {
          result=new NonRelationalHeapDomain(result._1, result._2.add(id, adds), cod, dom)
          ids=ids+((id, path ::: (field.getName()) :: Nil))
          val r=initializeObject(id, id, id.getType, result, path ::: (field.getName()) :: Nil)
          alreadyInitialized=alreadyInitialized+id
          result=r._1
          ids=r._2++ids;//This order is quite important: in this way we keep the shortest path to arrive to an abstract node!
        }
      }
      (result, ids, new Replacement)
    }
    else (heap, Map.empty[Identifier, List[String]], new Replacement)
  }

  override def setArgument(variable : Assignable, expr : Expression) = this.assign(variable, expr, null)

  override def backwardAssign(variable : Assignable, expr : Expression) = (this, new Replacement)

  override def assignField(variable : Assignable, s : String, expr : Expression) : (NonRelationalHeapDomain[I], Replacement) = {
    var result=this.bottom()
    var rep=new Replacement()
    val ids = this.getFieldIdentifier(variable, s, expr.getType, variable.getProgramPoint())._1
    for(id <- ids.value) {
      val res = result.lubWithReplacement(result, this.assign(id, expr, null)._1)
      result = res._1
      rep = rep ++ res._2
    }
    (result, rep)
    //We ignore the other parts since getting a field does not modify a non relational heap domain
  }

  override def assign[S <: SemanticDomain[S]](variable : Assignable, expr : Expression, state : S) : (NonRelationalHeapDomain[I], Replacement) = {

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
        val result =
          if (x.representSingleVariable())
            this._2.add(x, this.normalize(value))
          else
            this._2.add(x, this.get(x).add(this.normalize(value)))

        return (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement);

      case x : HeapIdSetDomain[I] =>
        if(x.isTop)
          return (this.top(), new Replacement)
        var result=this._2
        val value=this.eval(expr)
        //TODO:Distinguish between definite and maybe
        for(addr <- x.value/*this.normalize(x).value*/)
          result=result.add(addr, value.lub(this.normalize(value), this._2.get(addr)))
        return (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement);

    }
  }

  override def setToTop(variable : Assignable) = variable match  {
    case x : VariableIdentifier => (new NonRelationalHeapDomain(this._1.add(x, cod.top()), this._2, cod, dom), new Replacement)
    case x : HeapIdSetDomain[I] =>
    var result=this._2
    for(addr <- x.value)
      result=result.add(addr, cod.top())
    (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement)
    case x : I =>
      (new NonRelationalHeapDomain(this._1, this._2.add(x, cod.top()), cod, dom), new Replacement)
  }

  override def removeVariable(variable : Assignable) = variable match  {
    case x : VariableIdentifier => (new NonRelationalHeapDomain(this._1.remove(x), this._2, cod, dom), new Replacement)
    case x : HeapIdSetDomain[I] =>
    var result=this._2
    for(addr <- x.value)
      result=result.remove(addr)
    (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement)
    case x : I =>
      (new NonRelationalHeapDomain(this._1,this._2.remove(x),cod,dom),new Replacement)
  }

  /**
   *
   * Implements object creation in non-relational heaps. If we create an object that already exists,
   * we convert the existing heap identifier to a summary node and return the result
   *
   * @param typ The type of the object to be created
   * @param pp  The point of the program that creates the reference
   * @return    The identifier of the created object and the state of the heap after that
   *
   */
  override def createObject(typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], NonRelationalHeapDomain[I], Replacement) = {
    makeSummaryIfRequired(dom.createAddress(typ, pp))
  }

  /**
   *
   * Merges the given replacement with the current heap state.
   *
   * @param rep The replacement
   * @return The heap state with the replacements performed
   *
   */
  def merge(rep:Replacement):NonRelationalHeapDomain[I] = {
    new NonRelationalHeapDomain(this._1.merge(rep),this._2.merge(rep),cod,dom)
  }

  override def getFieldIdentifier(heapIdentifier : Assignable, name : String, typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], NonRelationalHeapDomain[I], Replacement) = {
    (this.evalFieldAccess(heapIdentifier, name, typ), this, new Replacement)
  }

  def getSummaryCollectionIfExists(collection: Assignable): HeapIdSetDomain[I] = {
    def getSummaryCollection(a: Assignable): HeapIdSetDomain[I] = a match {
      case collection: I =>
        var ids:HeapIdSetDomain[I] = new MaybeHeapIdSetDomain[I]()
        if (getIds().contains(collection.toSummaryNode)) {
          ids = ids.add(collection.toSummaryNode)
        } else {
          ids = ids.add(collection)
        }

        ids
      case _ => throw new SemanticException("This is not a collection identifier " + a.toString())
    }

    resolveVariables(new MaybeHeapIdSetDomain[I](), collection, getSummaryCollection(_))
  }

  override def createEmptyCollection(collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint) : (HeapIdSetDomain[I], NonRelationalHeapDomain[I], Replacement) = {
    makeSummaryIfRequired(dom.createCollection(collTyp, keyTyp, valueTyp, lengthTyp, pp))
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

  override def getCollectionTupleByValue(valueId: Assignable) : Assignable = {
    return this.dom.getCollectionTupleByValue(valueId)
  }

  override def isSummaryCollection(collectionId: Assignable) : Boolean = {
    var isSummary = false

    def f(a:Assignable): HeapIdSetDomain[I] = a match{
      case collectionId: CollectionIdentifier =>
        isSummary = isSummary || ! collectionId.representSingleVariable()
        new MaybeHeapIdSetDomain[I]()
      case _ => throw new SemanticException("This is not a collection identifier " + collectionId.toString)
    }
    resolveVariables(new MaybeHeapIdSetDomain[I](), collectionId, f(_))

    return isSummary
  }

  override def insertCollectionElement(collectionApprox: Assignable, pp: ProgramPoint) = collectionApprox match{
    case FieldAndProgramPoint(x:CollectionIdentifier, "oA", _, _) =>
      val collectionApproxId = collectionApprox.asInstanceOf[I]
      val (tupleIds,res,rep) = makeSummaryIfRequired(dom.createCollectionTuple(collectionApproxId, x.keyTyp, x.valueTyp, pp))

      var result = this.factory()
      result.d1 = res.d1
      result.d2 = res.d2
      val newIds = result.heapEnv.get(collectionApproxId).add(tupleIds)
      val newHeapEnv = result.heapEnv.remove(collectionApproxId).add(collectionApproxId, newIds)
      result = new NonRelationalHeapDomain[I](result.variableEnv, newHeapEnv, result.cod, dom)

      (tupleIds, result, rep)

    case _ => throw new SemanticException("This is not a collection over approximation identifier " + collectionApprox)
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

  override def getCollectionTuples(collectionApprox: Assignable): HeapIdSetDomain[I] = collectionApprox match {
    case FieldAndProgramPoint(_, "oA", _, _) =>
      val collectionApproxId = collectionApprox.asInstanceOf[I]
      this.heapEnv.get(collectionApproxId)
    case _ => throw new SemanticException("This is not a collection over approximation identifier " + collectionApprox)
  }

  override def getCollectionOverApproximation(collection: Assignable): HeapIdSetDomain[I] = {
    def getApprox(a: Assignable): HeapIdSetDomain[I] = a match {
      case collectionId: CollectionIdentifier =>
        val id = this.dom.getCollectionOverApproximation(collectionId)
        new MaybeHeapIdSetDomain[I]().convert(id)
      case _ => throw new SemanticException("This is not a collection " + collection.toString)
    }

    resolveVariables(new MaybeHeapIdSetDomain[I](), collection, getApprox(_))
  }

  override def getCollectionUnderApproximation(collection: Assignable): HeapIdSetDomain[I] = {
    throw new SemanticException("Under approximation for this Heap not supported.")
  }

  override def getCollectionKeys(collectionApprox: Assignable, keyTyp: Type): HeapIdSetDomain[I] = collectionApprox match {
    case FieldAndProgramPoint(_, "oA", _, _) =>
      val collectionApproxId = collectionApprox.asInstanceOf[I]
      val tupleIds = this.heapEnv.get(collectionApproxId)
      def f(typ: Type)(a:Assignable) = new MaybeHeapIdSetDomain[I]().convert(dom.getCollectionKey(a, typ))
      HeapIdSetFunctionalLifting.applyToSetHeapId(tupleIds, tupleIds, f(keyTyp))
    case _ => throw new SemanticException("This is not a collection over approximation identifier " + collectionApprox)

  }

  override def getCollectionValues(collectionApprox: Assignable, valueTyp: Type): HeapIdSetDomain[I] = collectionApprox match {
    case FieldAndProgramPoint(_, "oA", _, _) =>
      val collectionApproxId = collectionApprox.asInstanceOf[I]
      val tupleIds = this.heapEnv.get(collectionApproxId)
      def f(typ: Type)(a:Assignable) = new MaybeHeapIdSetDomain[I]().convert(dom.getCollectionValue(a, typ))
      HeapIdSetFunctionalLifting.applyToSetHeapId(tupleIds, tupleIds, f(valueTyp))
    case _ => throw new SemanticException("This is not a collection over approximation identifier " + collectionApprox)
  }

  override def getCollectionLength(collection: Assignable) = {
    def f(a:Assignable):HeapIdSetDomain[I] = new MaybeHeapIdSetDomain().convert(dom.getCollectionLength(a))
    resolveVariables(new MaybeHeapIdSetDomain(), collection, f(_))
  }

  override def getUnreachableHeap:Set[I] = {
    ReachabilityAnalysis.getUnreachableLocations(d1,d2)
  }

  /** this subsumes reachability analysis! */
  override def optimizeSummaryNodes:(NonRelationalHeapDomain[I],Replacement) = {

    (this,new Replacement)

    if (env.isBottom) return (this,new Replacement)
    if (heap.isBottom) return (this,new Replacement)

    // Search for all nodes reachable via a single access path
    val visitedOnce = mutable.HashSet.empty[I]
    val visitedTwice = mutable.HashSet.empty[I] // We keep this to detect loops
    var toVisit = List.empty[I]
    var toExcludeVisit = List.empty[I]
    for (curs <- env.value.values) toVisit = toVisit ::: curs.value.toList
    while (!toVisit.isEmpty) {
      val cur = toVisit.head

      if (!visitedOnce.contains(cur)) visitedOnce += cur
      else { visitedTwice += cur }
      val reachableViaReferences = heap.get(cur).value
      val reachableViaFieldAccessEtc = heap.getIds.filter( _.getReachableFromIds.contains(cur) )
      val newSuccessors = reachableViaReferences ++ reachableViaFieldAccessEtc -- visitedTwice

      if (!cur.hasMultipleAccessPaths) {
        toVisit = toVisit.tail ++ newSuccessors
      } else {
        visitedOnce += cur
        visitedTwice += cur
        toExcludeVisit = toExcludeVisit ++ newSuccessors
        toVisit = toVisit.tail
      }
    }

    while (!toExcludeVisit.isEmpty) {
      val cur = toExcludeVisit.head
      visitedOnce += cur
      visitedTwice += cur
      val reachableViaReferences = heap.get(cur).value
      val reachableViaFieldAccessEtc = heap.getIds.filter( _.getReachableFromIds.contains(cur) )
      val newSuccessors = reachableViaReferences ++ reachableViaFieldAccessEtc -- visitedTwice
      toExcludeVisit = toExcludeVisit.tail ++ newSuccessors
    }

    // For all summary nodes visited exactly once, add a replacement of form (Summary -> NonSummary)
    val replacement = new Replacement()
    for (candidate <- visitedOnce -- visitedTwice) {
      val nonSummary = candidate.toNonSummaryNode
      if (!candidate.equals(nonSummary)) {
        replacement.value += (Set(candidate.asInstanceOf[Identifier]) -> Set(nonSummary.asInstanceOf[Identifier]))
      }
    }

    (this.merge(replacement),replacement)
  }

  private def makeNewIdOrReturnSummary(objectIdentifier:I) : (HeapIdSetDomain[I], NonRelationalHeapDomain[I], Replacement) = {

    // CASE 1) We already have a summary node for this, return the summary identifier
    if (getIds.contains(objectIdentifier.toSummaryNode)) {
      (cod.convert(objectIdentifier.toSummaryNode), this, new Replacement)
    }

    // CASE 2) We do not have a not, return the non-summary-identifier. Change counter so that it is new
    var cur = objectIdentifier
    while (getIds.contains(cur)) {
      cur = cur.setCounter(cur.getCounter + 1)
    }
    (cod.convert(cur), this, new Replacement)

  }

  private def makeSummaryIfRequired(objectIdentifier:I) : (HeapIdSetDomain[I], NonRelationalHeapDomain[I], Replacement) = {

    // CASE 1) We already have a summary node for this, return the summary identifier
    if (getIds.contains(objectIdentifier.toSummaryNode)) {

      (cod.convert(objectIdentifier.toSummaryNode), this, new Replacement)
    }

    // CASE 2) We do not have a not, return the non-summary-identifier
    else if (!getIds.contains(objectIdentifier)) {

      (cod.convert(objectIdentifier), this, new Replacement)

    }

    // CASE 3) We have such a node already - convert it to a summary node and return the summary node
    else {

      // create a summary node and replace the old object identifier with the summary node. Also return the
      // replacement so that it can be replaced in the semantic domain, too

      def collectReachableNodes(node:I): Set[I] = {
        val oldNodesCurrentLevel = getIds collect { case x:I if x.getReachableFromIds.contains(node) => x }
        var oldNodes = Set[I](node)
        for (n <- oldNodesCurrentLevel) {
          oldNodes = oldNodes ++ collectReachableNodes(n)
        }

        oldNodes
      }

      val oldNodes = collectReachableNodes(objectIdentifier)

      val replacementMap = oldNodes.map({ x:I => (Set(x.asInstanceOf[Identifier]),Set(x.toSummaryNode.asInstanceOf[Identifier]))}).toMap
      val replacement = new Replacement(new scala.collection.mutable.HashMap[Set[Identifier], Set[Identifier]]() ++ replacementMap)

      val createdObject = cod.convert(objectIdentifier.toSummaryNode)
      val newHeapDomain = this.merge(replacement)

      (createdObject, newHeapDomain, replacement)
    }
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
        result=result.lub(result, extractField(simplepp, field, typ))

      //If I don't have any information, I return a top identifier
      if(result.isBottom)
        return result.top()
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
    if(result.isBottom) return result.top()
    result
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
    //TODO: correct?
    case x : I => return cod.convert(x);
    case x => return cod.top();
  }

  override def assume(expr : Expression) =
    (this, new Replacement) //TODO: for now there is nothing about the heap structure
}

case class TopHeapIdentifier(typ2 : Type, pp2 : ProgramPoint) extends NonRelationalHeapIdentifier[TopHeapIdentifier](typ2, pp2) {

  override def getArrayCell(array : Assignable, index : Expression) = this
  override def getArrayLength(array : Assignable) = this
  override def getLabel() = "Top"
  override def getNullNode(pp : ProgramPoint) = this
  override def getField() : Option[String] = None
  override def isNormalized() : Boolean = true
  override def representSingleVariable()=false
  override def getName() = "#abstractReference#"
  override def equals(o : Any) = o match {
    case x : TopHeapIdentifier => true
    case _ => false
  }
  override def factory() = this
  override def createAddress(typ : Type, pp : ProgramPoint)=this
  override def createAddressForArgument(typ : Type, pp : ProgramPoint)=this
  override def extractField(obj : TopHeapIdentifier, field : String, typ : Type)=this
  override def accessStaticObject(typ : Type, pp : ProgramPoint)=this
  override def hashCode() : Int = 0
  override def toSummaryNode : TopHeapIdentifier = this
  override def toNonSummaryNode : TopHeapIdentifier = this
  override def getReachableFromIds : Set[TopHeapIdentifier] = Set.empty
  override def hasMultipleAccessPaths = false
  override def getCounter = 0
  override def setCounter(c:Int) = this

  def createCollection(collTyp: Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint) = this
  def getCollectionOverApproximation(collection: Assignable) = this
  def getCollectionUnderApproximation(collection: Assignable) = this
  def createCollectionTuple(collectionApprox:Assignable, keyTyp:Type, valueTyp:Type, pp:ProgramPoint) = this
  def createCollectionTuple(collectionApprox:Assignable, keyTyp:Type, valueTyp:Type, pps:Set[ProgramPoint]) = this
  def createCollectionTuple(collectionTuple1: Assignable, collectionTuple2: Assignable) = this
  def getCollectionTupleByKey(collectionKey: Assignable) = this
  def getCollectionTupleByValue(collectionValue: Assignable) = this
  def getCollectionLength(collection: Assignable) = this
  def getCollectionKey(collectionTuple:Assignable, keyTyp:Type) = this
  def getCollectionValue(collectionTuple:Assignable, valueTyp:Type) = this
}
