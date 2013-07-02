package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis


import ch.ethz.inf.pm.sample.oorepresentation.Type;
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.abstractdomain.{SemanticException, Expression, Assignable, HeapIdentifier}
;


class FieldHeapIdentifier(val obj : Type, val field : String, value : Type, pp : ProgramPoint) extends ClassHeapIdentifier(value, pp) {
  override def getLabel() = "Class";
  override def getField() = throw new Exception("Not supported")
  override def isNormalized() : Boolean = false;
  override def toString() : String = obj.getName()+"."+field;
  override def clone() : Object = new FieldHeapIdentifier(obj, field, value, pp);
  override def hashCode() : Int = field.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case a1 : FieldHeapIdentifier => this.obj.equals(a1.obj) && this.field.equals(a1.field);
    case _ => false;
  }
}

class ClassNullNodeHeapIdentifier(typ : Type, pp : ProgramPoint) extends ClassHeapIdentifier(typ, pp) {
  override def toString() : String = "null"
}

class ClassHeapIdentifier(val value : Type, pp : ProgramPoint) extends NonRelationalHeapIdentifier[ClassHeapIdentifier](value, pp) {
  override def getLabel() = "Class";
  override def getNullNode(p : ProgramPoint) = new ClassNullNodeHeapIdentifier(value.top(), p);
  override def getField() = throw new Exception("Not supported")
  override def isNormalized() : Boolean = true;
  override def factory() : ClassHeapIdentifier=new ClassHeapIdentifier(this.value, this.getProgramPoint);
  override def extractField(h : ClassHeapIdentifier, s : String, t : Type) : ClassHeapIdentifier=new FieldHeapIdentifier(h.value, s, t, h.getProgramPoint);
  override def accessStaticObject(t : Type, p : ProgramPoint) : ClassHeapIdentifier=new ClassHeapIdentifier(t, p);
  override def createAddress(t : Type, p : ProgramPoint) : ClassHeapIdentifier=new ClassHeapIdentifier(t, p);
  override def createAddressForArgument(t : Type, p : ProgramPoint) : ClassHeapIdentifier=	new ClassHeapIdentifier(t, p);
  override def representSingleVariable() : Boolean=false;
  override def getName() : String=value.getName();
  override def clone() : Object =new ClassHeapIdentifier(this.value, this.getProgramPoint);
  override def equals(a : Any) :  Boolean = a match {
    case a1 : FieldHeapIdentifier => false;
    case a1 : ClassHeapIdentifier => this.value.equals(a1.value);
    case _ => false;
  }
  override def getArrayCell(array : Assignable, index : Expression) = throw new SemanticException("Not yet supported")
  override def getArrayLength(array : Assignable) = throw new SemanticException("Not yet supported")
  override def hashCode() : Int = 1;
  override def toString() : String = return this.getName();
  override def toSummaryNode : ClassHeapIdentifier = this
  override def toNonSummaryNode : ClassHeapIdentifier = this
  override def getAssociatedIds : Set[ClassHeapIdentifier] = Set.empty

  override def createCollection(collTyp: Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint) = throw new SemanticException("Not yet supported")
  override def getCollectionOverApproximation(collection: Assignable) = throw new SemanticException("Not yet supported")
  override def getCollectionUnderApproximation(collection: Assignable) = throw new SemanticException("Not yet supported")
  override def createCollectionTuple(collectionApprox:Assignable, pp:ProgramPoint) = throw new SemanticException("Not yet supported")
  override def createCollectionTuple(collectionApprox:Assignable, pps:Set[ProgramPoint]) = throw new SemanticException("Not yet supported")
  override def createCollectionTuple(collectionTuple1: Assignable, collectionTuple2: Assignable) = throw new SemanticException("Not yet supported")
  override def getCollectionTupleByKey(collectionKey: Assignable) = throw new SemanticException("Not yet supported")
  override def getCollectionTupleByValue(collectionValue: Assignable) = throw new SemanticException("Not yet supported")
  override def getCollectionLength(collection: Assignable) = throw new SemanticException("Not yet supported")
  override def getCollectionKey(collectionTuple:Assignable, keyTyp:Type) = throw new SemanticException("Not yet supported")
  override def getCollectionValue(collectionTuple:Assignable, valueTyp:Type) = throw new SemanticException("Not yet supported")
}