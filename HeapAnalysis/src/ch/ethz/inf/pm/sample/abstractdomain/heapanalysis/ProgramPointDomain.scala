package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.SystemParameters
import scala.Some

object ParameterIds {
  var map = Map.empty[String, Int];
  def get(method : String) : Int = map.get(method) match {
    case None => map=map+((method, 1)); return 0;
    case Some(x) => map=map+((method, x+1)); return x;
  }
  def reset() : Unit = map=Map.empty[String, Int];
}

sealed abstract class ProgramPointHeapIdentifier(t : Type, pp1 : ProgramPoint) extends NonRelationalHeapIdentifier[ProgramPointHeapIdentifier](t, pp1) {


  override def getLabel() = "Program point";
  override def extractField(h : ProgramPointHeapIdentifier, s : String, t : Type) : ProgramPointHeapIdentifier=h match {
    case x : SimpleProgramPointHeapIdentifier => new FieldAndProgramPoint(x, s, t);
    case x : ParameterHeapIdentifier => new FieldAndProgramPoint(x, s, t);
    case x : StaticProgramPointHeapIdentifier => new FieldAndProgramPoint(x, s, t);
    case x : UnsoundParameterHeapIdentifier => new FieldAndProgramPoint(x, s, t);
    case x : CollectionIdentifier => new FieldAndProgramPoint(x, s, t);
    case x : CollectionTupleIdentifier => new FieldAndProgramPoint(x, s, t);
    case _ => throw new SemanticException("Not allowed " + h.toString);
  }
  override def accessStaticObject(t : Type, pp : ProgramPoint) : ProgramPointHeapIdentifier=new StaticProgramPointHeapIdentifier(t, pp);
  override def createAddress(t : Type, p : ProgramPoint) : ProgramPointHeapIdentifier=new SimpleProgramPointHeapIdentifier(p, t);
  override def createAddressForArgument(t : Type, p : ProgramPoint) : ProgramPointHeapIdentifier=
    if(NonRelationalHeapDomainSettings.unsoundEntryState)
      new UnsoundParameterHeapIdentifier(t, Math.min(ParameterIds.get(SystemParameters.currentMethod), NonRelationalHeapDomainSettings.maxInitialNodes), p);
    else new ParameterHeapIdentifier(t, p);
  override def hashCode() : Int = 1;
  override def getNullNode(p : ProgramPoint) = new NullProgramPointHeapIdentifier(t.top(), p);
  override def getName() : String=this.toString();
  override def isNormalized() : Boolean = true;
  override def getArrayCell(array : Assignable, index : Expression) = new ArrayTopIdentifier();
  override def getArrayLength(array : Assignable) = new ArrayTopIdentifier()

  override def createCollection(collTyp: Type, keyTyp:Type, valueTyp:Type, lengthTyp: Type, pp:ProgramPoint) =
    new CollectionIdentifier(pp, collTyp, keyTyp, valueTyp, lengthTyp)

  override def getCollectionOverApproximation(collection: Assignable) = collection match {
    case x:CollectionIdentifier => extractField(x, "overApproximation", x.collTyp)
    case _ => throw new SemanticException("This is not a collection: " + collection.toString)
  }

  override def getCollectionUnderApproximation(collection: Assignable) = collection match {
    case x:CollectionIdentifier => extractField(x, "underApproximation", x.collTyp)
    case _ => throw new SemanticException("This is not a collection: " + collection.toString)
  }

  override def createCollectionTuple(collectionApprox:Assignable, pp:ProgramPoint) = {
    val pps = Set.empty[ProgramPoint] + pp
    createCollectionTuple(collectionApprox, pps)
  }

  override def createCollectionTuple(collectionApprox: Assignable, pps: Set[ProgramPoint]) = collectionApprox match {
    case x:ProgramPointHeapIdentifier => new CollectionTupleIdentifier(x, pps)
    case _ => throw new SemanticException("This is not a collection approximation node: " + collectionApprox.toString)
  }

  def createCollectionTuple(collectionTuple1: Assignable, collectionTuple2: Assignable) = collectionTuple1 match {
    case x: CollectionTupleIdentifier => collectionTuple2 match {
      case y: CollectionTupleIdentifier =>
        if (!x.collectionApprox.equals(y.collectionApprox))
          throw new SemanticException("Tuples do not have the same collection approximation: " + collectionTuple1.toString + ", " + collectionTuple2.toString)

        new CollectionTupleIdentifier(x.collectionApprox, x.pps ++ y.pps)

      case _ => throw new SemanticException("This is not a collection tuple: " + collectionTuple2.toString)
    }
    case _ => throw new SemanticException("This is not a collection tuple: " + collectionTuple1.toString)
  }

  override def getCollectionLength(collection: Assignable) = collection match {
    case x:CollectionIdentifier => extractField(x, "length", x.lengthTyp)
    case _ => throw new SemanticException("This is not a collection: "+collection.toString)
  }

  override def getCollectionTupleByKey(collectionKey: Assignable) = collectionKey match {
    case FieldAndProgramPoint(tuple, _, _) => tuple
    case _ => throw new SemanticException("This is not a collection key: " + collectionKey.toString)
  }

  override def getCollectionTupleByValue(collectionValue: Assignable) = collectionValue match {
    case FieldAndProgramPoint(tuple, _, _) => tuple
    case _ => throw new SemanticException("This is not a collection value: " + collectionValue.toString)
  }

  override def getCollectionKey(collectionTuple:Assignable, keyTyp:Type) = collectionTuple match {
    case x:ProgramPointHeapIdentifier => extractField(x, "key", keyTyp)
    case _ => throw new SemanticException("This is not a program point identifier: " + collectionTuple.toString)
  }

  override def getCollectionValue(collectionTuple:Assignable, valueTyp:Type) = collectionTuple match {
    case x:ProgramPointHeapIdentifier => extractField(x, "value", valueTyp)
    case _ => throw new SemanticException("This is not a collection tuple: " + collectionTuple.toString)
  }
}

case class NullProgramPointHeapIdentifier(t2 : Type, pp1 : ProgramPoint) extends ProgramPointHeapIdentifier(t2, pp1) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case NullProgramPointHeapIdentifier(t3, pp) => return true
    case _ => return false
  }
  override def toString() : String = "null"

  override def factory() : ProgramPointHeapIdentifier=new NullProgramPointHeapIdentifier(getType(), this.getProgramPoint);
  override def representSingleVariable() : Boolean=true;
  override def clone() : Object =new NullProgramPointHeapIdentifier(t2, this.getProgramPoint);
  override def toSummaryNode : ProgramPointHeapIdentifier = this
  override def toNonSummaryNode : ProgramPointHeapIdentifier = this
}

case class SimpleProgramPointHeapIdentifier(val pp1 : ProgramPoint, t2 : Type, summary:Boolean = false) extends ProgramPointHeapIdentifier(t2, pp1) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case SimpleProgramPointHeapIdentifier(pp, t3, summaryX) => return this.pp.equals(pp) && summaryX == summary
    case _ => return false
  }
  override def toString() : String = pp.toString()

  override def factory() : ProgramPointHeapIdentifier=new SimpleProgramPointHeapIdentifier(this.pp, this.getType());
  override def representSingleVariable() : Boolean= !summary
  override def clone() : Object =new SimpleProgramPointHeapIdentifier(pp, this.getType());
  override def toSummaryNode : ProgramPointHeapIdentifier=new SimpleProgramPointHeapIdentifier(this.pp, this.getType(), true);
  override def toNonSummaryNode : ProgramPointHeapIdentifier=new SimpleProgramPointHeapIdentifier(this.pp, this.getType(), false);
}

case class CollectionIdentifier(override val pp:ProgramPoint, collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, summary:Boolean = false) extends ProgramPointHeapIdentifier(collTyp, pp) {
  def getField() : Option[String] = None
  override def isNormalized() : Boolean = true
  override def equals(x : Any) : Boolean = x match {
    case CollectionIdentifier(ppX, collTypX, keyTypX, valueTypX, lengthTypX, summaryX) => (pp == ppX && collTyp == collTypX &&
      keyTyp == keyTypX && valueTyp == valueTypX && lengthTyp == lengthTypX && summary == summaryX)
    case _ => false
  }
  override def toString() : String = "Collection("+collTyp.toString+","+pp.toString+")"
  override def factory() : ProgramPointHeapIdentifier = new CollectionIdentifier(pp,collTyp, keyTyp, valueTyp, lengthTyp)
  override def representSingleVariable() : Boolean= !summary
  override def clone() : Object = new CollectionIdentifier(pp,collTyp, keyTyp, valueTyp, lengthTyp)
  override def toSummaryNode : ProgramPointHeapIdentifier = new CollectionIdentifier(pp,collTyp, keyTyp, valueTyp, lengthTyp, true)
  override def toNonSummaryNode : ProgramPointHeapIdentifier = new CollectionIdentifier(pp,collTyp, keyTyp, valueTyp, lengthTyp, false)
}

case class CollectionTupleIdentifier(collectionApprox:ProgramPointHeapIdentifier, pps: Set[ProgramPoint], summary:Boolean = false) extends ProgramPointHeapIdentifier(collectionApprox.getType(), pps.head) {
  def getField() : Option[String] = None
  override def isNormalized() : Boolean = false
  override def equals(x : Any) : Boolean = x match {
    case CollectionTupleIdentifier(collectionApproxX, ppsX, s) =>
      if (this.summary != s) return false
      var areEqual = this.collectionApprox.equals(collectionApproxX)
      areEqual = areEqual && pps.size == ppsX.size
      for (pp <- pps) {
        areEqual = areEqual && ppsX.contains(pp)
      }
      areEqual
    case _ => false
  }
  override def toString() : String = "Tup(" + this.collectionApprox.pp + ", " + this.pps.mkString(",") + ")"
  override def factory() : ProgramPointHeapIdentifier = new CollectionTupleIdentifier(this.collectionApprox, this.pps)
  override def representSingleVariable() : Boolean= !summary
  override def clone() : Object = new CollectionTupleIdentifier(this.collectionApprox, this.pps)
  override def toSummaryNode : ProgramPointHeapIdentifier = new CollectionTupleIdentifier(this.collectionApprox, this.pps, true)
  override def toNonSummaryNode : ProgramPointHeapIdentifier = new CollectionTupleIdentifier(this.collectionApprox, this.pps, false)
}

case class ArrayTopIdentifier() extends ProgramPointHeapIdentifier(null, null) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case ArrayTopIdentifier() => true
    case _ => false
  }
  override def toString() : String = "Array ID"

  override def factory() : ProgramPointHeapIdentifier=new ArrayTopIdentifier();
  override def representSingleVariable() : Boolean=false;
  override def clone() : Object =new ArrayTopIdentifier();
  override def toSummaryNode : ProgramPointHeapIdentifier = this
  override def toNonSummaryNode : ProgramPointHeapIdentifier = this
}

case class ParameterHeapIdentifier(t2 : Type, pp1 : ProgramPoint, summary:Boolean = false) extends ProgramPointHeapIdentifier(t2, pp1) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case ParameterHeapIdentifier(t3, pp, s) => this.t2.equals(t3) && this.summary == s
    case _ => false
  }
  override def representSingleVariable() : Boolean= !summary
  override def factory() : ProgramPointHeapIdentifier=new ParameterHeapIdentifier(this.getType(), this.getProgramPoint);
  override def toString() : String = "Parameter of type "+this.getType()+""
  override def toSummaryNode : ProgramPointHeapIdentifier=new ParameterHeapIdentifier(this.getType(), this.getProgramPoint, true)
  override def toNonSummaryNode : ProgramPointHeapIdentifier=new ParameterHeapIdentifier(this.getType(), this.getProgramPoint, false)
}

case class UnsoundParameterHeapIdentifier(t2 : Type, n : Int, pp1 : ProgramPoint) extends ProgramPointHeapIdentifier(t2, pp1) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case UnsoundParameterHeapIdentifier(t3, n2, pp) => return this.t2.equals(t3) && this.n ==n2;
    case _ => return false
  }
  override def representSingleVariable() : Boolean= this.n!=NonRelationalHeapDomainSettings.maxInitialNodes
  override def factory() : ProgramPointHeapIdentifier=new UnsoundParameterHeapIdentifier(this.getType(), this.n, this.getProgramPoint);
  override def toString() : String = "Unsound parameter of type "+this.getType()+" number "+this.n;
  override def toSummaryNode : ProgramPointHeapIdentifier = this
  override def toNonSummaryNode : ProgramPointHeapIdentifier = this
}

case class StaticProgramPointHeapIdentifier(t2 : Type, pp1 : ProgramPoint) extends ProgramPointHeapIdentifier(t2, pp1) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case StaticProgramPointHeapIdentifier(t2, pp) => return this.getType().equals(t2);
    case _ => return false
  }
  override def representSingleVariable() : Boolean = true
  override def factory() : ProgramPointHeapIdentifier= new StaticProgramPointHeapIdentifier(this.getType(), this.getProgramPoint);
  override def toString() : String = "Static "+this.getType()+""
  override def toSummaryNode : ProgramPointHeapIdentifier = this
  override def toNonSummaryNode : ProgramPointHeapIdentifier = this
}

case class FieldAndProgramPoint(val p1 : ProgramPointHeapIdentifier, val field : String, t2 : Type) extends ProgramPointHeapIdentifier(t2, p1.getProgramPoint) {
  def getField() : Option[String] = Some(field);
  override def isNormalized() : Boolean = false;
  override def equals(x : Any) : Boolean = x match {
    case FieldAndProgramPoint(pp1, field1, t2) => return this.p1.equals(pp1) && this.field.equals(field1);
    case _ => return false
  }
  override def representSingleVariable() : Boolean=p1.representSingleVariable();
  override def factory() : ProgramPointHeapIdentifier=new FieldAndProgramPoint(this.p1, this.field, this.getType());
  override def toString() : String = "("+p1.toString()+", "+field+")"
  override def toSummaryNode : ProgramPointHeapIdentifier = this
  override def toNonSummaryNode : ProgramPointHeapIdentifier = this
}