package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.SystemParameters
import scala.Some

object PPDSettings {

  val printSummary = false

}


object ParameterIds {
  var map = Map.empty[String, Int]

  def get(method: String): Int = map.get(method) match {
    case None => map = map + ((method, 1)); 0
    case Some(x) => map = map + ((method, x + 1)); x
  }

  def reset() {
    map = Map.empty[String, Int]
  }
}

sealed abstract class ProgramPointHeapIdentifier(t: Type, pp1: ProgramPoint, counter: Int = 0) extends NonRelationalHeapIdentifier[ProgramPointHeapIdentifier](t, pp1) {


  override def getLabel() = "Program point"

  override def extractField(h: ProgramPointHeapIdentifier, s: String, t: Type): ProgramPointHeapIdentifier = h match {
    case x: SimpleProgramPointHeapIdentifier => new FieldAndProgramPoint(x, s, t)
    case x: ParameterHeapIdentifier => new FieldAndProgramPoint(x, s, t)
    case x: StaticProgramPointHeapIdentifier => new FieldAndProgramPoint(x, s, t)
    case x: UnsoundParameterHeapIdentifier => new FieldAndProgramPoint(x, s, t)
    case x: CollectionIdentifier => new FieldAndProgramPoint(x, s, t)
    case x: CollectionTupleIdentifier => new FieldAndProgramPoint(x, s, t)
    case _ => throw new SemanticException("Not allowed " + h.toString)
  }

  override def accessStaticObject(t: Type, pp: ProgramPoint): ProgramPointHeapIdentifier = new StaticProgramPointHeapIdentifier(t, pp)

  override def createAddress(t: Type, p: ProgramPoint): ProgramPointHeapIdentifier = new SimpleProgramPointHeapIdentifier(p, t)

  override def createAddressForArgument(t: Type, p: ProgramPoint): ProgramPointHeapIdentifier =
    if (NonRelationalHeapDomainSettings.unsoundEntryState)
      new UnsoundParameterHeapIdentifier(t, Math.min(ParameterIds.get(SystemParameters.currentMethod), NonRelationalHeapDomainSettings.maxInitialNodes), p)
    else new ParameterHeapIdentifier(t, p)

  override def hashCode(): Int = pp1.hashCode() + counter

  override def getNullNode(p: ProgramPoint) = new NullProgramPointHeapIdentifier(t.top(), p)

  override def getName(): String = this.toString

  override def isNormalized(): Boolean = true

  override def getArrayCell(array: Assignable, index: Expression) = new ArrayTopIdentifier()

  override def getArrayLength(array: Assignable) = new ArrayTopIdentifier()

  override def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, pp: ProgramPoint) =
    new CollectionIdentifier(pp, collTyp, keyTyp, valueTyp, lengthTyp)

  override def getCollectionOverApproximation(collection: Assignable) = collection match {
    case x: CollectionIdentifier => extractField(x, "oA", x.collTyp)
    case _ => throw new SemanticException("This is not a collection: " + collection.toString)
  }

  override def getCollectionUnderApproximation(collection: Assignable) = collection match {
    case x: CollectionIdentifier => extractField(x, "uA", x.collTyp)
    case _ => throw new SemanticException("This is not a collection: " + collection.toString)
  }

  override def createCollectionTuple(collectionApprox:Assignable, keyTyp: Type, valueTyp: Type, pp:ProgramPoint) = {
    val pps = Set.empty[ProgramPoint] + pp
    createCollectionTuple(collectionApprox, keyTyp, valueTyp, pps)
  }

  override def createCollectionTuple(collectionApprox: Assignable, keyTyp: Type, valueTyp:Type, pps: Set[ProgramPoint]) = collectionApprox match {
    case x:ProgramPointHeapIdentifier => new CollectionTupleIdentifier(x, keyTyp, valueTyp, Set(new DummyProgramPoint))    // TODO: Revert pps
    case _ => throw new SemanticException("This is not a collection approximation node: " + collectionApprox.toString)
  }

  override def createCollectionTuple(collectionTuple1: Assignable, collectionTuple2: Assignable) = collectionTuple1 match {
    case x: CollectionTupleIdentifier => collectionTuple2 match {
      case y: CollectionTupleIdentifier =>
        if (!x.collectionApprox.equals(y.collectionApprox))
          throw new SemanticException("Tuples do not have the same collection approximation: " + collectionTuple1.toString + ", " + collectionTuple2.toString)

        new CollectionTupleIdentifier(x.collectionApprox, x.keyTyp, x.valueTyp, x.pps ++ y.pps, true)

      case _ => throw new SemanticException("This is not a collection tuple: " + collectionTuple2.toString)
    }
    case _ => throw new SemanticException("This is not a collection tuple: " + collectionTuple1.toString)
  }

  override def getCollectionLength(collection: Assignable) = collection match {
    case x: CollectionIdentifier => extractField(x, "length", x.lengthTyp)
    case _ => throw new SemanticException("This is not a collection: " + collection.toString)
  }

  override def getCollectionTupleByKey(collectionKey: Assignable) = collectionKey match {
    case FieldAndProgramPoint(tuple, _, _, _) => tuple
    case _ => throw new SemanticException("This is not a collection key: " + collectionKey.toString)
  }

  override def getCollectionTupleByValue(collectionValue: Assignable) = collectionValue match {
    case FieldAndProgramPoint(tuple, _, _, _) => tuple
    case _ => throw new SemanticException("This is not a collection value: " + collectionValue.toString)
  }

  override def getCollectionKey(collectionTuple: Assignable, keyTyp: Type) = collectionTuple match {
    case x: ProgramPointHeapIdentifier => extractField(x, "key", keyTyp)
    case _ => throw new SemanticException("This is not a program point identifier: " + collectionTuple.toString)
  }

  override def getCollectionValue(collectionTuple: Assignable, valueTyp: Type) = collectionTuple match {
    case x: ProgramPointHeapIdentifier => extractField(x, "value", valueTyp)
    case _ => throw new SemanticException("This is not a collection tuple: " + collectionTuple.toString)
  }

  override def hasMultipleAccessPaths = false
  override def getCounter = counter

}

case class NullProgramPointHeapIdentifier(t2: Type, pp1: ProgramPoint, cnt: Int = 0) extends ProgramPointHeapIdentifier(t2, pp1, cnt) {
  def getField(): Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case NullProgramPointHeapIdentifier(t3, _, c) => cnt == c
    case _ => false
  }

  override def toString: String = "null"

  override def factory(): ProgramPointHeapIdentifier = new NullProgramPointHeapIdentifier(getType(), this.getProgramPoint())

  override def representSingleVariable(): Boolean = true

  override def clone(): Object = new NullProgramPointHeapIdentifier(t2, this.getProgramPoint())

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) = new NullProgramPointHeapIdentifier(t2, pp1, c)
}

case class SimpleProgramPointHeapIdentifier(pp1: ProgramPoint, t2: Type, summary: Boolean = false, cnt: Int = 0) extends ProgramPointHeapIdentifier(t2, pp1, cnt) {
  def getField(): Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case SimpleProgramPointHeapIdentifier(ppX, _, summaryX, c) => this.pp.equals(ppX) && summaryX == summary && cnt == c
    case _ => false
  }

  override def toString: String = pp.toString + (if (PPDSettings.printSummary && summary) "Σ" else "")

  override def factory(): ProgramPointHeapIdentifier = new SimpleProgramPointHeapIdentifier(this.pp, this.getType())

  override def representSingleVariable(): Boolean = !summary

  override def clone(): Object = new SimpleProgramPointHeapIdentifier(pp, this.getType())

  override def toSummaryNode: ProgramPointHeapIdentifier = new SimpleProgramPointHeapIdentifier(this.pp, this.getType(), true)

  override def toNonSummaryNode: ProgramPointHeapIdentifier = new SimpleProgramPointHeapIdentifier(this.pp, this.getType(), false)

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) = new SimpleProgramPointHeapIdentifier(pp1, t2, summary, c)
}

case class CollectionIdentifier(override val pp: ProgramPoint, collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, summary: Boolean = false, cnt: Int = 0) extends ProgramPointHeapIdentifier(collTyp, pp, cnt) {
  def getField(): Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case CollectionIdentifier(ppX, collTypX, keyTypX, valueTypX, lengthTypX, summaryX, c) => pp == ppX && collTyp == collTypX && cnt == c &
      keyTyp == keyTypX && valueTyp == valueTypX && lengthTyp == lengthTypX && summary == summaryX
    case _ => false
  }

  override def toString: String = "Collection(" + collTyp.toString + "," + pp.toString + ")" + (if (PPDSettings.printSummary && summary) "Σ" else "")

  override def factory(): ProgramPointHeapIdentifier = new CollectionIdentifier(pp, collTyp, keyTyp, valueTyp, lengthTyp)

  override def representSingleVariable(): Boolean = !summary

  override def clone(): Object = new CollectionIdentifier(pp, collTyp, keyTyp, valueTyp, lengthTyp)

  override def toSummaryNode: ProgramPointHeapIdentifier = new CollectionIdentifier(pp, collTyp, keyTyp, valueTyp, lengthTyp, true)

  override def toNonSummaryNode: ProgramPointHeapIdentifier = new CollectionIdentifier(pp, collTyp, keyTyp, valueTyp, lengthTyp, false)

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) = new CollectionIdentifier(pp, collTyp, keyTyp, valueTyp, lengthTyp, summary, c)
}

case class CollectionTupleIdentifier(collectionApprox:ProgramPointHeapIdentifier, keyTyp:Type, valueTyp:Type, pps: Set[ProgramPoint], summary: Boolean = false, cnt: Int = 0) extends ProgramPointHeapIdentifier(collectionApprox.getType(), pps.head, cnt) {
  def getField() : Option[String] = None
  override def isNormalized() : Boolean = false

  override def equals(x : Any) : Boolean = x match {
    case CollectionTupleIdentifier(collectionApproxX, keyTypX, valueTypX, ppsX, summaryX, c) =>
      if (!this.collectionApprox.equals(collectionApproxX)) return false
      if (c != cnt) return false
      if (!this.keyTyp.equals(keyTypX)) return false
      if (!this.valueTyp.equals(valueTypX)) return false
      if (pps.size != ppsX.size) return false
      if (this.summary != summaryX) return false
      for (pp <- pps) {
        if(!ppsX.contains(pp)) return false
      }
      true
    case _ => false
  }

  override def toString() : String = {
    val approxType = this.collectionApprox match {
      case FieldAndProgramPoint(_, x, _, _) => x
    }

    "T(" + approxType + "," + this.collectionApprox.pp + ", " + this.pps.mkString(",") + ")" + (if (PPDSettings.printSummary && !representSingleVariable()) "Σ" else "")
  }

  override def factory() : ProgramPointHeapIdentifier = new CollectionTupleIdentifier(this.collectionApprox, this.keyTyp, this.valueTyp, this.pps, this.summary)
  override def representSingleVariable() : Boolean = !summary
  override def clone() : Object = new CollectionTupleIdentifier(this.collectionApprox, this.keyTyp, this.valueTyp, this.pps, this.summary)
  override def toSummaryNode : ProgramPointHeapIdentifier = new CollectionTupleIdentifier(this.collectionApprox.toSummaryNode, this.keyTyp, this.valueTyp, this.pps, true)
  override def toNonSummaryNode : ProgramPointHeapIdentifier = new CollectionTupleIdentifier(this.collectionApprox.toNonSummaryNode, this.keyTyp, this.valueTyp, this.pps, false)
  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set(collectionApprox)

  // TODO: This can be more precise; If the key is not a summary node, then this could return false.
  override def hasMultipleAccessPaths = true

  override def setCounter(c:Int) =  new CollectionTupleIdentifier(this.collectionApprox, this.keyTyp, this.valueTyp, this.pps, this.summary, c)
}

case class ArrayTopIdentifier(cnt: Int = 0) extends ProgramPointHeapIdentifier(null, null, cnt) {
  def getField(): Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case ArrayTopIdentifier(c) => c == cnt
    case _ => false
  }

  override def toString: String = "Array ID"

  override def factory(): ProgramPointHeapIdentifier = new ArrayTopIdentifier()

  override def representSingleVariable(): Boolean = false

  override def clone(): Object = new ArrayTopIdentifier()

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) =  new ArrayTopIdentifier(c)
}

case class ParameterHeapIdentifier(t2: Type, pp1: ProgramPoint, summary: Boolean = false, cnt: Int = 0) extends ProgramPointHeapIdentifier(t2, pp1, cnt) {
  def getField(): Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case ParameterHeapIdentifier(t3, _, s, c) => this.t2.equals(t3) && this.summary == s && c == cnt
    case _ => false
  }

  override def representSingleVariable(): Boolean = !summary

  override def factory(): ProgramPointHeapIdentifier = new ParameterHeapIdentifier(this.getType(), this.getProgramPoint())

  override def toString: String = "Parameter of type " + this.getType() + (if (PPDSettings.printSummary && summary) "Σ" else "")

  override def toSummaryNode: ProgramPointHeapIdentifier = new ParameterHeapIdentifier(this.getType(), this.getProgramPoint(), true)

  override def toNonSummaryNode: ProgramPointHeapIdentifier = new ParameterHeapIdentifier(this.getType(), this.getProgramPoint(), false)

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) =  new ParameterHeapIdentifier(t2,pp1,summary,c)
}

case class UnsoundParameterHeapIdentifier(t2: Type, n: Int, pp1: ProgramPoint, cnt: Int = 0) extends ProgramPointHeapIdentifier(t2, pp1, cnt) {
  def getField(): Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case UnsoundParameterHeapIdentifier(t3, n2, _, _) => this.t2.equals(t3) && this.n == n2
    case _ => false
  }

  override def representSingleVariable(): Boolean = this.n != NonRelationalHeapDomainSettings.maxInitialNodes

  override def factory(): ProgramPointHeapIdentifier = new UnsoundParameterHeapIdentifier(this.getType(), this.n, this.getProgramPoint())

  override def toString: String = "Unsound parameter of type " + this.getType() + " number " + this.n

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) =  new UnsoundParameterHeapIdentifier(t2,n,pp1,c)
}

case class StaticProgramPointHeapIdentifier(t2: Type, pp1: ProgramPoint, cnt: Int = 0) extends ProgramPointHeapIdentifier(t2, pp1, cnt) {
  def getField(): Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case StaticProgramPointHeapIdentifier(t3, _, c) => this.getType().equals(t3) && c == cnt
    case _ => false
  }

  override def representSingleVariable(): Boolean = true

  override def factory(): ProgramPointHeapIdentifier = new StaticProgramPointHeapIdentifier(this.getType(), this.getProgramPoint())

  override def toString: String = "Static " + this.getType() + ""

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) =  new StaticProgramPointHeapIdentifier(t2,pp1,c)
}

case class FieldAndProgramPoint(p1: ProgramPointHeapIdentifier, field: String, t2: Type, cnt: Int = 0) extends ProgramPointHeapIdentifier(t2, p1.getProgramPoint(), cnt) {
  def getField(): Option[String] = Some(field)

  override def isNormalized(): Boolean = false

  override def equals(x: Any): Boolean = x match {
    case FieldAndProgramPoint(pp1, field1, _, c) => this.p1.equals(pp1) && this.field.equals(field1) && c == cnt
    case _ => false
  }

  override def representSingleVariable(): Boolean = p1.representSingleVariable()

  override def factory(): ProgramPointHeapIdentifier = new FieldAndProgramPoint(this.p1, this.field, this.getType())

  override def toString: String = "(" + p1.toString + ", " + field + ")"

  override def toSummaryNode: ProgramPointHeapIdentifier = new FieldAndProgramPoint(this.p1.toSummaryNode, this.field, this.getType())

  override def toNonSummaryNode: ProgramPointHeapIdentifier = new FieldAndProgramPoint(this.p1.toNonSummaryNode, this.field, this.getType())

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set(p1)

  override def setCounter(c:Int) =  new FieldAndProgramPoint(p1,field,t2,c)
}