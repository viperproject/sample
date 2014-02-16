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

sealed trait ProgramPointHeapIdentifier
  extends NonRelationalHeapIdentifier[ProgramPointHeapIdentifier] {

  def counter: Int

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
    if (NonRelationalHeapDomainSettings.unsoundEntryState) {
      val context = SystemParameters.analysisUnitContext
      new UnsoundParameterHeapIdentifier(t, Math.min(ParameterIds.get(context.methodName), NonRelationalHeapDomainSettings.maxInitialNodes), p)
    }
    else new ParameterHeapIdentifier(t, p)

  override def hashCode(): Int = pp.hashCode() + counter

  override def getNullNode(p: ProgramPoint) = new NullProgramPointHeapIdentifier(typ.top(), p)

  override def getName: String = this.toString

  override def isNormalized(): Boolean = true

  override def getArrayCell(array: Assignable, index: Expression) = new ArrayTopIdentifier()

  override def getArrayLength(array: Assignable) = new ArrayTopIdentifier()

  override def createCollection(collTyp: Type, keyTyp:Type, valueTyp:Type, lengthTyp: Type, origCollectionTyp: Option[Type], keyCollectionTyp: Option[Type], pp:ProgramPoint) =
    new CollectionIdentifier(pp, collTyp, keyTyp, valueTyp, lengthTyp, origCollectionTyp, keyCollectionTyp)

  override def getCollectionOverApproximation(collection: Assignable) = collection match {
    case x:CollectionIdentifier => extractField(x, "may", x.collTyp)
    case _ => throw new SemanticException("This is not a collection: " + collection.toString)
  }

  override def getCollectionUnderApproximation(collection: Assignable) = collection match {
    case x:CollectionIdentifier => extractField(x, "must", x.collTyp)
    case _ => throw new SemanticException("This is not a collection: " + collection.toString)
  }

  override def getCollectionSummaryApproximation(collection:Assignable) = collection match {
    case x:CollectionIdentifier => extractField(x, "summary", x.collTyp)
    case _ => throw new SemanticException("This is not a collection: " + collection.toString)
  }

  def createCollectionSummaryTuple(collectionApprox:Assignable, keyTyp:Type, valueTyp:Type) = {
    val pps = Set[ProgramPoint](collectionApprox.pp)
    createCollectionTuple(collectionApprox, keyTyp, valueTyp, pps)
  }

  override def createCollectionTuple(collectionApprox:Assignable, keyTyp: Type, valueTyp: Type, pp:ProgramPoint) = {
    val pps = Set[ProgramPoint](pp)
    createCollectionTuple(collectionApprox, keyTyp, valueTyp, pps)
  }

  override def createCollectionTuple(collectionApprox: Assignable, keyTyp: Type, valueTyp:Type, pps: Set[ProgramPoint]) = collectionApprox match {
    case x:ProgramPointHeapIdentifier => new CollectionTupleIdentifier(x, keyTyp, valueTyp, pps)
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

  override def getCollectionKey(collectionTuple:Assignable) = collectionTuple match {
    case x:CollectionTupleIdentifier => extractField(x, "key", x.keyTyp)
    case _ => throw new SemanticException("This is not a program point identifier: " + collectionTuple.toString)
  }

  override def getCollectionValue(collectionTuple:Assignable) = collectionTuple match {
    case x:CollectionTupleIdentifier => extractField(x, "value", x.valueTyp)
    case _ => throw new SemanticException("This is not a collection tuple: " + collectionTuple.toString)
  }

  def createNonDeterminismSource(typ: Type, pp: ProgramPoint, multiple: Boolean): ProgramPointHeapIdentifier = {
    NonDeterminismSourceHeapId(typ, pp, multiple)
  }

  override def hasMultipleAccessPaths = false
  override def getCounter = counter

}

case class NullProgramPointHeapIdentifier(typ: Type, pp: ProgramPoint, counter: Int = 0)
  extends ProgramPointHeapIdentifier {

  def getField: Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case NullProgramPointHeapIdentifier(t3, _, c) => counter == c
    case _ => false
  }

  override def toString: String = "null"

  override def factory(): ProgramPointHeapIdentifier = new NullProgramPointHeapIdentifier(getType, this.pp)

  override def representsSingleVariable(): Boolean = true

  override def clone(): Object = new NullProgramPointHeapIdentifier(typ, this.pp)

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) = new NullProgramPointHeapIdentifier(typ, pp, c)
}

case class SimpleProgramPointHeapIdentifier(
    pp: ProgramPoint,
    typ: Type,
    summary: Boolean = false,
    counter: Int = 0)
  extends ProgramPointHeapIdentifier {

  def getField: Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case SimpleProgramPointHeapIdentifier(ppX, _, summaryX, c) => this.pp.equals(ppX) && summaryX == summary && counter == c
    case _ => false
  }

  override def toString: String = pp.toString + (if (PPDSettings.printSummary && summary) "Σ" else "")

  override def factory(): ProgramPointHeapIdentifier = new SimpleProgramPointHeapIdentifier(this.pp, this.getType)

  override def representsSingleVariable(): Boolean = !summary

  override def clone(): Object = new SimpleProgramPointHeapIdentifier(pp, this.getType)

  override def toSummaryNode: ProgramPointHeapIdentifier = new SimpleProgramPointHeapIdentifier(this.pp, this.getType, true)

  override def toNonSummaryNode: ProgramPointHeapIdentifier = new SimpleProgramPointHeapIdentifier(this.pp, this.getType, false)

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) = new SimpleProgramPointHeapIdentifier(pp, typ, summary, c)
}

case class NonDeterminismSourceHeapId(override val typ: Type, override val pp: ProgramPoint, summary: Boolean = false) extends ProgramPointHeapIdentifier {
  override val counter = 0

  override def equals(other: Any): Boolean = other match {
    case NonDeterminismSourceHeapId(otherTyp, otherPP, otherSummary) =>
      (pp == otherPP) && (summary == otherSummary)
    case _ => false
  }
  override def toString: String = "__nondet_"+ pp.toString + (if (PPDSettings.printSummary && summary) "Σ" else "")

  override def factory(): NonDeterminismSourceHeapId = this.copy()

  override def representsSingleVariable(): Boolean = !summary

  override def toSummaryNode: ProgramPointHeapIdentifier = this.copy(summary = true)

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this.copy(summary = false)

  override def hasMultipleAccessPaths: Boolean = true

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty
  override def setCounter(c:Int) = this
  def getField(): Option[String] = None
  override def isNormalized(): Boolean = true
}


case class CollectionIdentifier(
    pp: ProgramPoint,
    collTyp: Type,
    keyTyp: Type,
    valueTyp: Type,
    lengthTyp: Type,
    originalCollectionTyp: Option[Type],
    keyCollectionTyp: Option[Type],
    summary: Boolean = false,
    counter: Int = 0)
  extends ProgramPointHeapIdentifier {

  val typ = collTyp
  
  def getField : Option[String] = None
  override def isNormalized() : Boolean = true
  override def equals(x : Any) : Boolean = x match {
    case CollectionIdentifier(ppX, collTypX, keyTypX, valueTypX, lengthTypX, origCollTypX, keyCollTypX, summaryX, c) => pp == ppX && collTyp == collTypX && counter == c &&
      keyTyp == keyTypX && valueTyp == valueTypX && lengthTyp == lengthTypX && origCollTypX == originalCollectionTyp && keyCollTypX == keyCollectionTyp && summary == summaryX
    case _ => false
  }

  override def toString : String = collTyp.toString + "("+pp.toString+")" + (if (PPDSettings.printSummary && summary) "Σ" else "")
  override def factory() : ProgramPointHeapIdentifier = new CollectionIdentifier(pp,collTyp, keyTyp, valueTyp, lengthTyp, originalCollectionTyp, keyCollectionTyp)
  override def representsSingleVariable() : Boolean= !summary
  override def clone() : Object = new CollectionIdentifier(pp,collTyp, keyTyp, valueTyp, lengthTyp, originalCollectionTyp, keyCollectionTyp)
  override def toSummaryNode : ProgramPointHeapIdentifier = new CollectionIdentifier(pp,collTyp, keyTyp, valueTyp, lengthTyp, originalCollectionTyp, keyCollectionTyp, true)
  override def toNonSummaryNode : ProgramPointHeapIdentifier = new CollectionIdentifier(pp,collTyp, keyTyp, valueTyp, lengthTyp, originalCollectionTyp, keyCollectionTyp, false)
  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) = new CollectionIdentifier(pp, collTyp, keyTyp, valueTyp, lengthTyp, originalCollectionTyp, keyCollectionTyp, summary, c)
}

case class CollectionTupleIdentifier(
    collectionApprox: ProgramPointHeapIdentifier,
    keyTyp: Type,
    valueTyp: Type,
    pps: Set[ProgramPoint],
    summary: Boolean = false,
    counter: Int = 0)
  extends ProgramPointHeapIdentifier {

  val typ = collectionApprox.getType

  val pp = pps.head
  
  def getField : Option[String] = None
  override def isNormalized() : Boolean = false

  override def equals(x : Any) : Boolean = x match {
    case CollectionTupleIdentifier(collectionApproxX, keyTypX, valueTypX, ppsX, summaryX, c) =>
      if (!this.collectionApprox.equals(collectionApproxX)) return false
      if (c != counter) return false
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

  override def toString : String = {
    val approxType = this.collectionApprox match {
      case FieldAndProgramPoint(_, x, _, _) => x
    }

    "T(" + approxType + "," + this.collectionApprox.pp + ", " + this.pps.mkString(",") + ")" + (if (PPDSettings.printSummary && !representsSingleVariable()) "Σ" else "")
  }

  override def factory() : ProgramPointHeapIdentifier = new CollectionTupleIdentifier(this.collectionApprox, this.keyTyp, this.valueTyp, this.pps, this.summary)
  override def representsSingleVariable() : Boolean = !summary
  override def clone() : Object = new CollectionTupleIdentifier(this.collectionApprox, this.keyTyp, this.valueTyp, this.pps, this.summary)
  override def toSummaryNode : ProgramPointHeapIdentifier = new CollectionTupleIdentifier(this.collectionApprox.toSummaryNode, this.keyTyp, this.valueTyp, this.pps, true)
  override def toNonSummaryNode : ProgramPointHeapIdentifier = new CollectionTupleIdentifier(this.collectionApprox.toNonSummaryNode, this.keyTyp, this.valueTyp, this.pps, false)
  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set(collectionApprox)
  def contains(other:CollectionTupleIdentifier): Boolean = {
    if (this.pps.size <= other.pps.size) return false
    var contains = true
    for(pp <- other.pps) {
      contains &= this.pps.contains(pp)
    }
    contains
  }

  // TODO: This can be more precise; If the key is not a summary node, then this could return false.
  override def hasMultipleAccessPaths = true

  override def setCounter(c:Int) =  new CollectionTupleIdentifier(this.collectionApprox, this.keyTyp, this.valueTyp, this.pps, this.summary, c)
}

case class ArrayTopIdentifier(counter: Int = 0) extends ProgramPointHeapIdentifier {
  val typ = SystemParameters.typ.top()

  val pp = DummyProgramPoint

  def getField: Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case ArrayTopIdentifier(c) => c == counter
    case _ => false
  }

  override def toString: String = "Array ID"

  override def factory(): ProgramPointHeapIdentifier = new ArrayTopIdentifier()

  override def representsSingleVariable(): Boolean = false

  override def clone(): Object = new ArrayTopIdentifier()

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) =  new ArrayTopIdentifier(c)
}

case class ParameterHeapIdentifier(
    typ: Type,
    pp: ProgramPoint,
    summary: Boolean = false,
    counter: Int = 0)
  extends ProgramPointHeapIdentifier {

  def getField: Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case ParameterHeapIdentifier(t3, _, s, c) => this.typ.equals(t3) && this.summary == s && c == counter
    case _ => false
  }

  override def representsSingleVariable(): Boolean = !summary

  override def factory(): ProgramPointHeapIdentifier = new ParameterHeapIdentifier(this.getType, this.pp)

  override def toString: String = "Parameter of type " + this.getType + (if (PPDSettings.printSummary && summary) "Σ" else "")

  override def toSummaryNode: ProgramPointHeapIdentifier = new ParameterHeapIdentifier(this.getType, this.pp, true)

  override def toNonSummaryNode: ProgramPointHeapIdentifier = new ParameterHeapIdentifier(this.getType, this.pp, false)

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) =  new ParameterHeapIdentifier(typ,pp,summary,c)
}

case class UnsoundParameterHeapIdentifier(
    typ: Type,
    n: Int,
    pp: ProgramPoint,
    counter: Int = 0)
  extends ProgramPointHeapIdentifier {

  def getField: Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case UnsoundParameterHeapIdentifier(t3, n2, _, _) => this.typ.equals(t3) && this.n == n2
    case _ => false
  }

  override def representsSingleVariable(): Boolean = this.n != NonRelationalHeapDomainSettings.maxInitialNodes

  override def factory(): ProgramPointHeapIdentifier = new UnsoundParameterHeapIdentifier(this.getType, this.n, this.pp)

  override def toString: String = "Unsound parameter of type " + this.getType + " number " + this.n

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) =  new UnsoundParameterHeapIdentifier(typ,n,pp,c)
}

case class StaticProgramPointHeapIdentifier(typ: Type, pp: ProgramPoint, counter: Int = 0)
  extends ProgramPointHeapIdentifier {

  def getField: Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case StaticProgramPointHeapIdentifier(t3, _, c) => this.getType.equals(t3) && c == counter
    case _ => false
  }

  override def representsSingleVariable(): Boolean = true

  override def factory(): ProgramPointHeapIdentifier = new StaticProgramPointHeapIdentifier(this.getType, this.pp)

  override def toString: String = "Static " + this.getType + ""

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set.empty

  override def setCounter(c:Int) =  new StaticProgramPointHeapIdentifier(typ,pp,c)
}

case class FieldAndProgramPoint(
    p1: ProgramPointHeapIdentifier,
    field: String,
    typ: Type,
    counter: Int = 0)
  extends ProgramPointHeapIdentifier {

  val pp = p1.pp

  def getField: Option[String] = Some(field)

  override def isNormalized(): Boolean = false

  override def equals(x: Any): Boolean = x match {
    case FieldAndProgramPoint(pp1, field1, _, c) => this.p1.equals(pp1) && this.field.equals(field1) && c == counter
    case _ => false
  }

  override def representsSingleVariable(): Boolean = p1.representsSingleVariable()

  override def factory(): ProgramPointHeapIdentifier = new FieldAndProgramPoint(this.p1, this.field, this.getType)

  override def toString: String = "(" + p1.toString + ", " + field + ")"

  override def toSummaryNode: ProgramPointHeapIdentifier = new FieldAndProgramPoint(this.p1.toSummaryNode, this.field, this.getType)

  override def toNonSummaryNode: ProgramPointHeapIdentifier = new FieldAndProgramPoint(this.p1.toNonSummaryNode, this.field, this.getType)

  override def getReachableFromIds: Set[ProgramPointHeapIdentifier] = Set(p1)

  override def setCounter(c:Int) =  new FieldAndProgramPoint(p1,field,typ,c)
}