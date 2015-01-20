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

  def pp: ProgramPoint

  def counter: Int

  override def getLabel() = "Program point"

  override def extractField(h: ProgramPointHeapIdentifier, s: String, t: Type): ProgramPointHeapIdentifier = h match {
    case x: SimpleProgramPointHeapIdentifier => new FieldAndProgramPoint(x, s, t)
    case x: NonDeterminismSourceHeapId => new FieldAndProgramPoint(x, s, t)
    case x: ParameterHeapIdentifier => new FieldAndProgramPoint(x, s, t)
    case x: StaticProgramPointHeapIdentifier => new FieldAndProgramPoint(x, s, t)
    case x: UnsoundParameterHeapIdentifier => new FieldAndProgramPoint(x, s, t)
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

  def createNonDeterminismSource(typ: Type, pp: ProgramPoint, multiple: Boolean): ProgramPointHeapIdentifier = {
    NonDeterminismSourceHeapId(typ, pp, multiple)
  }
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

  override def factory(): ProgramPointHeapIdentifier = new NullProgramPointHeapIdentifier(typ, this.pp)

  override def representsSingleVariable: Boolean = true

  override def clone(): Object = new NullProgramPointHeapIdentifier(typ, this.pp)

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromId: Option[ProgramPointHeapIdentifier] = None

  override def setCounter(c: Int) = new NullProgramPointHeapIdentifier(typ, pp, c)
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

  override def factory(): ProgramPointHeapIdentifier = new SimpleProgramPointHeapIdentifier(this.pp, this.typ)

  override def representsSingleVariable: Boolean = !summary

  override def clone(): Object = new SimpleProgramPointHeapIdentifier(pp, this.typ)

  override def toSummaryNode: ProgramPointHeapIdentifier = new SimpleProgramPointHeapIdentifier(this.pp, this.typ, true)

  override def toNonSummaryNode: ProgramPointHeapIdentifier = new SimpleProgramPointHeapIdentifier(this.pp, this.typ, false)

  override def getReachableFromId: Option[ProgramPointHeapIdentifier] = None

  override def setCounter(c: Int) = new SimpleProgramPointHeapIdentifier(pp, typ, summary, c)
}

case class NonDeterminismSourceHeapId(override val typ: Type, override val pp: ProgramPoint, summary: Boolean = false) extends ProgramPointHeapIdentifier {
  override val counter = 0

  override def equals(other: Any): Boolean = other match {
    case NonDeterminismSourceHeapId(otherTyp, otherPP, otherSummary) =>
      (pp == otherPP) && (summary == otherSummary)
    case _ => false
  }

  override def toString: String = "__nondet_" + pp.toString + (if (PPDSettings.printSummary && summary) "Σ" else "")

  override def factory(): NonDeterminismSourceHeapId = this.copy()

  override def representsSingleVariable: Boolean = !summary

  override def toSummaryNode: ProgramPointHeapIdentifier = this.copy(summary = true)

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this.copy(summary = false)

  override def getReachableFromId: Option[ProgramPointHeapIdentifier] = None

  override def setCounter(c: Int) = this

  def getField(): Option[String] = None

  override def isNormalized(): Boolean = true
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

  override def representsSingleVariable: Boolean = false

  override def clone(): Object = new ArrayTopIdentifier()

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromId: Option[ProgramPointHeapIdentifier] = None

  override def setCounter(c: Int) = new ArrayTopIdentifier(c)
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

  override def representsSingleVariable: Boolean = !summary

  override def factory(): ProgramPointHeapIdentifier = new ParameterHeapIdentifier(this.typ, this.pp)

  override def toString: String = "Parameter of type " + this.typ + (if (PPDSettings.printSummary && summary) "Σ" else "")

  override def toSummaryNode: ProgramPointHeapIdentifier = new ParameterHeapIdentifier(this.typ, this.pp, true)

  override def toNonSummaryNode: ProgramPointHeapIdentifier = new ParameterHeapIdentifier(this.typ, this.pp, false)

  override def getReachableFromId: Option[ProgramPointHeapIdentifier] = None

  override def setCounter(c: Int) = new ParameterHeapIdentifier(typ, pp, summary, c)
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

  override def representsSingleVariable: Boolean = this.n != NonRelationalHeapDomainSettings.maxInitialNodes

  override def factory(): ProgramPointHeapIdentifier = new UnsoundParameterHeapIdentifier(this.typ, this.n, this.pp)

  override def toString: String = "Unsound parameter of type " + this.typ + " number " + this.n

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromId: Option[ProgramPointHeapIdentifier] = None

  override def setCounter(c: Int) = new UnsoundParameterHeapIdentifier(typ, n, pp, c)
}

case class StaticProgramPointHeapIdentifier(typ: Type, pp: ProgramPoint, counter: Int = 0)
  extends ProgramPointHeapIdentifier {

  def getField: Option[String] = None

  override def isNormalized(): Boolean = true

  override def equals(x: Any): Boolean = x match {
    case StaticProgramPointHeapIdentifier(t3, _, c) => this.typ.equals(t3) && c == counter
    case _ => false
  }

  override def representsSingleVariable: Boolean = true

  override def factory(): ProgramPointHeapIdentifier = new StaticProgramPointHeapIdentifier(this.typ, this.pp)

  override def toString: String = "Static " + this.typ + ""

  override def toSummaryNode: ProgramPointHeapIdentifier = this

  override def toNonSummaryNode: ProgramPointHeapIdentifier = this

  override def getReachableFromId: Option[ProgramPointHeapIdentifier] = None

  override def setCounter(c: Int) = new StaticProgramPointHeapIdentifier(typ, pp, c)
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

  override def representsSingleVariable: Boolean = p1.representsSingleVariable

  override def factory(): ProgramPointHeapIdentifier = new FieldAndProgramPoint(this.p1, this.field, this.typ)

  override def toString: String = "(" + p1.toString + ", " + field + ")"

  override def toSummaryNode: ProgramPointHeapIdentifier = new FieldAndProgramPoint(this.p1.toSummaryNode, this.field, this.typ)

  override def toNonSummaryNode: ProgramPointHeapIdentifier = new FieldAndProgramPoint(this.p1.toNonSummaryNode, this.field, this.typ)

  override def getReachableFromId: Option[ProgramPointHeapIdentifier] = Some(p1)

  override def setCounter(c: Int) = new FieldAndProgramPoint(p1, field, typ, c)
}