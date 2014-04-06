package ch.ethz.inf.pm.td.analysis.interpreter

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.td.semantics.{TString, TNumber, TBoolean}
import ch.ethz.inf.pm.sample.oorepresentation.Type

/**
 * A concrete runtime value of TouchDevelop
 *
 * The different value types are roughly structured as follows:
 *
 *  tv :=   Unit | Invalid | Boolean | Number | String | Ref(typ, heapObjId)
 *  heapObj := BasicHeapObj(fields: fieldName -> tv) | CollectionObj(fields: fieldName -> tv, contents: tv -> tv)
 *
 */
sealed trait TouchValue {
  def typ: TouchType
}

trait ValidV extends TouchValue

object InvalidV {
  // Convenience constructor because there is often no TouchType available
  def apply(t: Type): InvalidV = InvalidV(t.asInstanceOf[TouchType])
}

/** Represents invalid vlaues */
case class InvalidV(typ: TouchType) extends TouchValue

case object UnitV extends TouchValue {
  def typ: TouchType = TBoolean.typ.bottom()
}

case class BooleanV(v: Boolean) extends ValidV {
  def typ: TouchType = TBoolean.typ
}

case class NumberV(v: NumberType) extends ValidV {
  def typ: TouchType = TNumber.typ
}

case class StringV(v: String) extends ValidV {
  def typ: TouchType = TString.typ
}

case class RefV(typ: TouchType, id: HeapObjId) extends ValidV

case class HeapObjId(address: Long)

sealed trait HeapObj {
  def id: HeapObjId

  def typ: TouchType

  def fields: Map[String, TouchValue]
}

/** A "normal" object with fields */
case class BasicHeapObj(
    id: HeapObjId,
    typ: TouchType,
    fields: Map[String, TouchValue])
  extends HeapObj

/** A collection object */
case class CollectionObj(
    id: HeapObjId,
    typ: TouchCollection,
    fields: Map[String, TouchValue],
    entries: Map[TouchValue, TouchValue])
  extends HeapObj

