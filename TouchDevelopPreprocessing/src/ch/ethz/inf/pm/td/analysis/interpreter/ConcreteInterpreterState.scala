package ch.ethz.inf.pm.td.analysis.interpreter

import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, ControlFlowGraph, MethodDeclaration}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import collection.mutable.{Map => MMap}
import ch.ethz.inf.pm.td.analysis.backward.InterpreterTestInput
import scala.util.Random
import ch.ethz.inf.pm.td.semantics.TouchField

class ConcreteInterpreterState(
  var methodStack: List[MethodDeclaration] = Nil,
  var callPoints: List[ProgramPoint] = Nil,
  var envStack: List[MMap[VariableIdentifier, TouchValue]] = Nil,
  globals: MMap[VariableIdentifier, TouchValue] = MMap.empty,
  heap: MMap[HeapObjId, HeapObj] = MMap.empty,
  randomSeed: Int = 0) {

  def currentMethod: MethodDeclaration = methodStack.head
  def env: MMap[VariableIdentifier, TouchValue] = envStack.head
  def currentCFG: ControlFlowGraph = currentMethod.body

  var heapAddrCounter: Long = 0
  var random = new Random(randomSeed)

  def getVar(id: VariableIdentifier): TouchValue = {
    globals.get(id) match {
      case Some(touchVal) => touchVal
      case None => env.get(id) match {
        case Some(touchVal) => touchVal
        case None => failInternal(s"Accessing non-existing  variable $id")
      }
    }
  }

  def getField(ref: RefV, field: TouchField): TouchValue =
    getField(ref, field.getName)

  def getField(ref: RefV, fieldName: String): TouchValue = {
    heap.get(ref.id) match {
      case Some(heapObj: BasicHeapObj) => heapObj.fields.get(fieldName) match {
        case Some(touchVal) => touchVal
        case None => failInternal(s"Accessing non-existing field $fieldName on object $heapObj")
      }
      case None => failInternal(s"No heap object at address (id) ${ref.id}")
    }
  }

  def setField(ref: RefV, fieldName: String, fieldVal: TouchValue) = {
    heap.get(ref.id) match {
      case Some(heapObj: BasicHeapObj) => heapObj.fields.get(fieldName) match {
        case Some(touchVal) =>
          val updatedFields = heapObj.fields + (fieldName -> fieldVal)
          heap(heapObj.id) = heapObj.copy(fields = updatedFields)
        case None => failInternal(s"Accessing non-existing field $fieldName on object $heapObj")
      }
      case None => failInternal(s"No heap object at address (id) ${ref.id}")
    }
  }

  def setVar(id: VariableIdentifier, value: TouchValue) = {
    env(id) = value
  }

  def setGlobal(id: VariableIdentifier, ref: TouchValue) = {
    globals(id) = ref
  }

  def createObjectWithTouchFields(typ: TouchType,
                                  fieldInit: Map[TouchField, TouchValue] = Map.empty): RefV = {
    createObject(typ, fieldInit.map { case (k, v) => k.toString -> v})
  }

  def createObject(typ: TouchType, fieldInit: Map[String, TouchValue] = Map.empty): RefV = {
    val heapId = HeapObjId(heapAddrCounter)
    heapAddrCounter = heapAddrCounter + 1
    val heapObj = BasicHeapObj(heapId, typ, fieldInit)
    heap(heapId) = heapObj
    RefV(typ, heapId)
  }

  def createCollection(typ: TouchCollection,
                       entries: Map[TouchValue, TouchValue] = Map.empty,
                       fields: Map[String, TouchValue] = Map.empty): RefV = {
    val heapId = HeapObjId(heapAddrCounter)
    heapAddrCounter = heapAddrCounter + 1
    val heapObj = CollectionObj(heapId, typ, fields, entries)
    heap(heapId) = heapObj
    RefV(typ, heapId)
  }

  def updateCollectionEntries(collRef: RefV,
                              newEntries: Map[TouchValue, TouchValue]): CollectionObj = {
    val addr = collRef.id
    heap.get(addr) match {
      case Some(c: CollectionObj) =>
        val newColl = c.copy(entries = newEntries)
        heap(addr) = newColl
        newColl

      case _ => failInternal("Illegal reference to collection")
    }
  }

  def getCollection(ref: RefV): CollectionObj = {
    heap.get(ref.id) match {
      case Some(c: CollectionObj) =>
        c
      case _ => failInternal("Illegal reference to collection")
    }
  }

  def getObject(ref: RefV): BasicHeapObj = {
    heap.get(ref.id) match {
      case Some(c: BasicHeapObj) =>
        c
      case _ => failInternal("Illegal reference to collection")
    }
  }

  def failInternal(message: String): Nothing = {
    sys.error(message)
  }

  /** pretty-prints a TouchValue (largely untested) */
  def printTouchValue(tv: TouchValue): String = {
    // Remember visited heap addresses.
    var visitedHeapAddrs = Set.empty[HeapObjId]

    def printPrimitive(tvPrimitive: TouchValue): String = {
      tvPrimitive match {
        case InvalidV(t) =>
          "invalid[" + t.name + "]"
        case UnitV => "unit"
        case NumberV(n) => n.toString
        case BooleanV(b) => b.toString
        case StringV(s) => "\"" + s + "\""
        case RefV(typ, heapId) =>
          val addr = s"@${heapId.address}[${typ.name}]"
          if (visitedHeapAddrs.contains(heapId)) {
            // Do not print same object twice (avoid recursion in  potentially circular heap structures)
            addr + "{ ... }"
          } else {
            visitedHeapAddrs += heapId
            addr + printObj(heapId)
          }
      }
    }

    def printObj(heapId: HeapObjId): String = {
      heap.get(heapId) match {
        case Some(heapObj) =>
          heapObj match {
            case BasicHeapObj(id, typ, fields) =>
              fields.map({case (k, fieldVal) => k + " -> " + printTouchValue(fieldVal)})
                .mkString("{", ",", "}")
            case CollectionObj(id, typ, fields, collectionEntries) =>
              val normalFields = fields.map({case (k, fieldVal) => k + " -> " + printTouchValue(fieldVal)}).toList
              val printedCollEntries = collectionEntries.map({case (k, fieldVal) => printTouchValue(k) + " -> " + printTouchValue(fieldVal)}).toList.mkString("{", ",", "}")
              (normalFields ::: List("entries -> " + printedCollEntries)).mkString("{", ",", "}")
          }
      }
    }

    printPrimitive(tv)
  }



}

object ConcreteInterpreterState {
  def createForInput(input: InterpreterTestInput): ConcreteInterpreterState = {
    new ConcreteInterpreterState()
  }
}