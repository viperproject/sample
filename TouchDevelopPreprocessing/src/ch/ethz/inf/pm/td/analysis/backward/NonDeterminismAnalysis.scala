package ch.ethz.inf.pm.td.analysis.backward

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ClassDefinition, Type, ProgramPoint}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.semantics.TNumber
import ch.ethz.inf.pm.td.compiler.{TouchCompiler, TouchCollection}
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, MethodSummaries}
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.sample.backwardanalysis.NonDetSource

object NonDeterminismAnalysis {

  /**
   * Performs a data flow analysis to collect accesses of non-deterministim souces
   * and determines whether they are single accesses or potentially multiple (summaries!)
   */
  def findNonDetSources(classes: List[ClassDefinition], compiler: TouchCompiler): Set[NonDetSource] = {
    SystemParameters.resetOutput()
    MethodSummaries.reset[NonDeterminismCollectingState]()
    Reporter.disableAllOutputs()

    val accessMaps =
      for (clazz <- classes; method <- clazz.methods if compiler.isTopLevel(method)) yield {
        val params = for (a <- method.arguments.head) yield { new ExpressionSet(a.typ) }
        val result = MethodSummaries.collect(method.programpoint, method, NonDeterminismCollectingState(SystemParameters.getType().top()), params)
        val accessDomMap = result.accessDom

        if (TouchAnalysisParameters.backwardInterprocAnalysis && compiler.isEvent(method)) {
          // be on the safe side and create summaries because events may be executed multiple times
          accessDomMap.setAllTop()
        } else {
          accessDomMap
        }
      }
    accessMap2NonDetSources(Lattice.bigLub(accessMaps))
  }

  private def accessMap2NonDetSources(am: AccessMap): Set[NonDetSource] = {
    val functionalValues = am.map
    for ((AccessKey(pp, typ), accessCount) <- functionalValues.toSet) yield {
      val summary =
        accessCount match {
          case SingleAccess => false
          case MultipleAccess => true
          case NoAccess => sys.error("Not accessed id should not be in here.")
        }
      NonDetSource(pp, typ, summary)
    }
  }
}

trait CollectingState

sealed abstract class AccessCount extends Lattice[AccessCount] {
  def factory(): AccessCount = NoAccess
  def top(): AccessCount = MultipleAccess
  def bottom(): AccessCount = NoAccess

  def lub(that: AccessCount): AccessCount = (this, that) match {
    case (NoAccess, other) => other
    case (other, NoAccess) => other
    case (MultipleAccess, _) => MultipleAccess
    case (_, MultipleAccess) => MultipleAccess
    case (SingleAccess, SingleAccess) => SingleAccess
  }

  def glb(that: AccessCount): AccessCount = (this, that) match {
    case (NoAccess, _) => NoAccess
    case (_, NoAccess) => NoAccess
    case (MultipleAccess, other) => other
    case (other, MultipleAccess) => other
    case (SingleAccess, SingleAccess) => SingleAccess
  }

  def widening(that: AccessCount): AccessCount = MultipleAccess

  def lessEqual(r: AccessCount): Boolean = {
    if (this == r) true
    else (this, r) match {
      case (NoAccess, _) => true
      case (SingleAccess, MultipleAccess) => true
      case _ => false
    }
  }
}

case object MultipleAccess extends AccessCount
case object SingleAccess extends AccessCount
case object NoAccess extends AccessCount

case class AccessKey(pp: ProgramPoint, typ: Type)

case class AccessMap(map: Map[AccessKey, AccessCount] = Map.empty, isTop: Boolean = false, override val isBottom: Boolean = false)
  extends FunctionalDomain[AccessKey, AccessCount, AccessMap] {

  def functionalFactory(value: Map[AccessKey, AccessCount], _isBottom: Boolean, _isTop: Boolean): AccessMap =  new AccessMap(value, isTop = _isTop, isBottom= _isBottom)

  def get(key: AccessKey): AccessCount = map.getOrElse(key, NoAccess)

  def setAllTop(): AccessMap = {
    if (isTop || isBottom) return this

    new AccessMap(map.mapValues(_ => MultipleAccess))
  }
}

/** State for the non-determinism data flow analysis */
case class NonDeterminismCollectingState(typDom:Type, accessDom: AccessMap = new AccessMap())
  extends CartesianProductDomain[Type, AccessMap, NonDeterminismCollectingState]
  with State[NonDeterminismCollectingState]
  with CollectingState {

  def _1 = typDom

  def _2 = accessDom

  def nonDeterminismSourceAt(pp: ProgramPoint, typ: Type): NonDeterminismCollectingState = {
    val wrapperType = typ.asInstanceOf[TNonDetWrapper]
    val innerType = wrapperType.innerType
    val aKey = AccessKey(pp, innerType)
    val n = _2.get(aKey)
    val updated = n match {
      case NoAccess => SingleAccess
      case SingleAccess => MultipleAccess
      case MultipleAccess => MultipleAccess
    }
    set_2(_2.add(aKey, updated)).setType(typ)
  }

  def createNonDeterminismSource(typ: Type, pp: ProgramPoint, summary: Boolean): NonDeterminismCollectingState = this.setType(typ)

  def setType(typ:Type): NonDeterminismCollectingState = set_1(typ)
  def getType:Type = typDom

  def getFieldValue(obj: ExpressionSet, field: String, typ: Type): NonDeterminismCollectingState = setType(typ)

  def setExpression(expr: ExpressionSet): NonDeterminismCollectingState = this.setType(expr.getType())
  def expr: ExpressionSet = new ExpressionSet(typDom).add(new UnitExpression(typDom,null))
  def removeExpression(): NonDeterminismCollectingState = this.setType(SystemParameters.getType().top())
  def createObject(typ: Type, pp: ProgramPoint, fields : Option[Set[Identifier]] = None): NonDeterminismCollectingState = this.setType(typ)
  def evalConstant(value: String, typ: Type, pp: ProgramPoint): NonDeterminismCollectingState = this.setType(typ)
  def getVariableValue(id: Assignable): NonDeterminismCollectingState = this.setType(id.typ)

  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, keyCollectionTyp:Option[Type], tpp: ProgramPoint, fields : Option[Set[Identifier]] = None) =
    this.setType(collTyp)
  def getSummaryCollectionIfExists(collectionSet: ExpressionSet) = this
  def getCollectionValue(valueIds: ExpressionSet) = this
  def insertCollectionTopElement(collectionSet: ExpressionSet, keyTop: ExpressionSet, valueTop: ExpressionSet, pp: ProgramPoint) = this
  def getCollectionKeyByKey(collectionSet: ExpressionSet, keySet: ExpressionSet) =
    this.setType(collectionSet.getType().asInstanceOf[TouchCollection].keyType)
  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet) =
    this.setType(collectionSet.getType().asInstanceOf[TouchCollection].valueType)
  def getCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet) =
    this.setType(collectionSet.getType().asInstanceOf[TouchCollection].valueType)
  def extractCollectionKeys(fromCollectionSet: ExpressionSet, newKeyValueSet: ExpressionSet, fromCollectionTyp:Type, collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint) = this
  def getOriginalCollection(collectionSet: ExpressionSet) = this
  def getKeysCollection(collectionSet: ExpressionSet) = this
  def removeCollectionKeyConnection(origCollectionSet: ExpressionSet, keyCollectionSet: ExpressionSet) = this
  def copyCollection(fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet) = this
  def insertCollectionElement(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet, pp: ProgramPoint) = this
  def removeCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet) = this
  def removeFirstCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet) = this
  def assignAllCollectionKeys(collectionSet: ExpressionSet, valueSet: ExpressionSet) = this
  def clearCollection(collectionSet: ExpressionSet) = this
  def getCollectionLength(collectionSet: ExpressionSet): NonDeterminismCollectingState = this.setType(TNumber.typ)
  def collectionContainsKey(collectionSet: ExpressionSet, keySet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint) = this
  def collectionContainsValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint) = this
  def isSummaryCollection(collectionSet: ExpressionSet) = false

  def pruneVariables(filter:Identifier => Boolean) : NonDeterminismCollectingState = this
  def pruneUnreachableHeap() : NonDeterminismCollectingState = this
  def optimizeSummaryNodes() : NonDeterminismCollectingState = this
  def testFalse(): NonDeterminismCollectingState = this
  def testTrue(): NonDeterminismCollectingState = this
  def assume(cond: ExpressionSet): NonDeterminismCollectingState = this
  def backwardAssignVariable(oldPreState: NonDeterminismCollectingState, x: ExpressionSet, right: ExpressionSet): NonDeterminismCollectingState = this
  def backwardGetFieldValue(objs: ExpressionSet, field: String, typ: Type): NonDeterminismCollectingState = this
  def backwardGetVariableValue(id: Assignable): NonDeterminismCollectingState = this
  def getArrayLength(array: ExpressionSet): NonDeterminismCollectingState = this
  def getArrayCell(obj: ExpressionSet, index: ExpressionSet, typ: Type): NonDeterminismCollectingState = this
  def throws(t: ExpressionSet): NonDeterminismCollectingState = this
  def removeVariable(x: ExpressionSet): NonDeterminismCollectingState = this
  def setVariableToTop(x: ExpressionSet): NonDeterminismCollectingState = this
  def setArgument(x: ExpressionSet, right: ExpressionSet): NonDeterminismCollectingState = this
  def assignArrayCell(obj: ExpressionSet, index: ExpressionSet, right: ExpressionSet, typ: Type): NonDeterminismCollectingState = this
  def assignField(obj: ExpressionSet, field: String, right: ExpressionSet): NonDeterminismCollectingState = this
  def createArray(length: ExpressionSet, typ: Type, pp: ProgramPoint): NonDeterminismCollectingState = this
  def assignVariable(x: ExpressionSet, right: ExpressionSet): NonDeterminismCollectingState = this
  def createVariable(x: ExpressionSet, typ: Type, pp: ProgramPoint): NonDeterminismCollectingState = this
  def createVariableForArgument(x: ExpressionSet, typ: Type): NonDeterminismCollectingState = this
  def before(pp: ProgramPoint): NonDeterminismCollectingState = this
  def removeObject(oldPreState: NonDeterminismCollectingState, obj: ExpressionSet, fields: Option[Set[Identifier]]): NonDeterminismCollectingState = this
  def backwardAssignField(oldPreState: NonDeterminismCollectingState, obj: ExpressionSet, field: String, right: ExpressionSet): NonDeterminismCollectingState = this
  def setCollectionToTop(collectionSet: ExpressionSet): NonDeterminismCollectingState = this
  def undoPruneVariables(unprunedPreState: NonDeterminismCollectingState, filter: (Identifier) => Boolean): NonDeterminismCollectingState = this
  def undoPruneUnreachableHeap(preState: NonDeterminismCollectingState): NonDeterminismCollectingState = this

  def factory(a: Type, b: AccessMap): NonDeterminismCollectingState = new NonDeterminismCollectingState(a, b)
}
