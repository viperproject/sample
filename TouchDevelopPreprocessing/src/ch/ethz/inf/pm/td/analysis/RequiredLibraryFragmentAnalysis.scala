package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ClassDefinition, Type, ProgramPoint}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.semantics.TNumber
import ch.ethz.inf.pm.td.analysis.{MethodSummaries}
import ch.ethz.inf.pm.sample.reporting.Reporter

/**
 * User: lucas
 * Date: 3/1/13
 * Time: 2:19 PM
 */

trait CollectingState


object RequiredLibraryFragmentAnalysis {

  var spottedFields = Set.empty[String]

  def apply(classes:List[ClassDefinition]):Set[String] = {
    spottedFields = Set.empty[String]

    SystemParameters.resetOutput()
    MethodSummaries.reset[AccessCollectingState]()
    Reporter.disableAllOutputs()

    for (clazz <- classes; method <- clazz.methods) {

      val params = for (a <- method.arguments.head) yield { new ExpressionSet(a.typ) }
      MethodSummaries.collect(method.programpoint, method, new AccessCollectingState(SystemParameters.getType().top()),params)

    }

    spottedFields
  }

}

class AccessCollectingState(myType:Type) extends State[AccessCollectingState] with CollectingState {

  def factory(): AccessCollectingState = new AccessCollectingState(SystemParameters.getType().top())
  def setType(typ:Type): AccessCollectingState = new AccessCollectingState(typ)
  def getType:Type = myType

  def getFieldValue(obj: ExpressionSet, field: String, typ: Type): AccessCollectingState = {
    RequiredLibraryFragmentAnalysis.spottedFields =
      RequiredLibraryFragmentAnalysis.spottedFields +
        (obj.getType().toString+"."+field) +
        obj.getType().toString
    new AccessCollectingState(typ)
  }

  def setExpression(expr: ExpressionSet): AccessCollectingState = this.setType(expr.getType())
  def getExpression: ExpressionSet = ExpressionSet(new UnitExpression(myType, null))
  def removeExpression(): AccessCollectingState = this.setType(SystemParameters.getType().top())
  def createObject(typ: Type, pp: ProgramPoint, fields : Option[Set[Identifier]] = None): AccessCollectingState = this.setType(typ)
  def evalConstant(value: String, typ: Type, pp: ProgramPoint): AccessCollectingState = this.setType(typ)
  def getVariableValue(id: Assignable): AccessCollectingState = this.setType(id.typ)

  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, keyCollectionTyp:Option[Type], tpp: ProgramPoint, fields : Option[Set[Identifier]] = None) =
    this.setType(collTyp)
  def getSummaryCollectionIfExists(collectionSet: ExpressionSet) = this
  def getCollectionValue(valueIds: ExpressionSet) = this
  def insertCollectionTopElement(collectionSet: ExpressionSet, keyTop: ExpressionSet, valueTop: ExpressionSet, pp: ProgramPoint) = this
  def getCollectionKeyByKey(collectionSet: ExpressionSet, keySet: ExpressionSet) =
    this.setType(collectionSet.getType().asInstanceOf[TouchCollection].getKeyType)
  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet) =
    this.setType(collectionSet.getType().asInstanceOf[TouchCollection].getValueType)
  def getCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet) =
    this.setType(collectionSet.getType().asInstanceOf[TouchCollection].getValueType)
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
  def getCollectionLength(collectionSet: ExpressionSet): AccessCollectingState = this.setType(TNumber.typ)
  def collectionContainsKey(collectionSet: ExpressionSet, keySet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint) = this
  def collectionContainsValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint) = this
  def isSummaryCollection(collectionSet: ExpressionSet) = false

  def pruneVariables(filter:Identifier => Boolean) : AccessCollectingState = this
  def pruneUnreachableHeap() : AccessCollectingState = this
  def optimizeSummaryNodes() : AccessCollectingState = this
  def testFalse(): AccessCollectingState = this
  def testTrue(): AccessCollectingState = this
  def assume(cond: ExpressionSet): AccessCollectingState = this
  def backwardAssignVariable(oldPreState: AccessCollectingState, x: ExpressionSet, right: ExpressionSet): AccessCollectingState = this
  def backwardGetFieldValue(obj: ExpressionSet, field: String, typ: Type): AccessCollectingState = this
  def backwardGetVariableValue(id: Assignable): AccessCollectingState = this
  def throws(t: ExpressionSet): AccessCollectingState = this
  def removeVariable(x: ExpressionSet): AccessCollectingState = this
  def setVariableToTop(x: ExpressionSet): AccessCollectingState = this
  def setArgument(x: ExpressionSet, right: ExpressionSet): AccessCollectingState = this
  def assignField(obj: ExpressionSet, field: String, right: ExpressionSet): AccessCollectingState = this
  def assignVariable(x: ExpressionSet, right: ExpressionSet): AccessCollectingState = this
  def createVariable(x: ExpressionSet, typ: Type, pp: ProgramPoint): AccessCollectingState = this
  def createVariableForArgument(x: ExpressionSet, typ: Type): AccessCollectingState = this
  def before(pp: ProgramPoint): AccessCollectingState = this

  def bottom(): AccessCollectingState = new AccessCollectingState(myType.bottom())

  def glb(other: AccessCollectingState): AccessCollectingState =
    new AccessCollectingState(getType.glb(other.getType))

  def lessEqual(r: AccessCollectingState): Boolean = myType.lessEqual(r.getType)

  def lub(other: AccessCollectingState): AccessCollectingState =
    new AccessCollectingState(getType.lub(other.getType))

  def top(): AccessCollectingState = new AccessCollectingState(myType.top())

  def widening(other: AccessCollectingState): AccessCollectingState =
    new AccessCollectingState(getType.widening(other.getType))


  def createNonDeterminismSource(typ: Type, pp: ProgramPoint, summary: Boolean): AccessCollectingState = this

  def nonDeterminismSourceAt(pp: ProgramPoint, typ: Type): AccessCollectingState = this

  def removeObject(oldPreState: AccessCollectingState, obj: ExpressionSet, fields: Option[Set[Identifier]]): AccessCollectingState = this

  def backwardAssignField(oldPreState: AccessCollectingState, obj: ExpressionSet, field: String, right: ExpressionSet): AccessCollectingState = this

  def setCollectionToTop(collectionSet: ExpressionSet): AccessCollectingState = this

  def undoPruneVariables(unprunedPreState: AccessCollectingState, filter: (Identifier) => Boolean): AccessCollectingState = this

  def undoPruneUnreachableHeap(preState: AccessCollectingState): AccessCollectingState = this

}
