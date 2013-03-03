package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ClassDefinition, Type, ProgramPoint}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.semantics.TNumber
import ch.ethz.inf.pm.td.analysis.MethodSummaries

/**
 * User: lucas
 * Date: 3/1/13
 * Time: 2:19 PM
 */
object RequiredLibraryFragmentAnalysis {

  var spottedFields = Set.empty[String]

  def apply(classes:List[ClassDefinition]):Set[String] = {
    spottedFields = Set.empty[String]

    SystemParameters.resetOutput
    MethodSummaries.reset[AccessCollectingState]()
    SystemParameters.enableOutputOfAlarms = false
    SystemParameters.enableOutputOfBottomWarnings = false
    SystemParameters.enableOutputOfPrecisionWarnings = false

    for (clazz <- classes; method <- clazz.methods) {
      method.forwardSemantics(new AccessCollectingState(SystemParameters.getType().top())).exitState()
    }
    spottedFields
  }

}

class AccessCollectingState(myType:Type) extends State[AccessCollectingState] {

  def factory(): AccessCollectingState = new AccessCollectingState(SystemParameters.getType().top())
  def setType(typ:Type): AccessCollectingState = new AccessCollectingState(typ)
  def getType:Type = myType

  def getFieldValue(obj: List[ExpressionSet], field: String, typ: Type): AccessCollectingState = {
    for (o <- obj)
      RequiredLibraryFragmentAnalysis.spottedFields =
        RequiredLibraryFragmentAnalysis.spottedFields +
          (o.getType().toString+"."+field) +
          (o.getType().toString)
    new AccessCollectingState(typ)
  }

  def setExpression(expr: ExpressionSet): AccessCollectingState = this.setType(expr.getType())
  def getExpression(): ExpressionSet = new ExpressionSet(myType).add(new UnitExpression(myType,null))
  def removeExpression(): AccessCollectingState = this.setType(SystemParameters.getType().top())
  def createObject(typ: Type, pp: ProgramPoint, createFields : Boolean = true): AccessCollectingState = this.setType(typ)
  def evalNumericalConstant(value: String, typ: Type, pp: ProgramPoint): AccessCollectingState = this.setType(typ)
  def getVariableValue(id: Assignable): AccessCollectingState = this.setType(id.getType())
  def getCollectionLength(collectionSet: ExpressionSet): AccessCollectingState = this.setType(TNumber.typ)
  def getCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet): AccessCollectingState =
    this.setType(collectionSet.getType().asInstanceOf[TouchCollection].getValueType)
  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, tpp: ProgramPoint): AccessCollectingState =
    this.setType(collTyp)

  def removeCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet): AccessCollectingState = this
  def insertCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet): AccessCollectingState = this
  def assignCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet): AccessCollectingState = this
  def testFalse(): AccessCollectingState = this
  def testTrue(): AccessCollectingState = this
  def assume(cond: ExpressionSet): AccessCollectingState = this
  def backwardAssignVariable(x: ExpressionSet, right: ExpressionSet): AccessCollectingState = this
  def backwardGetFieldValue(objs: List[ExpressionSet], field: String, typ: Type): AccessCollectingState = this
  def backwardGetVariableValue(id: Assignable): AccessCollectingState = this
  def getArrayLength(array: ExpressionSet): AccessCollectingState = this
  def getArrayCell(obj: ExpressionSet, index: ExpressionSet, typ: Type): AccessCollectingState = this
  def throws(t: ExpressionSet): AccessCollectingState = this
  def removeVariable(x: ExpressionSet): AccessCollectingState = this
  def setVariableToTop(x: ExpressionSet): AccessCollectingState = this
  def setArgument(x: ExpressionSet, right: ExpressionSet): AccessCollectingState = this
  def assignArrayCell(obj: ExpressionSet, index: ExpressionSet, right: ExpressionSet, typ: Type): AccessCollectingState = this
  def assignField(obj: List[ExpressionSet], field: String, right: ExpressionSet): AccessCollectingState = this
  def createArray(length: ExpressionSet, typ: Type, pp: ProgramPoint): AccessCollectingState = this
  def assignVariable(x: ExpressionSet, right: ExpressionSet): AccessCollectingState = this
  def createVariable(x: ExpressionSet, typ: Type, pp: ProgramPoint): AccessCollectingState = this
  def createVariableForArgument(x: ExpressionSet, typ: Type): AccessCollectingState = this
  def before(pp: ProgramPoint): AccessCollectingState = this

  def bottom(): AccessCollectingState = new AccessCollectingState(myType.bottom())
  def glb(left: AccessCollectingState, right: AccessCollectingState): AccessCollectingState =
    new AccessCollectingState(myType.glb(left.getType,right.getType))
  def lessEqual(r: AccessCollectingState): Boolean = myType.lessEqual(r.getType)
  def lub(left: AccessCollectingState, right: AccessCollectingState): AccessCollectingState =
    new AccessCollectingState(myType.lub(left.getType,right.getType))
  def top(): AccessCollectingState = new AccessCollectingState(myType.top())
  def widening(left: AccessCollectingState, right: AccessCollectingState): AccessCollectingState =
    new AccessCollectingState(myType.widening(left.getType,right.getType))
}
