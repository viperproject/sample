package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ClassDefinition, Type, ProgramPoint}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.property.{OutputCollector, SingleStatementProperty}
import ch.ethz.inf.pm.sample.util.AccumulatingTimer
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.domain.HeapIdentifier
import ch.ethz.inf.pm.td.semantics.{ACollection, TNumber}
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging


/**
 *
 * @author Lucas Brutschy
 *
 */

trait CollectingState

/**
 * Collects all the fields that are accessed in a program. This can be used to
 * optimize the analysis
 */
object RequiredLibraryFragmentAnalysis extends LazyLogging {

  var spottedFields = Set.empty[String]

  def apply(classes: List[ClassDefinition], output: OutputCollector) = {
    //SystemParameters.progressOutput.begin("Library fragment analysis")
    val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]

    spottedFields = Set.empty[String]
    compiler.relevantLibraryFields = Set.empty

    for (clazz <- classes; method <- clazz.methods) {

      val params = for (a <- method.arguments.head) yield {
        new ExpressionSet(a.typ)
      }
      MethodSummaries.collect(method.programpoint, method, new AccessCollectingState(SystemParameters.getType().top()), params)

    }

    SystemParameters.resetOutput()
    val summaries = MethodSummaries.getSummaries[AccessCollectingState]
    val mustCheck = (s: MethodSummary[AccessCollectingState]) => s.method.classDef == compiler.main || !TouchAnalysisParameters.get.reportOnlyAlarmsInMainScript
    val results = for (s@MethodSummary(_, mdecl, cfgState) <- summaries.values.toList if mustCheck(s))
    yield (mdecl.classDef.typ, mdecl, cfgState)

    // now check if we see anything suspicious
    if (TouchAnalysisParameters.get.reportUnanalyzedFunctions) {
      val unanalyzed = compiler.allMethods.toSet -- summaries.values.map(_.method)
      for (un <- unanalyzed) {
        logger.debug("In ReqFragAnalysis: Did not analyze "+un.name+" (may be unreachable)")
      }
    }

    new SingleStatementProperty(new BottomVisitor()).check(results,output)

    compiler.relevantLibraryFields = spottedFields ++ Set("data", "art", "records", "code")
    SystemParameters.resetOutput()
    //SystemParameters.progressOutput.end()
  }

}

/**
 * This state does nothing but tracking the type of the current expression on the evaluation stack
 * and collecting all field accesses in the form of Type->FieldName. This can be used to perform
 * a very simple pre-analysis to detect the fragment of the library that is to be used in the present
 * program.
 *
 * @param myType the current expression on the evaluation stack
 */
class AccessCollectingState(myType: Type) extends State[AccessCollectingState] with CollectingState {

  def factory(): AccessCollectingState = new AccessCollectingState(SystemParameters.getType().top())

  def isTop = myType.isTop

  def isBottom = myType.isBottom

  def setType(typ: Type): AccessCollectingState = new AccessCollectingState(typ)

  def getType: Type = myType

  def getFieldValue(obj: ExpressionSet, field: String, typ: Type): AccessCollectingState = {
    RequiredLibraryFragmentAnalysis.spottedFields =
      RequiredLibraryFragmentAnalysis.spottedFields +
        (obj.getType().toString + "." + field) +
        obj.getType().toString
    new AccessCollectingState(typ)
  }

  def setExpression(expr: ExpressionSet): AccessCollectingState = this.setType(expr.getType())

  def expr: ExpressionSet = ExpressionSet(new UnitExpression(myType, null))

  def removeExpression(): AccessCollectingState = this.setType(SystemParameters.getType().top())

  def createObject(typ: Type, pp: ProgramPoint): AccessCollectingState = this.setType(typ)

  def evalConstant(value: String, typ: Type, pp: ProgramPoint): AccessCollectingState = this.setType(typ)

  def getVariableValue(id: Assignable): AccessCollectingState = this.setType(id.typ)

  def createCollection(collTyp: Type, keyType: Type, valueType: Type, lengthTyp: Type, keyCollectionTyp: Option[Type], tpp: ProgramPoint, fields: Option[Set[Identifier]] = None) =
    this.setType(collTyp)

  def getSummaryCollectionIfExists(collectionSet: ExpressionSet) = this

  def getCollectionValue(valueIds: ExpressionSet) = this

  def insertCollectionTopElement(collectionSet: ExpressionSet, keyTop: ExpressionSet, valueTop: ExpressionSet, pp: ProgramPoint) = this

  def getCollectionKeyByKey(collectionSet: ExpressionSet, keySet: ExpressionSet) =
    this.setType(collectionSet.getType().asInstanceOf[ACollection].keyType)

  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet) =
    this.setType(collectionSet.getType().asInstanceOf[ACollection].valueType)

  def getCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet) =
    this.setType(collectionSet.getType().asInstanceOf[ACollection].valueType)

  def extractCollectionKeys(fromCollectionSet: ExpressionSet, newKeyValueSet: ExpressionSet, fromCollectionTyp: Type, collTyp: Type, keyType: Type, valueType: Type, lengthTyp: Type, pp: ProgramPoint) = this

  def getOriginalCollection(collectionSet: ExpressionSet) = this

  def getKeysCollection(collectionSet: ExpressionSet) = this

  def removeCollectionKeyConnection(origCollectionSet: ExpressionSet, keyCollectionSet: ExpressionSet) = this

  def copyCollection(fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet) = this

  def insertCollectionElement(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet, pp: ProgramPoint) = this

  def removeCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet) = this

  def removeFirstCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet) = this

  def assignAllCollectionKeys(collectionSet: ExpressionSet, valueSet: ExpressionSet) = this

  def clearCollection(collectionSet: ExpressionSet) = this

  def getCollectionLength(collectionSet: ExpressionSet): AccessCollectingState = this.setType(TNumber)

  def collectionContainsKey(collectionSet: ExpressionSet, keySet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint) = this

  def collectionContainsValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint) = this

  def isSummaryCollection(collectionSet: ExpressionSet) = false

  def pruneVariables(filter: VariableIdentifier => Boolean): AccessCollectingState = this

  def pruneUnreachableHeap(): AccessCollectingState = this

  def optimizeSummaryNodes(): AccessCollectingState = this

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

  def undoPruneVariables(unprunedPreState: AccessCollectingState, filter: (VariableIdentifier) => Boolean): AccessCollectingState = this

  def undoPruneUnreachableHeap(preState: AccessCollectingState): AccessCollectingState = this

  override def getFieldValueWhere(objs: ExpressionSet, field: String, typ: Type, filter: (Identifier, AccessCollectingState) => Boolean): (Set[Identifier], Set[Identifier]) = {
    RequiredLibraryFragmentAnalysis.spottedFields =
      RequiredLibraryFragmentAnalysis.spottedFields +
        (objs.getType().toString + "." + field) +
        objs.getType().toString
    filter(HeapIdentifier.makeDummy(typ),this)
    (Set(HeapIdentifier.makeDummy(typ)),Set(HeapIdentifier.makeDummy(typ)))
  }

  override def merge(r: Replacement): AccessCollectingState = this
}
