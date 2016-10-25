/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ClassDefinition, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.property.{OutputCollector, SingleStatementProperty}
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.td.domain.{HeapIdentifier, TouchStateInterface}
import com.typesafe.scalalogging.LazyLogging


/**
  * Collects all the fields that are accessed in a program. This can be used to
  * optimize the analysis
  *
  * @author Lucas Brutschy
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
      MethodSummaries.collect(method.programpoint, method, AccessCollectingState(SystemParameters.typ.top()), params)

    }

    SystemParameters.resetOutput()
    val summaries = MethodSummaries.getSummaries[AccessCollectingState]
    val mustCheck = (s: MethodSummary[AccessCollectingState]) => s.method.classDef == compiler.main || TouchAnalysisParameters.get.libraryErrorReportingMode == LibraryErrorReportingMode.Report
    val results = for (s@MethodSummary(_, mdecl, cfgState) <- summaries.values.toList if mustCheck(s))
      yield (mdecl.classDef.typ, mdecl, cfgState)

    // now check if we see anything suspicious
    if (TouchAnalysisParameters.get.reportUnanalyzedFunctions) {
      val unanalyzed = compiler.allMethods.toSet -- summaries.values.map(_.method)
      for (un <- unanalyzed) {
        logger.debug("In ReqFragAnalysis: Did not analyze " + un.name + " (may be unreachable)")
      }
    }

    SingleStatementProperty.Default(new BottomVisitor()).check(results, output)

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
case class AccessCollectingState(myType: Type)
  extends State[AccessCollectingState]
    with TouchStateInterface[AccessCollectingState] {

  def factory(): AccessCollectingState = AccessCollectingState(SystemParameters.typ.top())

  def isTop = myType.isTop

  def isBottom = myType.isBottom

  def setType(typ: Type): AccessCollectingState = AccessCollectingState(typ)

  def getType: Type = myType

  def getFieldValue(obj: ExpressionSet, field: String, typ: Type): AccessCollectingState = {
    RequiredLibraryFragmentAnalysis.spottedFields =
      RequiredLibraryFragmentAnalysis.spottedFields +
        (obj.typ.toString + "." + field) +
        obj.typ.toString
    AccessCollectingState(typ)
  }

  def setExpression(expr: ExpressionSet): AccessCollectingState = this.setType(expr.typ)

  def expr: ExpressionSet = ExpressionSet(UnitExpression(myType, null))

  def removeExpression(): AccessCollectingState = this.setType(SystemParameters.typ.top())

  def createObject(typ: Type, pp: ProgramPoint): AccessCollectingState = this.setType(typ)

  def evalConstant(value: String, typ: Type, pp: ProgramPoint): AccessCollectingState = this.setType(typ)

  def getVariableValue(id: Identifier): AccessCollectingState = this.setType(id.typ)

  def pruneVariables(filter: VariableIdentifier => Boolean): AccessCollectingState = this

  def pruneUnreachableHeap(): AccessCollectingState = this

  def testFalse(): AccessCollectingState = this

  def testTrue(): AccessCollectingState = this

  def assume(cond: ExpressionSet): AccessCollectingState = this

  def refiningAssignVariable(oldPreState: AccessCollectingState, x: ExpressionSet, right: ExpressionSet): AccessCollectingState = this

  def refiningGetFieldValue(obj: ExpressionSet, field: String, typ: Type): AccessCollectingState = this

  def refiningGetVariableValue(id: Identifier): AccessCollectingState = this

  def throws(t: ExpressionSet): AccessCollectingState = this

  def removeVariable(x: ExpressionSet): AccessCollectingState = this

  def setVariableToTop(x: ExpressionSet): AccessCollectingState = this

  def setArgument(x: ExpressionSet, right: ExpressionSet): AccessCollectingState = this

  def assignField(obj: ExpressionSet, field: String, right: ExpressionSet): AccessCollectingState = this

  def assignVariable(x: ExpressionSet, right: ExpressionSet): AccessCollectingState = this

  def createVariable(x: ExpressionSet, typ: Type, pp: ProgramPoint): AccessCollectingState = this

  def createVariableForArgument(x: ExpressionSet, typ: Type): AccessCollectingState = this

  def before(pp: ProgramPoint): AccessCollectingState = this

  def bottom(): AccessCollectingState =
    AccessCollectingState(myType.bottom())

  def glb(other: AccessCollectingState): AccessCollectingState =
    AccessCollectingState(getType.glb(other.getType))

  def lessEqual(r: AccessCollectingState): Boolean = myType.lessEqual(r.getType)

  def lub(other: AccessCollectingState): AccessCollectingState =
    AccessCollectingState(getType.lub(other.getType))

  def top(): AccessCollectingState = AccessCollectingState(myType.top())

  def widening(other: AccessCollectingState): AccessCollectingState =
    AccessCollectingState(getType.widening(other.getType))

  def removeObject(oldPreState: AccessCollectingState, obj: ExpressionSet, fields: Option[Set[Identifier]]): AccessCollectingState = this

  def refiningAssignField(oldPreState: AccessCollectingState, obj: ExpressionSet, field: String, right: ExpressionSet): AccessCollectingState = this

  def undoPruneVariables(unprunedPreState: AccessCollectingState, filter: (VariableIdentifier) => Boolean): AccessCollectingState = this

  def undoPruneUnreachableHeap(preState: AccessCollectingState): AccessCollectingState = this

  override def getFieldValueWhere(objs: ExpressionSet, field: String, typ: Type, filter: (Identifier, AccessCollectingState) => Boolean): (Set[Identifier], Set[Identifier]) = {
    RequiredLibraryFragmentAnalysis.spottedFields =
      RequiredLibraryFragmentAnalysis.spottedFields +
        (objs.typ.toString + "." + field) +
        objs.typ.toString
    filter(HeapIdentifier.makeDummy(typ), this)
    (Set(HeapIdentifier.makeDummy(typ)), Set(HeapIdentifier.makeDummy(typ)))
  }

  override def merge(r: Replacement): AccessCollectingState = this

  override def getPossibleConstants(id: Identifier) = SetDomain.Default.Top[Constant]()

  override def ids: IdentifierSet = IdentifierSet.Top

  override def readableFrom(expr: IdentifierSet): IdentifierSet = IdentifierSet.Top

}
