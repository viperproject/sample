package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{Constant, UnitExpression, VariableIdentifier, _}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.domain.MultiValExpression
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._
import RichNativeSemantics._

/**
 *
 * This class defines a richer interface to interact with the current state. This enables us to specify the
 * abstract semantics of many of TouchDevelops native functions in a more natural way.
 *
 * Lucas Brutschy
 * Date: 10/5/12
 * Time: 4:30 PM
 *
 */

object RichNativeSemantics extends RichExpressionImplicits {

  /*-- Checking / Reporting errors --*/

  def Dummy[S <: State[S]](obj: RichExpression, method: String)(implicit state: S, pp: ProgramPoint) {
    val currentClass = SystemParameters.analysisUnitContext.clazzType.toString
    if (TouchAnalysisParameters.reportDummyImplementations &&
      (!TouchAnalysisParameters.reportOnlyAlarmsInMainScript || currentClass.equals(SystemParameters.compiler.asInstanceOf[TouchCompiler].main.toString)))
      Reporter.reportDummy(obj.getType().toString + "->" + method, pp)
  }

  def Dummy[S <: State[S]](text: String)(implicit state: S, pp: ProgramPoint) {
    val currentClass = SystemParameters.analysisUnitContext.clazzType.toString
    if (TouchAnalysisParameters.reportDummyImplementations &&
      (!TouchAnalysisParameters.reportOnlyAlarmsInMainScript || currentClass.equals(SystemParameters.compiler.asInstanceOf[TouchCompiler].main.toString)))
      Reporter.reportDummy(text, pp)
  }

  def Error[S <: State[S]](expr: RichExpression, message: String)(implicit state: S, pp: ProgramPoint): S = {
    if (!state.isInstanceOf[AccessCollectingState]) {
      val errorState = state.assume(expr).setExpression(ExpressionSet(new UnitExpression(SystemParameters.typ.top(), pp)))
      if (!errorState.lessEqual(state.bottom())) {
        val currentClass = SystemParameters.analysisUnitContext.clazzType.toString
        if (!TouchAnalysisParameters.reportOnlyAlarmsInMainScript
          || currentClass.equals(SystemParameters.compiler.asInstanceOf[TouchCompiler].main.toString)) {
          Reporter.reportError(message, pp, state.explainError(expr))
        }
        val ret = state.assume(expr.not())
        if (ret.lessEqual(ret.bottom())) return ret.bottom()
        ret
      } else state
    } else state
  }

  def Error[S <: State[S]](expr: RichExpression, method: String, message: String)(implicit state: S, pp: ProgramPoint): S = {
    Error[S](expr, "When calling " + method + ": " + message)
  }

  def CheckInRangeInclusive[S <: State[S]](expr: RichExpression, low: RichExpression, high: RichExpression, method: String, parameter: String)(implicit s: S, pp: ProgramPoint): S = {
    if (TouchAnalysisParameters.printValuesInWarnings) {
      val state1 = Error(expr < low, method + ": Parameter " + parameter + " (" + expr + ") may be less than the lowest allowed value (" + low + ")")(s, pp)
      Error(expr > high, method + ": Parameter " + parameter + " (" + expr + ") may be greater than the highest allowed value " + high + ")")(state1, pp)
    } else {
      val state1 = Error(expr < low, method + ": Parameter " + parameter + " may be less than the lowest allowed value")(s, pp)
      Error(expr > high, method + ": Parameter " + parameter + " may be greater than the highest allowed value")(state1, pp)
    }
  }

  def CheckNonNegative[S <: State[S]](expr: RichExpression, method: String, parameter: String)(implicit s: S, pp: ProgramPoint): S = {
    if (TouchAnalysisParameters.printValuesInWarnings)
      Error(expr < 0, method + ": Parameter " + parameter + " (" + expr + ") may be negative")(s, pp)
    else
      Error(expr < 0, method + ": Parameter " + parameter + " may be negative")(s, pp)
  }

  def If[S <: State[S]](expr: RichExpression, Then: S => S, Else: S => S)(implicit state: S, pp: ProgramPoint): S = {
    val thenState = state.assume(expr)
    val elseState = state.assume(expr.not())

    if (thenState.lessEqual(state.bottom())) {
      Else(elseState)
    } else if (elseState.lessEqual(state.bottom())) {
      Then(thenState)
    } else {
      val thenRes = Then(thenState)
      val elseRes = Else(elseState)
      val res = thenRes.lub(elseRes)
      res
    }
  }

  /**
   * Creates a new Object of type typ, and initializes its fields with the given arguments.
   */
  def New[S <: State[S]](typ: TouchType,
                         initials: Map[TouchField, RichExpression] = Map.empty[TouchField, RichExpression],
                         initializeFields: Boolean = true,
                         createFields: Boolean = true,
                         initialCollectionSize: Option[RichExpression] = None,
                         initialCollectionValue: Option[RichExpression] = None)(implicit s: S, pp: ProgramPoint): S = {

    typ.name match {
      case TNumber.typeName => s.setExpression(ExpressionSet(Constant("0", TNumber, pp)))
      case TBoolean.typeName => s.setExpression(new ExpressionSet(TBoolean).add(False))
      case TString.typeName => s.setExpression(ExpressionSet(Constant("", TString, pp)))
      case _ =>

        val fields =
          if (TouchAnalysisParameters.libraryFieldPruning && (initializeFields || createFields)) {
            val relFields = SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields
            val typFields = typ.possibleTouchFields
            Some(typFields
              .filter({ f: TouchField => relFields.contains(typ.toString + "." + f.getName)})
              .toSet[Identifier])
          }
          else if (!initializeFields) Some(Set.empty[Identifier])
          else None

        var curState = s.createObject(typ, pp, fields)
        val obj = curState.expr

        if (initializeFields) {
          typ match {
            case col: ACollection =>
              initialCollectionValue match {
                case None =>
                // Remains bottom
                case Some(x) =>
                  //TODO: Can be more precise
                  curState = Top[S](SystemParameters.compiler.asInstanceOf[TouchCompiler].getType(col.keyTypeName))(curState, pp)
                  curState = col.collectionInsert[S](obj, curState.expr, x)(curState, pp)
              }

              // Initialize collection size
              curState = col.collectionSetSize[S](obj, initialCollectionSize.getOrElse(0))(curState, pp)
            case _ => ()
          }

          // Assign fields with given arguments
          for (f <- typ.possibleTouchFields) {
            if (!TouchAnalysisParameters.libraryFieldPruning ||
              SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.toString + "." + f.getName)) {
              val (newPP, referenceLoop) = DeepeningProgramPoint(pp, f.getName)
              val a = initials.get(f) match {
                case None => f.default match {
                  case InvalidInitializer(r) =>
                    Invalid(f.typ, r)
                  case TopInitializer =>
                    curState = Top[S](f.typ, createFields = !referenceLoop, initializeFields = !referenceLoop)(curState, newPP)
                    toRichExpression(curState.expr)
                  case TopWithInvalidInitializer(r) =>
                    curState = TopWithInvalid[S](f.typ, r, initializeFields = !referenceLoop)(curState, newPP)
                    toRichExpression(curState.expr)
                  case NewInitializer =>
                    curState = New[S](f.typ, createFields = !referenceLoop, initializeFields = !referenceLoop)(curState, newPP)
                    toRichExpression(curState.expr)
                  case ExpressionInitializer(e) => e
                }
                case Some(st) => st
              }
              curState = AssignField[S](obj, f, a)(curState, pp)
            }
          }
        }

        curState.setExpression(obj)
    }
  }

  def Top[S <: State[S]](typ: TouchType,
                         initials: Map[Identifier, RichExpression] = Map.empty[Identifier, RichExpression],
                         createFields: Boolean = true,
                         initializeFields: Boolean = true,
                         initialCollectionSize: Option[RichExpression] = None)
                        (implicit s: S, pp: ProgramPoint): S = {
    typ.typeName match {
      case TNumber.typeName => s.setExpression(Valid(TNumber))
      case TBoolean.typeName => s.setExpression(new ExpressionSet(TBoolean).add(True).add(False))
      case TString.typeName => s.setExpression(Valid(TString))
      case _ =>
        val fields =
          if (TouchAnalysisParameters.libraryFieldPruning && (createFields || initializeFields)) {
            val relFields = SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields
            val typFields = typ.possibleTouchFields
            Some(typFields
              .filter({ f: TouchField => relFields.contains(typ.toString + "." + f.getName)})
              .map(_.asInstanceOf[Identifier]))
          }
          else if (!initializeFields) Some(Set.empty[Identifier])
          else None

        var curState = s.createObject(typ, pp, fields)
        val obj = curState.expr

        // TODO: Support for recursive datastructures
//        val tempVariable = VariableIdentifier("initVar"+pp.toString)(typ,pp)
//        curState = s.createVariable(toRichExpression(tempVariable),typ,pp)
//        curState = s.assignVariable(toRichExpression(tempVariable),obj)

        if (initializeFields) {
          typ match {
            case col: ACollection =>

              val (newPP1, referenceLoop1) = DeepeningProgramPoint(pp, "colEntry_" + col.entryType.typeName)
              curState = Top[S](col.entryType, initializeFields = !referenceLoop1)(curState, newPP1)
              val entryTop = curState.expr

              curState = AssignField[S](obj,col.field_entry,entryTop)

              // Initialize collection size
              initialCollectionSize match {
                case None =>
                  val (newPP3, referenceLoop3) = DeepeningProgramPoint(pp, "__length")
                  curState = Top[S](TNumber, initializeFields = !referenceLoop3)(curState, newPP3)
                  val lengthTop = curState.expr
                  curState = col.collectionSetSize[S](obj, lengthTop)(curState, newPP3)
                case Some(x) =>
                  curState = col.collectionSetSize[S](obj, x)(curState, pp)
              }

            case _ => ()
          }

          // Assign fields with given arguments
          for (f <- typ.possibleTouchFields) {
            if (!TouchAnalysisParameters.libraryFieldPruning ||
              SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.toString + "." + f.getName)) {
              val (newPP, referenceLoop) = DeepeningProgramPoint(pp, f.getName)
              val a = initials.get(f) match {
                case None => f.topDefault match {
                  case InvalidInitializer(r) => Invalid(f.typ, r)
                  case TopInitializer => curState = Top[S](f.typ, initializeFields = !referenceLoop)(curState, newPP); toRichExpression(curState.expr)
                  case TopWithInvalidInitializer(r) => curState = TopWithInvalid[S](f.typ, r, initializeFields = !referenceLoop)(curState, newPP); toRichExpression(curState.expr)
                  case NewInitializer => curState = New[S](f.typ, initializeFields = !referenceLoop)(curState, newPP); toRichExpression(curState.expr)
                  case ExpressionInitializer(e) => e
                }
                case Some(st) => st
              }
              curState = AssignField[S](obj, f, a)(curState, pp)
            }
          }
        }

        curState.setExpression(obj)

    }
  }

  def TopWithInvalid[S <: State[S]](typ: TouchType,
                                    invalidCause: String,
                                    initials: Map[Identifier, RichExpression] = Map.empty[Identifier, RichExpression],
                                    createFields: Boolean = true,
                                    initializeFields: Boolean = true)(implicit s: S, pp: ProgramPoint): S = {

    val curState = Top[S](typ, initials, createFields = createFields, initializeFields = initializeFields)(s, pp)
    val validResult = curState.expr
    Return[S](validResult, Invalid(typ, invalidCause))(curState, pp)

  }

  def Clone[S <: State[S]](obj: RichExpression, initials: Map[Identifier, RichExpression] = Map.empty[Identifier, RichExpression], recursive: Boolean = true)(implicit s: S, pp: ProgramPoint): S = {

    val touchTyp = obj.getType().asInstanceOf[TouchType]

    // Never clone immutable types where we don't change the fields. This includes all primitives
    if (touchTyp.isImmutable && initials.isEmpty) {
      return Return[S](obj)
    }

    var curState = New[S](touchTyp, initializeFields = false)(s, pp)
    val newObject = toRichExpression(curState.expr)

    // Clone fields

    if (obj.getType().isInstanceOf[ACollection]) {
      curState = curState.copyCollection(obj, newObject)
    }

    for (f <- obj.getType().asInstanceOf[TouchType].possibleTouchFields) {
      if (!TouchAnalysisParameters.libraryFieldPruning ||
        SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(obj.getType().toString + "." + f.getName)) {
        initials.get(f) match {
          case None =>
            if (recursive) {
              val (newPP, referenceLoop) = DeepeningProgramPoint(pp, f.getName)
              val oldField = Field[S](obj, f)(curState, newPP)
              curState = Clone[S](oldField, recursive = !referenceLoop)(curState, newPP)
              val clonedContent = curState.expr
              curState = AssignField[S](newObject, f, clonedContent)(curState, newPP)
            } else {
              val oldField = Field[S](obj, f)(curState, pp)
              curState = AssignField[S](newObject, f, oldField)(curState, pp)
            }
          case Some(st) =>
            curState = AssignField[S](newObject, f, st)(curState, pp)
        }
      }
    }

    curState.setExpression(newObject)

  }

  private def getKeyCollectionTyp(colTyp: ACollection) = colTyp match {
    case TString_Map => Some(TString_Collection)
    case TNumber_Map => Some(TNumber_Collection)
    case TJson_Object => Some(TString_Collection)
    case _ => None
  }

  /*-- Collections --*/

  def CollectionSize[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    Field[S](collection,collection.getType().asInstanceOf[ACollection].field_count)
  }

  def CollectionContainsValue[S <: State[S]](collection: RichExpression, value: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    if (state.assume(CollectionSize[S](collection) > 0).lessEqual(state.bottom())) return False
    return state.collectionContainsValue(collection, value, TBoolean, pp).expr
  }

  def KeyCollectionContainsValue[S <: State[S]](collection: RichExpression, value: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    var newState = Top[S](collection.getType().asInstanceOf[ACollection].keyType)(state, pp)
    val keyTop = newState.expr
    var expression = collection contains(keyTop, value, pp)

    val origCollection = newState.getOriginalCollection(collection).expr
    if (!origCollection.lessEqual(origCollection.bottom()) && origCollection.getType().isInstanceOf[ACollection]) {
      newState = Top[S](origCollection.getType().asInstanceOf[ACollection].valueType)
      val valueTop = newState.expr
      expression = expression && (origCollection contains(value, valueTop, pp))
    }

    newState.setExpression(expression)
  }

  def CollectionContainsKey[S <: State[S]](collection: RichExpression, key: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    if (state.assume(CollectionSize[S](collection) > 0).lessEqual(state.bottom())) return False
    return state.collectionContainsKey(collection, key, TBoolean, pp).expr
  }

  def CollectionExtractKeys[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    val collectionTyp = collection.getType().asInstanceOf[ACollection]
    val keyTyp = collectionTyp.keyType

    val newCollectionTyp = getKeyCollectionTyp(collectionTyp) match {
      case Some(x) => x
      case None => throw new SemanticException("keys() operation is not supported for that object")
    }

    var newState = state.extractCollectionKeys(collection, 0 ndTo (CollectionSize[S](collection) - 1), collectionTyp, newCollectionTyp, TNumber, keyTyp, TNumber, pp)
    val newCollection = newState.expr
    // Make sure that our value is "valid"  now
    newState = newState.assignVariable(newCollection, Valid(newCollectionTyp))
    newState = newState.assignField(newCollection, "orig", collection)
    newState = newState.assignField(collection, "keys", newCollection)


    newState.setExpression(newCollection)
  }

  def CollectionInvalidateKeys[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    state.assignAllCollectionKeys(collection, 0 ndTo CollectionSize[S](collection) - 1)
  }

  def CollectionClear[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    var newState = state.clearCollection(collection)
    newState = Assign[S](CollectionSize[S](collection), 0)(newState, pp)

    val res = if (newState.isSummaryCollection(collection)) {
      newState.lub(state)
    } else {
      newState
    }

    res
  }

  def CollectionUpdate[S <: State[S]](collection: RichExpression, key: RichExpression, value: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    val newState = CollectionRemove[S](collection, key)(state, pp)
    CollectionInsert[S](collection, key, value)(newState, pp)
  }

  def CollectionInsert[S <: State[S]](collection: RichExpression, index: RichExpression, right: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    var result = state.insertCollectionElement(collection, index, right, pp)

    val originalCollection = result.getOriginalCollection(collection).expr
    if (!originalCollection.lessEqual(originalCollection.bottom()) && originalCollection.getType().isInstanceOf[ACollection]) {
      result = result.removeCollectionKeyConnection(originalCollection, collection)
    }
    result
  }

  def CollectionRemove[S <: State[S]](collection: RichExpression, index: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    var result = state.removeCollectionValueByKey(collection, index)
    val keysCollection = result.getKeysCollection(collection).expr
    if (!keysCollection.lessEqual(keysCollection.bottom()) && keysCollection.getType().isInstanceOf[ACollection]) {
      result = result.removeCollectionKeyConnection(collection, keysCollection)
    }
    result
  }

  def CollectionRemoveFirst[S <: State[S]](collection: RichExpression, value: RichExpression)(implicit state: S, pp: ProgramPoint) = {
    state.removeFirstCollectionValueByValue(collection, value)
  }

  def CollectionIndexInRange[S <: State[S]](collection: RichExpression, index: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    index >= 0 && index < CollectionSize[S](collection)
  }

  def CollectionAt[S <: State[S]](collection: RichExpression, key: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    state.getCollectionValueByKey(collection, key).expr
  }

  /*-- Misc --*/

  def Assume[S <: State[S]](expr: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    state.assume(expr)
  }

  def CallLocalAction[S <: State[S]](method: String, parameters: List[ExpressionSet] = Nil)
                                    (implicit state: S, pp: ProgramPoint): S = {
    val context = SystemParameters.analysisUnitContext
    val classType = context.clazzType
    SystemParameters.compiler.asInstanceOf[TouchCompiler].getMethodWithClassDefinition(method, classType, parameters map (_.getType())) match {
      case Some(mdecl) =>
        val res = MethodSummaries.collect(pp, mdecl, state, parameters)
        res
      case _ =>
        Reporter.reportImprecision("Could not find method " + method, pp)
        state.top()
    }
  }

  def CallApi[S <: State[S]](obj: RichExpression, method: String, parameters: List[ExpressionSet] = Nil, returnedType: TouchType)(implicit state: S, pp: ProgramPoint): S = {
    obj.getType().asInstanceOf[AAny].forwardSemantics(obj, method, parameters, returnedType)(pp, state)
  }

  def Return[S <: State[S]](e: RichExpression*)(implicit state: S, pp: ProgramPoint): S = {
    lazy val typ = e.head.getType()
    var set = new ExpressionSet(typ)
    for (ex <- e) {
      set = set.add(ex.thisExpr)
    }
    val stateN = state.setExpression(set)
    stateN
  }

  def Assign[S <: State[S]](id: RichExpression, value: RichExpression)(implicit state: S, pp: ProgramPoint): S = {

    def join(a: List[ExpressionSet], b: List[ExpressionSet]): List[ExpressionSet] = {
      a match {
        case x :: xs =>
          b match {
            case y :: ys => x.add(y) :: join(xs, ys)
            case Nil => a
          }
        case Nil => b
      }
    }

    def getMultiValAsList(expr: RichExpression): List[ExpressionSet] = {
      var ret: List[ExpressionSet] = Nil
      for (sExpr <- expr.thisExpr.getSetOfExpressions) yield {
        sExpr match {
          case MultiValExpression(left, right, retVal) =>
            val multiVal = getMultiValAsList(left) ::: getMultiValAsList(right)
            ret = join(ret, multiVal)
          case _ =>
            ret = join(ret, List(new ExpressionSet(expr.getType).add(expr)))
        }
      }
      ret
    }

    val leftExprs = getMultiValAsList(id)
    val rightExprs = getMultiValAsList(value)

    if (leftExprs.length != rightExprs.length) {
      Reporter.reportImprecision("A multival assignment has an unmatching number of values - going to top", pp)
      return state.top()
    }

    var curState = state
    for ((l, r) <- leftExprs.zip(rightExprs)) {

      // Create variable that might not exist yet
      if (l.getSetOfExpressions.size == 1 && l.getSetOfExpressions.head.isInstanceOf[VariableIdentifier]) {
        curState = curState.createVariable(l, l.getType(), pp)
      }

      curState = curState.assignVariable(l, r)
    }
    curState

  }

  /*-- Reading and writing of fields --*/

  def AssignField[S <: State[S]](obj: RichExpression, field: Identifier, value: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    AssignField[S](obj, field.getName, value)
  }

  def AssignField[S <: State[S]](obj: RichExpression, field: String, value: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    if (!TouchAnalysisParameters.libraryFieldPruning ||
      SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(obj.getType().toString + "." + field)) {

      if (TouchAnalysisParameters.topFields.contains(field)) {
        state.assignField(obj, field, Valid(value.getType()))
      } else {
        state.assignField(obj, field, value)
      }

    } else state
  }

  def Field[S <: State[S]](obj: RichExpression, field: TouchField)(implicit state: S, pp: ProgramPoint): RichExpression = {
    obj.thisExpr.getType().asInstanceOf[AAny].forwardSemantics(obj,field.getName,Nil,field.typ).expr
  }

  /*-- Skipping --*/

  def Exit[S <: State[S]](implicit state: S, pp: ProgramPoint): S = MethodSummaries.collectExit[S](state)

  def Skip[S <: State[S]](implicit state: S, pp: ProgramPoint): S = state.removeExpression()

  def Unimplemented[S <: State[S]](method: String)(implicit state: S, pp: ProgramPoint): S = {
    Reporter.reportImprecision(method + " not implemented, going to top", pp)
    state.top()
  }

}

case class TouchField(
                  name: String,
                  typeName: TypeName,
                  default: Initializer = NewInitializer,
                  topDefault: Initializer = TopInitializer,
                  isSummaryNode: Boolean = false)
  extends Identifier {

  val pp = null

  def typ = SystemParameters.compiler.asInstanceOf[TouchCompiler].getType(typeName)

  override def getName = name.toString

  override def toString = name.toString

  override def getField = Some(name)

  override def hashCode(): Int = name.hashCode() + typeName.hashCode()

  override def representsSingleVariable = !isSummaryNode
}

trait Initializer

case class InvalidInitializer(invalidReason: String) extends Initializer

case object NewInitializer extends Initializer

case object TopInitializer extends Initializer

case class TopWithInvalidInitializer(invalidReason: String) extends Initializer

case class ExpressionInitializer(e: RichExpression) extends Initializer
