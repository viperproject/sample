package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.{SystemParameters, Reporter}
import ch.ethz.inf.pm.td.compiler._
import collection.immutable.Range.Inclusive
import RichNativeSemantics._
import scala.Some
import ch.ethz.inf.pm.td.compiler.TouchCollection
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.abstractdomain.UnitExpression
import ch.ethz.inf.pm.td.analysis.{MethodSummaries, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.domain.MultiValExpression

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

object RichNativeSemantics {

  /*-- Checking / Reporting errors --*/

  def Dummy[S <: State[S]](obj:RichExpression, method:String)(implicit state:S, pp:ProgramPoint) {
    if(TouchAnalysisParameters.reportDummyImplementations &&
      (!TouchAnalysisParameters.reportOnlyAlarmsInMainScript || SystemParameters.currentClass.toString.equals(SystemParameters.compiler.asInstanceOf[TouchCompiler].main.typ.toString)))
      Reporter.reportDummy(obj.getType().toString+"->"+method,pp)
  }

  def Dummy[S <: State[S]](text:String)(implicit state:S, pp:ProgramPoint) {
    if(TouchAnalysisParameters.reportDummyImplementations &&
      (!TouchAnalysisParameters.reportOnlyAlarmsInMainScript || SystemParameters.currentClass.toString.equals(SystemParameters.compiler.asInstanceOf[TouchCompiler].main.typ.toString)))
      Reporter.reportDummy(text,pp)
  }

  def Error[S <: State[S]](expr:RichExpression, message:String)(implicit state:S, pp:ProgramPoint):S = {
    val errorState = state.assume( expr ).setExpression(new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(SystemParameters.typ.top(),pp)))
    if(!errorState.lessEqual(state.bottom())) {
      if (!TouchAnalysisParameters.reportOnlyAlarmsInMainScript || SystemParameters.currentClass.toString.equals(SystemParameters.compiler.asInstanceOf[TouchCompiler].main.typ.toString))
        Reporter.reportError(message,pp)
      state.assume(expr.not())
    } else state
  }

  def Error[S <: State[S]](expr:RichExpression, method:String, message:String)(implicit state:S, pp:ProgramPoint):S = {
    Error[S](expr,"When calling "+method+": "+message)
  }

  def CheckInRangeInclusive[S <: State[S]](expr:RichExpression, low:RichExpression, high:RichExpression, method:String, parameter:String)(implicit s:S, pp:ProgramPoint):S = {
    if(TouchAnalysisParameters.printValuesInWarnings) {
      val state1 = Error(expr < low,method+": Parameter "+parameter+" ("+expr+") may be less than the lowest allowed value ("+low+")")(s,pp)
      Error(expr > high,method+": Parameter "+parameter+" ("+expr+") may be greater than the highest allowed value "+high+")")(state1,pp)
    } else {
      val state1 = Error(expr < low,method+": Parameter "+parameter+" may be less than the lowest allowed value")(s,pp)
      Error(expr > high,method+": Parameter "+parameter+" may be greater than the highest allowed value")(state1,pp)
    }
  }

  def CheckNonNegative[S <: State[S]](expr:RichExpression, method:String, parameter:String)(implicit s:S, pp:ProgramPoint):S = {
    if(TouchAnalysisParameters.printValuesInWarnings)
      Error(expr < 0,method+": Parameter "+parameter+" ("+expr+") may be negative")(s,pp)
    else
      Error(expr < 0,method+": Parameter "+parameter+" may be negative")(s,pp)
  }

  def If[S <: State[S]](expr:RichExpression, Then: S => S, Else: S => S)(implicit state:S, pp:ProgramPoint):S = {
    val thenState = state.assume( expr )
    val elseState = state.assume( expr.not() )

    if (thenState.lessEqual(state.bottom())){
      Else(elseState)
    } else if (elseState.lessEqual(state.bottom())) {
      Then(thenState)
    } else {
      val thenRes = Then(thenState)
      val elseRes = Else(elseState)
      val res = state.lub(thenRes,elseRes)
      res
    }
  }

  /**
   * Creates a new Object of type typ, and initializes its fields with the given arguments.
   */
  def New[S <: State[S]](typ:TouchType,
                         initials:Map[TouchField,RichExpression] = Map.empty[TouchField,RichExpression],
                         initializeFields:Boolean = true,
                         createFields:Boolean = true,
                         initialCollectionSize: Option[RichExpression] = None,
                         initialCollectionValue: Option[RichExpression] = None)(implicit s:S, pp:ProgramPoint): S = {

    typ.getName match {
      case TNumber.typName => s.setExpression(new ExpressionSet(TNumber.typ).add(Constant("0",TNumber.typ,pp)))
      case TBoolean.typName => s.setExpression(new ExpressionSet(TBoolean.typ).add(False))
      case TString.typName => s.setExpression(new ExpressionSet(TString.typ).add(Constant("",TString.typ,pp)))
      case _ =>

        val fields =
          if (TouchAnalysisParameters.libraryFieldPruning && (initializeFields || createFields)) {
            val relFields = SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields
            val typFields = typ.getPossibleTouchFields()
            Some(typFields
              .filter({ f:TouchField => relFields.contains(typ.toString()+"."+f.getName())})
              .map(_.asInstanceOf[Identifier]))
          }
          else if (!initializeFields) Some(Set.empty[Identifier])
          else None

        var curState = typ match {
          case col:TouchCollection =>
            s.createCollection(col,col.getKeyType,col.getValueType,TNumber.typ,pp,fields)
          case _ =>
            s.createObject(typ,pp,fields)
        }
        val obj = curState.getExpression()

        if(initializeFields) {
          typ match {
            case col:TouchCollection =>
              initialCollectionValue match {
                case None =>
                // Remains bottom
                case Some(x) =>
                  //TODO: Can be more precise
                  curState = Top[S](col.getKeyType)(curState, pp)
                  curState = CollectionInsert[S](obj, curState.getExpression(), x)(curState, pp)
              }

              // Initialize collection size
              initialCollectionSize match {
                case None =>
                  curState = Assign[S](CollectionSize[S](obj),0)(curState,pp)
                case Some(x) =>
                  curState = Assign[S](CollectionSize[S](obj),x)(curState,pp)
              }
            case _ => ()
          }

          // Assign fields with given arguments
          for (f <- typ.getPossibleTouchFields()) {
            if(!TouchAnalysisParameters.libraryFieldPruning ||
              SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.toString()+"."+f.getName())) {
              val (newPP, referenceLoop) = DeepeningProgramPoint(pp,f.getName())
              val a = initials.get(f) match {
                case None => f.default match {
                  case InvalidInitializer() =>
                    Invalid(f.getType())
                  case TopInitializer() =>
                    curState = Top[S](f.getType(),createFields = !referenceLoop,initializeFields = !referenceLoop)(curState,newPP)
                    toRichExpression(curState.getExpression())
                  case TopWithInvalidInitializer() =>
                    curState = TopWithInvalid[S](f.getType(),initializeFields = !referenceLoop)(curState,newPP)
                    toRichExpression(curState.getExpression())
                  case NewInitializer() =>
                    curState = New[S](f.getType(),createFields = !referenceLoop,initializeFields = !referenceLoop)(curState,newPP)
                    toRichExpression(curState.getExpression())
                  case ExpressionInitializer(e) => e
                }
                case Some(st) => st
              }
              curState = curState.assignField(List(obj),f.getName(),a)
            }
          }
        }

        // Make sure that our value is "valid"  now
        curState = curState.assignVariable(obj,Valid(typ))

        curState.setExpression(obj)
    }
  }

  def Top[S <: State[S]](typ:TouchType,
                         initials:Map[Identifier,RichExpression] = Map.empty[Identifier,RichExpression],
                         createFields:Boolean = true,
                         initializeFields:Boolean = true,
                         initialCollectionSize: Option[RichExpression] = None)
                        (implicit s:S, pp:ProgramPoint): S = {
    typ.getName match {
      case TNumber.typName => s.setExpression(Valid(TNumber.typ))
      case TBoolean.typName => s.setExpression(new ExpressionSet(TBoolean.typ).add(True).add(False))
      case TString.typName => s.setExpression(Valid(TString.typ))
      case _ =>

        val fields =
          if (TouchAnalysisParameters.libraryFieldPruning && (createFields || initializeFields)) {
            val relFields = SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields
            val typFields = typ.getPossibleTouchFields()
            Some(typFields
              .filter({ f:TouchField => relFields.contains(typ.toString()+"."+f.getName())})
              .map(_.asInstanceOf[Identifier]))
          }
          else if (!initializeFields) Some(Set.empty[Identifier])
          else None

        var curState = typ match {
          case col:TouchCollection =>
            s.createCollection(col, col.getKeyType, col.getValueType, TNumber.typ, pp, fields)
          case _ =>
            s.createObject(typ,pp,fields)
        }
        val obj = curState.getExpression()

        // Make sure that our value is "valid"  now
        curState = curState.assignVariable(obj,Valid(typ))

        if(initializeFields) {
          typ match {
            case col:TouchCollection =>

              val (newPP1, referenceLoop1) = DeepeningProgramPoint(pp,"__collkey")
              curState = Top[S](col.getKeyType, initializeFields = !referenceLoop1)(curState, newPP1)
              val keyTop = curState.getExpression()

              val (newPP2, referenceLoop2) = DeepeningProgramPoint(pp,"__collvalue")
              curState = Top[S](col.getValueType, initializeFields = !referenceLoop2)(curState, newPP2)
              val valueTop = curState.getExpression()

              curState = CollectionInsert[S](obj, keyTop, valueTop)(curState, pp)

              // Initialize collection size
              initialCollectionSize match {
                case None =>
                  val (newPP3, referenceLoop3) = DeepeningProgramPoint(pp,"__length")
                  curState = Top[S](TNumber.typ, initializeFields = !referenceLoop3)(curState, newPP3)
                  val lengthTop = curState.getExpression()
                  curState = Assign[S](CollectionSize[S](obj), lengthTop)(curState, newPP3)
                case Some(x) =>
                  curState = Assign[S](CollectionSize[S](obj), x)(curState,pp)
              }

            case _ => ()
          }

          // Assign fields with given arguments
          for (f <- typ.getPossibleTouchFields()) {
            if(!TouchAnalysisParameters.libraryFieldPruning ||
              SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.toString()+"."+f.getName())) {
              val (newPP, referenceLoop) = DeepeningProgramPoint(pp,f.getName())
              val a = initials.get(f) match {
                case None => f.topDefault match {
                  case InvalidInitializer() => Invalid(f.getType())
                  case TopInitializer() => curState = Top[S](f.getType(),initializeFields = !referenceLoop)(curState,newPP); toRichExpression(curState.getExpression())
                  case TopWithInvalidInitializer() => curState = TopWithInvalid[S](f.getType(),initializeFields = !referenceLoop)(curState,newPP); toRichExpression(curState.getExpression())
                  case NewInitializer() => curState = New[S](f.getType(),initializeFields = !referenceLoop)(curState,newPP); toRichExpression(curState.getExpression())
                  case ExpressionInitializer(e) => e
                }
                case Some(st) => st
              }
              curState = curState.assignField(List(obj),f.getName(),a)
            }
          }
        }

        curState.setExpression(obj)

    }
  }

  def TopWithInvalid[S <: State[S]](typ:TouchType,
                                    initials:Map[Identifier,RichExpression] = Map.empty[Identifier,RichExpression],
                                    createFields:Boolean = true,
                                    initializeFields:Boolean = true)(implicit s:S, pp:ProgramPoint): S = {

    val curState = Top[S](typ,initials,createFields = createFields, initializeFields = initializeFields)(s,pp)
    val validResult = curState.getExpression()
    Return[S](validResult,Invalid(typ))(curState,pp)

  }

  def Clone[S <: State[S]](obj:RichExpression, initials:Map[Identifier,RichExpression] = Map.empty[Identifier,RichExpression], recursive : Boolean = true)(implicit s:S, pp:ProgramPoint): S = {

    val touchTyp = obj.getType().asInstanceOf[TouchType]

    // Never clone immutable types where we don't change the fields. This includes all primitives
    if (touchTyp.isImmutable && initials.isEmpty) {
      return Return[S](obj)
    }

    var curState = New[S](touchTyp,initializeFields = false)(s,pp)
    val newObject = toRichExpression(curState.getExpression())

    // Clone fields

    if (obj.getType().isInstanceOf[TouchCollection]) {
      val collTyp = obj.getType().asInstanceOf[TouchCollection]
      curState = curState.copyCollection(obj, newObject, collTyp.getKeyType, collTyp.getValueType)
      curState = Assign[S](CollectionSize[S](newObject), CollectionSize[S](obj))(curState, pp)
    }

    for (f <- obj.getType().asInstanceOf[TouchType].getPossibleTouchFields()) {
      if(!TouchAnalysisParameters.libraryFieldPruning ||
        SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(obj.getType().toString+"."+f.getName())) {
        initials.get(f) match {
          case None =>
            if(recursive) {
              val (newPP, referenceLoop) = DeepeningProgramPoint(pp,f.getName())
              val oldField = Field[S](obj,f)(curState,newPP)
              curState = Clone[S](oldField,recursive = !referenceLoop)(curState,newPP)
              val clonedContent = curState.getExpression()
              curState = curState.assignField(List(newObject),f.getName(),clonedContent)
            } else {
              val oldField = Field[S](obj,f)(curState,pp)
              curState = curState.assignField(List(newObject),f.getName(),oldField)
            }
          case Some(st) =>
            curState = curState.assignField(List(newObject),f.getName(),st)
        }
      }
    }

    curState.setExpression(newObject)

  }

  /*-- Collections --*/

  def CollectionSize[S <: State[S]](collection:RichExpression)(implicit state:S, pp:ProgramPoint):RichExpression = {
    state.getCollectionLength(collection).getExpression()
  }

  def CollectionAt[S <: State[S]](collection:RichExpression,key:RichExpression)(implicit state:S, pp:ProgramPoint):RichExpression = {
    state.getCollectionValueByKey(collection, key, collection.getType().asInstanceOf[TouchCollection].getValueType).getExpression()
  }

  def CollectionContainsValue[S <: State[S]](collection: RichExpression, value: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    if(state.assume(CollectionSize[S](collection) > 0).lessEqual(state.bottom())) return False

    val expressions = state.getCollectionValueByValue(collection, value).getExpression()
    if (expressions.isBottom) {
      return False
    } else {
      return True or False
    }
  }

  def CollectionContainsKey[S <: State[S]](collection: RichExpression, key: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    if(state.assume(CollectionSize[S](collection) > 0).lessEqual(state.bottom())) return False

    val expressions = state.getCollectionValueByKey(collection, key, collection.getType().asInstanceOf[TouchCollection].getValueType).getExpression()
    if (expressions.isBottom) {
      return False
    } else {
      return True or False
    }
  }

  def CollectionSummary[S <: State[S]](collection:RichExpression)(implicit state:S, pp:ProgramPoint):RichExpression = {
    val keyTyp = collection.thisExpr._1.asInstanceOf[TouchCollection].getKeyType
    val valueTyp = collection.thisExpr._1.asInstanceOf[TouchCollection].getValueType
    state.getCollectionValueByKey(collection, Valid(keyTyp), valueTyp).getExpression()
  }

  def CollectionKeySummary[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    val result = state.getCollectionKeyByKey(collection, Valid(collection.getType().asInstanceOf[TouchCollection].getKeyType))
    result.getExpression()
  }

  def CollectionExtractKeys[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    val keyTyp = collection.getType().asInstanceOf[TouchCollection].getKeyType
    val collectionTyp = collection.getType()
    val newCollectionTyp = collectionTyp.getName() match {
      case TNumber_Map.typName => TNumber_Collection.typ
      case TString_Map.typName => TString_Collection.typ
    }

    var newState = state.extractCollectionKeys(collection, 0 ndTo (CollectionSize[S](collection) - 1), newCollectionTyp, TNumber.typ, keyTyp, TNumber.typ, pp)

    // Make sure that our value is "valid"  now
    val expression = newState.getExpression()
    newState = newState.assignVariable(expression, Valid(newCollectionTyp))
    newState.setExpression(expression)
  }

  def CollectionInvalidateKeys[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    val keyTyp = collection.thisExpr._1.asInstanceOf[TouchCollection].getKeyType
    state.assignAllCollectionKeys(collection, 0 ndTo CollectionSize[S](collection) - 1, keyTyp)
  }

  def CollectionClear[S <: State[S]](collection:RichExpression)(implicit state:S, pp:ProgramPoint):S = {
    val keyTyp = collection.thisExpr._1.asInstanceOf[TouchCollection].getKeyType
    val valueTyp = collection.thisExpr._1.asInstanceOf[TouchCollection].getValueType
    var newState = state.clearCollection(collection, keyTyp, valueTyp)
    newState = Assign[S](CollectionSize[S](collection), 0)(newState, pp)
    newState
//    if (newState.isSummaryCollection(collection)) {
//      return newState.lub(newState, state)
//    } else {
//      return newState
//    }
  }

  def CollectionUpdate[S <: State[S]](collection: RichExpression, key:RichExpression, value: RichExpression)(implicit state:S, pp:ProgramPoint): S = {
    val newState = CollectionRemove[S](collection, key)(state, pp)
    CollectionInsert[S](collection, key, value)(newState, pp)
  }

  def CollectionInsert[S <: State[S]](collection:RichExpression, index:RichExpression, right:RichExpression)(implicit state:S, pp:ProgramPoint):S = {
    state.insertCollectionValue(collection, index, right, pp)
  }

  def CollectionRemove[S <: State[S]](collection:RichExpression, index:RichExpression)(implicit state:S, pp:ProgramPoint):S = {
    state.removeCollectionValueByKey(collection, index, collection.getType().asInstanceOf[TouchCollection].getValueType)
  }

  def CollectionRemoveByValue[S <: State[S]](collection: RichExpression, value: RichExpression)(implicit  state: S, pp: ProgramPoint) = {
    state.removeCollectionValueByValue(collection, value, collection.getType().asInstanceOf[TouchCollection].getKeyType)
  }

  def CollectionIndexInRange[S <: State[S]](collection: RichExpression, index: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    index >= 0 && index < CollectionSize[S](collection)
  }

  def CollectionIncreaseLength[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    val newState = Assign[S](CollectionSize[S](collection), CollectionSize[S](collection) + 1)
    newState

//    if (newState.isSummaryCollection(collection)){
//      return newState.lub(newState, state)
//    } else {
//      return newState
//    }
  }

  def CollectionDecreaseLength[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    val assigned = Assign[S](CollectionSize[S](collection), CollectionSize[S](collection) - 1)
    // ensure that collection length is never < 0
    val newState = assigned.assume(CollectionSize[S](collection) >= 0)
    newState
//    if (newState.isSummaryCollection(collection)) {
//      return newState.lub(newState, state)
//    } else {
//      return newState
//    }
  }

  /*-- Misc --*/

  def Assume[S <: State[S]](expr:RichExpression)(implicit state:S, pp:ProgramPoint): S = {
    state.assume(expr)
  }

  def CallLocalAction[S <: State[S]](method:String,parameters:List[ExpressionSet] = Nil)(implicit state:S, pp:ProgramPoint): S = {
    SystemParameters.compiler.asInstanceOf[TouchCompiler].getMethodWithClassDefinition(method,SystemParameters.typ,parameters map (_.getType())) match {
      case Some((clazz,methodDef)) =>
        val res = MethodSummaries.collect(pp,clazz,methodDef,state,parameters)
        res
      case _ =>
        Reporter.reportImprecision("Could not find method "+method,pp)
        state.top()
    }
  }

  def CallApi[S <: State[S]](obj:RichExpression,method:String,parameters:List[ExpressionSet] = Nil,returnedType:TouchType)(implicit state:S, pp:ProgramPoint): S = {
    val semantics = SystemParameters.compiler.asInstanceOf[TouchCompiler].getType(obj.getType().getName())
    semantics.forwardSemantics(obj,method,parameters,returnedType)(pp,state)
  }

  def Return[S <: State[S]](e:RichExpression*)(implicit state:S, pp:ProgramPoint):S = {
    var set = new ExpressionSet(e.head.getType())
    for (ex <- e) { set = set.add(ex.thisExpr) }
    val stateN = state.setExpression(set)
    stateN
  }

  def Assign[S <: State[S]](id:RichExpression,value:RichExpression)(implicit state:S, pp:ProgramPoint): S = {

    def join(a:List[ExpressionSet],b:List[ExpressionSet]):List[ExpressionSet] = {
      a match {
        case x :: xs =>
          b match {
            case y :: ys => x.add(y) :: join(xs,ys)
            case Nil => a
          }
        case Nil => b
      }
    }

    def getMultiValAsList(expr:RichExpression):List[ExpressionSet] = {
      var ret:List[ExpressionSet] = Nil
      for (sExpr <- expr.thisExpr.getSetOfExpressions) yield {
        sExpr match {
          case MultiValExpression(left,right,retVal) =>
            val multiVal = getMultiValAsList(left) ::: getMultiValAsList(right)
            ret = join(ret,multiVal)
          case _ =>
            ret = join(ret,List(new ExpressionSet(expr.getType).add(expr)))
        }
      }
      ret
    }

    val leftExprs = getMultiValAsList(id)
    val rightExprs = getMultiValAsList(value)

    if(leftExprs.length != rightExprs.length) {
      Reporter.reportImprecision("A multival assignment has an unmatching number of values - going to top",pp)
      return state.top()
    }

    var curState = state
    for ((l,r) <- leftExprs.zip(rightExprs)) {

      // Create variable that might not exist yet
      if(l.getSetOfExpressions.size == 1 && l.getSetOfExpressions.head.isInstanceOf[VariableIdentifier]) {
        curState = curState.createVariable(l,l.getType(),pp)
      }

      curState = curState.assignVariable(l,r)
    }
    curState

  }
  /*-- Reading and writing of fields --*/

  def AssignField[S <: State[S]](obj:RichExpression,field:Identifier,value:RichExpression)(implicit state:S, pp:ProgramPoint): S = {
    AssignField[S](obj,field.getName(),value)
  }

  def AssignField[S <: State[S]](obj:RichExpression,field:String,value:RichExpression)(implicit state:S, pp:ProgramPoint): S = {
    if(!TouchAnalysisParameters.libraryFieldPruning ||
      SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(obj.getType().toString()+"."+field)) {

      if (TouchAnalysisParameters.topFields.contains(field)) {
        state.assignField(List(obj),field,Valid(value.getType()))
      } else {
        state.assignField(List(obj),field,value)
      }

    } else state
  }

  def Field[S <: State[S]](obj:RichExpression, field:TouchField)(implicit state:S, pp:ProgramPoint):RichExpression = {
    state.getFieldValue(List(obj),field.getName(),field.getType()).getExpression()
  }

  /*-- Skipping --*/

  def Exit[S <: State[S]](implicit state:S, pp:ProgramPoint): S = MethodSummaries.collectExit[S](state)

  def Skip[S <: State[S]](implicit state:S, pp:ProgramPoint): S = state.removeExpression()

  def Unimplemented[S <: State[S]](method:String)(implicit state:S, pp:ProgramPoint): S = {
    Reporter.reportImprecision(method+" not implemented, going to top",pp)
    state.top()
  }

  def MatchFields[S <: State[S]](this0: RichExpression, parameters:List[ExpressionSet], typ:TouchType, method:String)(implicit state:S, pp:ProgramPoint): S = {

    val fieldResult =
      if(parameters.length == 0)
      // Getters
        typ.getPossibleFields().find(_.getName() == method ) match {
          case Some(field) =>
            val fieldValue = Field[S](this0,field.asInstanceOf[TouchField])
            val stateWithExpr = Return[S](fieldValue)
            Some(stateWithExpr)
          case None => None
        }
      else if (parameters.length == 1)
      // Setters
        typ.getPossibleFields().find("set "+_.getName() == method ) match {
          case Some(field) =>
            Some(AssignField[S](this0,field,parameters.head))
          case None =>  None
        }
      else None

    fieldResult match {
      case Some(res) => res
      case None => Unimplemented[S](typ.toString+"."+method)
    }

  }


  /*-- Constants --*/

  def String(a:String)(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant(a,TString.typ,pp))
  def True(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant("true",TBoolean.typ,pp))
  def False(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant("false",TBoolean.typ,pp))
  def Bottom(typ:TouchType): RichExpression = toRichExpression(new ExpressionSet(typ).bottom())
  def PositiveInfinity(implicit pp:ProgramPoint) :RichExpression = toRichExpression(new Constant("posinfty",TNumber.typ,pp))
  def NegativeInfinity(implicit pp:ProgramPoint) :RichExpression = toRichExpression(new Constant("neginfty",TNumber.typ,pp))
  def Invalid(typ:Type)(implicit pp:ProgramPoint) :RichExpression = toRichExpression(new Constant("invalid",typ,pp))
  def Valid(typ:Type)(implicit pp:ProgramPoint) :RichExpression = toRichExpression(new Constant("valid",typ,pp))
  def Singleton(typ:Type)(implicit pp:ProgramPoint) : RichExpression = toRichExpression(VariableIdentifier(typ.getName().toLowerCase,typ,pp,EmptyScopeIdentifier()))

  /*-- Conversion --*/

  implicit def toRichExpression(value:ExpressionSet) : RichExpression =
    RichExpression(value)

  implicit def toRichExpression(value:Inclusive) : RichExpression =
    toRichExpression(value.head) ndTo toRichExpression(value.last)

  implicit def toRichExpression(value:Int) : RichExpression =
    RichExpression(new ExpressionSet(TNumber.typ).add(new Constant(value.toString,TNumber.typ,null)))

  implicit def toRichExpression(value:Double) : RichExpression =
    RichExpression(new ExpressionSet(TNumber.typ).add(new Constant(value.toString,TNumber.typ,null)))

  implicit def toRichExpression(value:Expression) : RichExpression =
    RichExpression(new ExpressionSet(value.getType()).add(value))

  implicit def toExpressionSet(value:RichExpression) : ExpressionSet =
    value.thisExpr

}

class TouchField(name:String, typName:String, val default: Initializer = NewInitializer(), val topDefault: Initializer = TopInitializer(), val isSummaryNode:Boolean = false)
  extends Identifier(null,null) {
  override def getType():TouchType = SystemParameters.compiler.asInstanceOf[TouchCompiler].getType(typName).getTyp
  override def getName() = name.toString
  override def toString = name.toString
  override def getField() = Some(name)
  override def hashCode() : Int = name.hashCode() + typName.hashCode()
  override def representSingleVariable()=isSummaryNode
  override def equals(o : Any) = (o.isInstanceOf[TouchField] && o.asInstanceOf[TouchField].getName() == this.getName()
    && o.asInstanceOf[TouchField].getType() == this.getType())
  def identifiers() : Set[Identifier] = Set(this)
}

trait Initializer
case class InvalidInitializer() extends Initializer
case class NewInitializer() extends Initializer
case class TopInitializer() extends Initializer
case class TopWithInvalidInitializer() extends Initializer
case class ExpressionInitializer(e: RichExpression) extends Initializer

case class RichExpression(thisExpr : ExpressionSet) {

  override def toString:String = thisExpr.toString

  def <= (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.<=, TBoolean.typ))

  def >= (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.>=,TBoolean.typ))

  def < (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.<, TBoolean.typ))

  def > (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.>, TBoolean.typ))

  def equal (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.==, TBoolean.typ))

  def unequal (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.!=, TBoolean.typ))

  def + (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.+, TNumber.typ))

  def * (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.*, TNumber.typ))

  def - (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator.-, TNumber.typ))

  def / (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBinaryExpression(thisExpr, thatExpr, ArithmeticOperator./, TNumber.typ))

  def or (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createNondeterministicBinaryExpression(thisExpr,thatExpr,NondeterministicOperator.or,thisExpr.getType()))

  def ndTo (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createNondeterministicBinaryExpression(thisExpr,thatExpr,NondeterministicOperator.to,TNumber.typ))

  def && (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBooleanBinaryExpression(thisExpr,thatExpr,BooleanOperator.&&,TBoolean.typ))

  def || (thatExpr : RichExpression) : RichExpression =
    RichExpression(ExpressionFactory.createBooleanBinaryExpression(thisExpr,thatExpr,BooleanOperator.||,TBoolean.typ))

  def not () : RichExpression =
    RichExpression(ExpressionFactory.createNegatedBooleanExpression(thisExpr))
}