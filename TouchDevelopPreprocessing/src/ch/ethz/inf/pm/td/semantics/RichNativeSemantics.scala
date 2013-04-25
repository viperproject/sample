package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.{Reporter, SystemParameters}
import ch.ethz.inf.pm.td.compiler._
import collection.immutable.Range.Inclusive
import RichNativeSemantics._
import scala.Some
import ch.ethz.inf.pm.td.compiler.TouchCollection
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.abstractdomain.UnitExpression
import ch.ethz.inf.pm.td.analysis.{MethodSummaries, TouchAnalysisParameters}

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

  def Error[S <: State[S]](expr:RichExpression, message:String)(implicit state:S, pp:ProgramPoint):S = {
    val errorState = state.assume( expr ).setExpression(new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(SystemParameters.typ.top(),pp)))
    if(!errorState.lessEqual(state.bottom())) {
      Reporter.reportError(message,pp)
      state.assume(expr.not())
    } else state
  }

  def Error[S <: State[S]](expr:RichExpression, method:String, message:String)(implicit state:S, pp:ProgramPoint):S = {
    Error[S](expr,"When calling "+method+": "+message)
  }

  def CheckInRangeInclusive[S <: State[S]](expr:RichExpression, low:RichExpression, high:RichExpression, method:String, parameter:String)(implicit s:S, pp:ProgramPoint):S = {
    val state1 = Error(expr < low,method+": Parameter "+parameter+" ("+expr+") may be less than the lowest allowed value ("+low+")")(s,pp)
    Error(expr > high,method+": Parameter "+parameter+" ("+expr+") may be greater than the highest allowed value "+high+")")(state1,pp)
  }

  def CheckNonNegative[S <: State[S]](expr:RichExpression, method:String, parameter:String)(implicit s:S, pp:ProgramPoint):S = {
    Error(expr < 0,method+": Parameter "+parameter+" ("+expr+") may negative")(s,pp)
  }

  def If[S <: State[S]](expr:RichExpression, Then: S => S, Else: S => S)(implicit state:S, pp:ProgramPoint):S = {
    val thenState = state.assume( expr )
    val elseState = state.assume( expr.not() )
    state.lub(Then(thenState),Else(elseState))
  }

  /**
   * Creates a new Object of type typ, and initializes its fields with the given arguments.
   */
  def New[S <: State[S]](typ:TouchType,
                         initials:Map[TouchField,RichExpression] = Map.empty[TouchField,RichExpression],
                         recursive:Boolean = true,
                         initialCollectionSummary:Option[ExpressionSet] = None,
                         initialCollectionSize:Option[ExpressionSet] = None)(implicit s:S, pp:ProgramPoint): S = {
    typ.getName match {
      case TNumber.typName => s.setExpression(new ExpressionSet(TNumber.typ).add(Constant("0",TNumber.typ,pp)))
      case TBoolean.typName => s.setExpression(new ExpressionSet(TBoolean.typ).add(False))
      case TString.typName => s.setExpression(new ExpressionSet(TString.typ).add(Constant("",TString.typ,pp)))
      case _ =>

        var curState = typ match {
          case col:TouchCollection =>
            s.createCollection(col,col.getKeyType,col.getValueType,TNumber.typ,pp)
          case _ =>
            s.createObject(typ,pp,false)
        }
        val obj = curState.getExpression()

        if(recursive) {
          typ match {
            case col:TouchCollection =>
              // Initialize collection size
              initialCollectionSize match {
                case None =>
                  curState = Assign[S](CollectionSize[S](obj),0)(curState,pp)
                case Some(x) =>
                  curState = Assign[S](CollectionSize[S](obj),x)(curState,pp)
              }
              initialCollectionSummary match {
                case None =>
                  // Remains bottom
                case Some(x) =>
                  curState = Assign[S](CollectionSummary[S](obj),x)(curState,pp)
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
                  case InvalidInitializer() => Invalid(f.touchTyp)
                  case TopInitializer() => curState = Top[S](f.touchTyp,recursive = !referenceLoop)(curState,newPP); toRichExpression(curState.getExpression())
                  case TopWithInvalidInitializer() => curState = TopWithInvalid[S](f.touchTyp,recursive = !referenceLoop)(curState,newPP); toRichExpression(curState.getExpression())
                  case NewInitializer() => curState = New[S](f.touchTyp,recursive = !referenceLoop)(curState,newPP); toRichExpression(curState.getExpression())
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

  def Top[S <: State[S]](typ:TouchType, initials:Map[Identifier,RichExpression] = Map.empty[Identifier,RichExpression],recursive:Boolean = true)(implicit s:S, pp:ProgramPoint): S = {
    typ.getName match {
      case TNumber.typName => s.setExpression(Valid(TNumber.typ))
      case TBoolean.typName => s.setExpression(new ExpressionSet(TBoolean.typ).add(True).add(False))
      case TString.typName => s.setExpression(Valid(TString.typ))
      case _ =>

        var curState = typ match {
          case col:TouchCollection =>
            s.createCollection(col,col.getKeyType,col.getValueType,TNumber.typ,pp)
          case _ =>
            s.createObject(typ,pp,false)
        }
        val obj = curState.getExpression()

        // Make sure that our value is "valid"  now
        curState = curState.assignVariable(obj,Valid(typ))

        if(recursive) {
          typ match {
            case col:TouchCollection =>
              val (newPP1, referenceLoop1) = DeepeningProgramPoint(pp,"*summary")
              // Create summary node of collections
              curState = Top[S](col.getValueType,recursive = !referenceLoop1)(curState,newPP1)
              val topSummary = curState.getExpression()
              curState = Assign[S](CollectionSummary[S](obj),topSummary)(curState,newPP1)
              val (newPP2, referenceLoop2) = DeepeningProgramPoint(pp,"*size")
              // Set size to top
              curState = Top[S](TNumber.typ,recursive = !referenceLoop2)(curState,newPP2)
              val topSize = curState.getExpression()
              curState = Assign[S](CollectionSize[S](obj),topSize)(curState,newPP2)
            case _ => ()
          }

          // Assign fields with given arguments
          for (f <- typ.getPossibleFields()) {
            if(!TouchAnalysisParameters.libraryFieldPruning ||
              SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.toString()+"."+f.getName())) {
              initials.get(f) match {
                case None =>
                  val (newPP, referenceLoop) = DeepeningProgramPoint(pp,f.getName())
                  curState = Top[S](f.getType().asInstanceOf[TouchType],recursive = !referenceLoop)(curState,newPP)
                  val topField = curState.getExpression()
                  curState = curState.assignField(List(obj),f.getName(),topField)
                case Some(st) => curState = curState.assignField(List(obj),f.getName(),st)
              }
            }
          }
        }

        curState.setExpression(obj)

    }
  }

  def TopWithInvalid[S <: State[S]](typ:TouchType,
                                    initials:Map[Identifier,RichExpression] = Map.empty[Identifier,RichExpression],
                                    recursive:Boolean = true)(implicit s:S, pp:ProgramPoint): S = {

    val curState = Top[S](typ,initials,recursive)(s,pp)
    val validResult = curState.getExpression()
    Return[S](validResult,Invalid(typ))(curState,pp)

  }

  def Clone[S <: State[S]](obj:RichExpression, initials:Map[Identifier,RichExpression] = Map.empty[Identifier,RichExpression], recursive : Boolean = true)(implicit s:S, pp:ProgramPoint): S = {

    val touchTyp = obj.getType().asInstanceOf[TouchType]

    // Never clone immutable types where we don't change the fields. This includes all primitives
    if (touchTyp.isImmutable && initials.isEmpty) {
      return Return[S](obj)
    }

    var curState = New[S](touchTyp,recursive = false)(s,pp)
    val newObject = toRichExpression(curState.getExpression())

    // Clone fields

    if (obj.getType().isInstanceOf[TouchCollection]) {
      curState = Assign[S](CollectionSize[S](newObject),CollectionSize[S](obj))(curState,pp)
      if (!CollectionSummary[S](obj).isBottom) {
        curState = Assign[S](CollectionSummary[S](newObject),CollectionSummary[S](obj))(curState,pp)
      }
    }

    for (f <- obj.getType().getPossibleFields()) {
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

  def CollectionAt[S <: State[S]](collection:RichExpression,index:RichExpression)(implicit state:S, pp:ProgramPoint):RichExpression = {
    state.getCollectionCell(collection,index).getExpression()
  }

  def CollectionSummary[S <: State[S]](collection:RichExpression)(implicit state:S, pp:ProgramPoint):RichExpression = {
    state.getCollectionCell(collection,Valid(TNumber.typ)).getExpression()
  }

  def CollectionClear[S <: State[S]](collection:RichExpression)(implicit state:S, pp:ProgramPoint):S = {
    state.clearCollection(collection)
  }

  def CollectionInsert[S <: State[S]](collection:RichExpression, index:RichExpression, right:RichExpression)(implicit state:S, pp:ProgramPoint):S = {
    state.insertCollectionCell(collection,index,right)
  }

  def CollectionUpdate[S <: State[S]](collection:RichExpression, index:RichExpression, right:RichExpression)(implicit state:S, pp:ProgramPoint):S = {
    state.assignCollectionCell(collection,index,right)
  }

  def CollectionRemove[S <: State[S]](collection:RichExpression, index:RichExpression)(implicit state:S, pp:ProgramPoint):S = {
    state.removeCollectionCell(collection,index)
  }

  /*-- Misc --*/

//  def RunActionFromString[S <: State[S]](handlerName:RichExpression,parameters:List[ExpressionSet] = Nil)(implicit state:S, pp:ProgramPoint): S = {       TODO
//    val handlerNames:Set[String] = state.getStringValues(handlerName)
//
//    if (handlerNames.isEmpty) {
//      Reporter.reportImprecision("Calling an unknown handler, going to top",pp)
//      state.top()
//    } else {
//      // Compute least upper bound over all possible handlers
//      var result = state.bottom()
//      for (handler <- handlerNames) {
//        result = result.lub(result,CallLocalAction[S](handler,parameters)(state,pp))
//      }
//      result
//    }
//  }

  def CallLocalAction[S <: State[S]](method:String,parameters:List[ExpressionSet] = Nil)(implicit state:S, pp:ProgramPoint): S = {
    SystemParameters.compiler.asInstanceOf[TouchCompiler].getMethodWithClassDefinition(method,SystemParameters.typ,parameters map (_.getType())) match {
      case Some((clazz,methodDef)) =>
        val res = MethodSummaries.collect(pp,clazz,methodDef,state,parameters)
        res
      case _ =>
        Reporter.reportImprecision("Could not find this method "+method,pp)
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
    state.assignVariable(id,value)
  }

  /*-- Reading and writing of fields --*/

  def AssignField[S <: State[S]](obj:RichExpression,field:Identifier,value:RichExpression)(implicit state:S, pp:ProgramPoint): S = {
    AssignField[S](obj,field.getName(),value)
  }

  def AssignField[S <: State[S]](obj:RichExpression,field:String,value:RichExpression)(implicit state:S, pp:ProgramPoint): S = {
    if(!TouchAnalysisParameters.libraryFieldPruning ||
      SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(obj.getType().toString()+"."+field)) {
      state.assignField(List(obj),field,value)
    } else state
  }

  def Field[S <: State[S]](obj:RichExpression, field:Identifier)(implicit state:S, pp:ProgramPoint):RichExpression = {
    state.getFieldValue(List(obj),field.getName(),field.getType()).getExpression()
  }

  /*-- Skipping --*/

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
            val fieldValue = Field[S](this0,field.asInstanceOf[VariableIdentifier])
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
  def Invalid(typ:Type)(implicit pp:ProgramPoint) :RichExpression = toRichExpression(new Constant("invalid",typ,pp))
  def Valid(typ:Type)(implicit pp:ProgramPoint) :RichExpression = toRichExpression(new Constant("valid",typ,pp))
  def Singleton(typ:Type)(implicit pp:ProgramPoint) : RichExpression = toRichExpression(VariableIdentifier(typ.getName(),typ,pp))

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

class TouchField(name:String, val touchTyp:TouchType, var default: Initializer = NewInitializer(), val isSummaryNode:Boolean = false)
  extends VariableIdentifier(name,touchTyp,null)

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