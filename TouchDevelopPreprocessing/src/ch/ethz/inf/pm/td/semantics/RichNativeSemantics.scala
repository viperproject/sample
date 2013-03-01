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

  def Error[S <: State[S]](method:String, message:String)(implicit state:S, pp:ProgramPoint) {
     Reporter.reportError("When calling "+method+": "+message,pp)
  }

  def Error[S <: State[S]](expr:RichExpression, message:String)(implicit state:S, pp:ProgramPoint) {
    if (!Reporter.hasError(message,pp)) {
      // The next line is a fix for "bottom" expression, where I don't know where they come from.
      val errorState = state.assume( expr ).setExpression(new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(SystemParameters.typ.top(),pp)))
      if(!errorState.lessEqual(state.bottom())) {
        Reporter.reportError(message,pp)
      }
    }
  }

  def Error[S <: State[S]](expr:RichExpression, method:String, message:String)(implicit state:S, pp:ProgramPoint) {
    Error[S](expr,"When calling "+method+": "+message)
  }

  def CheckInRangeInclusive[S <: State[S]](expr:RichExpression, low:RichExpression, high:RichExpression, method:String, parameter:String)(implicit s:S, pp:ProgramPoint) {
    Error(expr < low,method+": Parameter "+parameter+" ("+expr+") may be less than the lowest allowed value ("+low+")")(s,pp)
    Error(expr > high,method+": Parameter "+parameter+" ("+expr+") may be greater than the highest allowed value "+high+")")(s,pp)
  }

  def IfPossible[S <: State[S]](expr:RichExpression, then: => S, els: => S)(implicit state:S, pp:ProgramPoint):S = {
    val errorState = state.assume( expr ).setExpression(new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(SystemParameters.typ.top(),pp)))
    if(!errorState.lessEqual(state.bottom())) {
      then
    } else {
      els
    }
  }

  /**
   * Creates a new Object of type typ, and initializes its fields with the given arguments.
   */
  def New[S <: State[S]](typ:TouchType, initials:Map[Identifier,RichExpression])(implicit s:S, pp:ProgramPoint): S = {

    var curState = typ match {
      case col:TouchCollection =>
        s.createCollection(col,col.getKeyType,col.getValueType,TNumber.typ,pp)
      case _ =>
        s.createObject(typ,pp)
    }
    val obj = curState.getExpression()

    // Assign fields with given arguments
    for (f <- typ.getPossibleFields()) {
      if(SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.toString()+"."+f.getName())) {
        val a = initials.get(f) match {
          case None =>
            val tF = f.asInstanceOf[TouchField]
            tF.default
          case Some(st) => st
        }
        curState = curState.assignField(List(obj),f.getName(),a)
      }
    }

    // Make sure that our value is "valid"  now
    curState = curState.assignVariable(obj,Valid(typ))

    curState.setExpression(obj)
  }


  def New[S <: State[S]](typ:TouchType, args:RichExpression*)(implicit s:S, pp:ProgramPoint): S = {
    New[S](typ,(typ.getPossibleFieldsSorted() zip args).toMap)(s,pp)
  }

  def New[S <: State[S]](typ:TouchType)(implicit s:S, pp:ProgramPoint): S = {
    New[S](typ,Map.empty[Identifier,RichExpression])(s,pp)
  }

  def Top[S <: State[S]](typ:TouchType, initials:Map[Identifier,RichExpression] = Map.empty[Identifier,RichExpression])(implicit s:S, pp:ProgramPoint): S = {
    typ.getName match {
      case TNumber.typName => s.setExpression(Valid(TNumber.typ))
      case TBoolean.typName => s.setExpression(new ExpressionSet(TBoolean.typ).add(True).add(False))
      case _ =>

        var curState = typ match {
          case col:TouchCollection =>
            s.createCollection(col,col.getKeyType,col.getValueType,TNumber.typ,pp)
          case _ =>
            s.createObject(typ,pp)
        }
        val obj = curState.getExpression()

        // Make sure that our value is "valid"  now
        curState = curState.assignVariable(obj,Valid(typ))

        typ match {
          case col:TouchCollection =>
            val (newPP1, referenceLoop1) = DeepeningProgramPoint(pp,"__summary")
            if (!referenceLoop1) {
              // Create summary node of collections
              curState = Top[S](col.getValueType)(curState,newPP1)
              val topSummary = curState.getExpression()
              curState = Assign[S](CollectionSummary[S](obj),topSummary)(curState,newPP1)
            }
            val (newPP2, referenceLoop2) = DeepeningProgramPoint(pp,"__size")
            if (!referenceLoop2) {
              // Set size to top
              curState = Top[S](TNumber.typ)(curState,newPP2)
              val topSize = curState.getExpression()
              curState = Assign[S](CollectionSize[S](obj),topSize)(curState,newPP2)
            }
          case _ => ()
        }

        // Assign fields with given arguments
        for (f <- typ.getPossibleFields()) {
          if(SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(typ.toString()+"."+f.getName())) {
            initials.get(f) match {
              case None =>
                val (newPP, referenceLoop) = DeepeningProgramPoint(pp,f.getName())
                if (!referenceLoop) {
                  curState = Top[S](f.getType().asInstanceOf[TouchType])(curState,newPP)
                  val topField = curState.getExpression()
                  curState = curState.assignField(List(obj),f.getName(),topField)
                }
              case Some(st) => curState = curState.assignField(List(obj),f.getName(),st)
            }
          }
        }

        curState.setExpression(obj)

    }
  }

  def Clone[S <: State[S]](obj:RichExpression)(implicit s:S, pp:ProgramPoint): S = {

    var curState = New[S](obj.getType().asInstanceOf[TouchType])(s,pp)
    val newObject = toRichExpression(curState.getExpression())

    if (obj.getType().isInstanceOf[TouchCollection]) {
      curState = Assign[S](CollectionSummary[S](newObject),CollectionSummary[S](obj))(curState,pp)
      curState = Assign[S](CollectionSize[S](newObject),CollectionSize[S](obj))(curState,pp)
    }

    // Clone fields
    for (f <- obj.getType().getPossibleFields()) {
      if(SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(obj.getType().toString()+"."+f.getName())) {
        val (newPP, referenceLoop) = DeepeningProgramPoint(pp,f.getName())
        if (!referenceLoop) {
          val oldField = Field[S](obj,f)(curState,newPP)
          curState = Clone[S](oldField)(curState,newPP)
          val clonedContent = curState.getExpression()
          curState = curState.assignField(List(newObject),f.getName(),clonedContent)
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

  // TODO: Implement this decently
  def CollectionSummary[S <: State[S]](collection:RichExpression)(implicit state:S, pp:ProgramPoint):RichExpression = {
    state.getCollectionCell(collection,toRichExpression(0)).getExpression()
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

  def CollectionCopy[S <: State[S]](collection:RichExpression)(implicit state:S, pp:ProgramPoint):S = {
    val state1 = New[S](collection.getType().asInstanceOf[TouchCollection])
    val newCollection = state1.getExpression().getSetOfExpressions.head
    val state2 = Assign[S](CollectionSummary[S](newCollection),CollectionSummary[S](collection))(state1,pp)
    val state3 = Assign[S](CollectionSize[S](newCollection),CollectionSize[S](collection))(state2,pp)
    val state4 = state3.setExpression(new ExpressionSet(newCollection.getType()).add(newCollection))
    state4
  }

  /*-- Misc --*/

  def Return[S <: State[S]](e:RichExpression*)(implicit state:S, pp:ProgramPoint):S = {
    var set = new ExpressionSet(e.head.getType())
    for (ex <- e) { set = set.add(ex.thisExpr) }
    state.setExpression(set)
  }

  def Assign[S <: State[S]](id:RichExpression,value:RichExpression)(implicit state:S, pp:ProgramPoint): S = {
    state.assignVariable(id,value)
  }

  /*-- Reading and writing of fields --*/

  def AssignField[S <: State[S]](obj:RichExpression,field:Identifier,value:RichExpression)(implicit state:S, pp:ProgramPoint): S = {
    if(SystemParameters.compiler.asInstanceOf[TouchCompiler].relevantLibraryFields.contains(obj.getType().toString()+"."+field.getName())) {
      state.assignField(List(obj),field.getName(),value)
    } else state
  }

  def Field[S <: State[S]](obj:RichExpression, field:Identifier)(implicit state:S, pp:ProgramPoint):RichExpression = {
    state.getFieldValue(List(obj),field.getName(),field.getType()).getExpression()
  }

  /*-- Skipping --*/

  def Skip[S <: State[S]](implicit state:S, pp:ProgramPoint): S = state

  def Unimplemented[S <: State[S]](method:String)(implicit state:S, pp:ProgramPoint): S = {
    Reporter.reportImprecision(method+" not implemented, unsound from now on",pp)
    Skip[S]
  }

  def MatchFields[S <: State[S]](this0: RichExpression, parameters:List[ExpressionSet], typ:TouchType, method:String)(implicit state:S, pp:ProgramPoint): S = {

    val fieldResult =
      if(parameters.length == 0)
        // Getters
        typ.getPossibleFieldsSorted().find(_.getName() == method ) match {
          case Some(field) =>
            val fieldValue = Field[S](this0,field.asInstanceOf[VariableIdentifier])
            val stateWithExpr = Return[S](fieldValue)
            Some(stateWithExpr)
          case None => None
        }
      else if (parameters.length == 1)
        // Setters
        typ.getPossibleFieldsSorted().find("set_"+_.getName() == method ) match {
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

  def StringCst(a:String)(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant(a,TString.typ,pp))
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

class TouchField(name:String, val touchTyp:TouchType, var default: RichExpression = null, val isSummaryNode:Boolean = false)
  extends VariableIdentifier(name,touchTyp,null) {

  if (default == null) {
    default = toRichExpression(new Constant("valid",touchTyp,null))
  }
}

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