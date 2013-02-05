package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, NativeMethodSemantics, Type}
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression
import ch.ethz.inf.pm.sample.abstractdomain.BinaryBooleanExpression
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchException, TouchType}
import collection.immutable.Range.Inclusive
import RichNativeSemantics._

/**
 *
 * This class defines a richer interface to interact with the current state. This enables us to specify the
 * abstract semantics of many of TouchDevelops native functions in a more natural way.
 *
 *
 * Lucas Brutschy
 * Date: 10/5/12
 * Time: 4:30 PM
 * 
 */

object ErrorReporter {

  var seenErrors = Set[(String,ProgramPoint)]()

  def hasError(message:String,pp:ProgramPoint):Boolean = seenErrors.contains((message,pp))

  def reportError(message:String,pp:ProgramPoint) {
    println(message+" at line "+pp.getLine()+", column "+pp.getColumn())
    seenErrors += ((message,pp))
  }

}


object RichNativeSemantics {

  /*-- Checking for errors --*/

  def Error[S <: State[S]](expr:RichExpression, message:String)(implicit state:S, pp:ProgramPoint) {
    if (!ErrorReporter.hasError(message,pp)) {
      // The next line is a fix for "bottom" expression, where I don't know where they come from.
      val errorState = state.assume( expr ).setExpression(new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(SystemParameters.typ.top(),pp)))
      if(!errorState.lessEqual(state.bottom())) {
        ErrorReporter.reportError(message,pp)
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
  /**
   * Creates a new Object of type typ, and initializes its fields with the given arguments.
   */
  def New[S <: State[S]](typ:TouchType, initials:scala.collection.immutable.Map[Identifier,RichExpression])(implicit s:S, pp:ProgramPoint): S = {

    var curState = typ match {
      case col:TouchCollection =>
        s.createCollection(col,col.getKeyType,col.getValueType,TNumber.typ,pp)
      case _ =>
        s.createObject(typ,pp)
    }
    val obj = curState.getExpression()

    // Assign fields with given arguments
    for (f <- typ.getPossibleFields()) {
      val a = initials.get(f) match {
        case None =>
          val tF = f.asInstanceOf[TouchField]
          //if (tF.isSummaryNode) {
          //
          //} else {
            tF.default
          //}
        case Some(st) => st
      }
      curState = curState.assignField(List(obj),f.getName(),a)
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
    val state1 = New[S](collection.typ.asInstanceOf[TouchCollection])
    val newCollection = state1.getExpression().setOfExpressions.head
    val state2 = Assign[S](CollectionSummary[S](newCollection),CollectionSummary[S](collection))(state1,pp)
    val state3 = Assign[S](CollectionSize[S](newCollection),CollectionSize[S](collection))(state2,pp)
    val state4 = state3.setExpression(new ExpressionSet(newCollection.getType()).add(newCollection))
    state4
  }

  /*-- Misc --*/

  def Return[S <: State[S]](e:RichExpression)(implicit state:S, pp:ProgramPoint):S = {
    state.setExpression(e)
  }

  def Assign[S <: State[S]](id:Expression,value:ExpressionSet)(implicit state:S, pp:ProgramPoint): S = {
    state.assignVariable(new ExpressionSet(id.getType()).add(id),value)
  }

  /*-- Reading and writing of fields --*/

  def AssignField[S <: State[S]](obj:ExpressionSet,field:Identifier,value:ExpressionSet)(implicit state:S, pp:ProgramPoint): S = {
    state.assignField(List(obj),field.getName(),value)
  }

  def Field[S <: State[S]](obj:RichExpression, field:VariableIdentifier)(implicit state:S, pp:ProgramPoint):RichExpression = {
    state.getFieldValue(List(obj),field.getName(),field.getType()).getExpression()
  }

  def JoinField[S <: State[S]](thiss:RichExpression,field:TouchField,value:RichExpression)(implicit s:S, pp:ProgramPoint): S = {
    if (field.isSummaryNode) {
      // Assignment is weak!
      AssignField[S](thiss,field,Field[S](thiss,field) or value)
    } else {
      throw new TouchException("Join field is only implemented for summary nodes");
    }
  }

  def BottomField[S <: State[S]](thiss:RichExpression,field:TouchField)(implicit s:S, pp:ProgramPoint): S = {
    AssignField[S](thiss,field,Bottom(field.touchTyp))
  }

  def CopyOfField[S <: State[S]](thiss:RichExpression,field:TouchField)(implicit s:S, pp:ProgramPoint): RichExpression = {
    Field[S](thiss,field) // TODO: IMPLEMENT
  }

  /*-- Skipping --*/

  def Skip[S <: State[S]](implicit state:S, pp:ProgramPoint): S = state

  def Unimplemented[S <: State[S]](method:String)(implicit state:S, pp:ProgramPoint): S = {
    println(method+" not implemented, unsound from now on, at "+pp)
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
  def Top(typ:TouchType): RichExpression = toRichExpression(new ExpressionSet(typ).top())
  def Bottom(typ:TouchType): RichExpression = toRichExpression(new ExpressionSet(typ).bottom())
  def Invalid(typ:Type)(implicit pp:ProgramPoint) :RichExpression = RichExpression(new Constant("invalid",typ,pp))
  def Valid(typ:Type)(implicit pp:ProgramPoint) :RichExpression = RichExpression(new Constant("valid",typ,pp))

  // TODO: Implement this
  def UnknownSize(typ:TouchCollection)(implicit pp:ProgramPoint) :RichExpression = RichExpression(new Constant("valid",typ,pp))

  /*-- Conversion --*/

  implicit def toRichExpression[S <: State[S]](value:ExpressionSet) : RichExpression = {
    val numOfExpressions = (value.setOfExpressions.size)
    if (numOfExpressions > 1) throw new TouchException("Generic Abstract State fucked me over again!")
    else if (numOfExpressions < 1 ) toRichExpression(0).ndTo(toRichExpression(1000)) // TODO TODO TODO TODO
    else RichExpression(value.setOfExpressions.head)
  }

  implicit def toRichExpression(value:Inclusive) : RichExpression =
    toRichExpression(value.head) ndTo toRichExpression(value.last)

  implicit def toRichExpression(value:Int) : RichExpression =
    RichExpression(new Constant(value.toString,TNumber.typ,null))

  implicit def toRichExpression(value:Double) : RichExpression =
    RichExpression(new Constant(value.toString,TNumber.typ,null))

  implicit def toRichExpression(value:Expression) : RichExpression =
    RichExpression(value)

  implicit def toExpression(value:RichExpression) : Expression =
    value.thisExpr

  implicit def toExpressionSet(value:RichExpression) : ExpressionSet =
    (new ExpressionSet(value.thisExpr.getType())).add(value.thisExpr)

}

class TouchField(name:String, val touchTyp:TouchType, var default: RichExpression = null, val isSummaryNode:Boolean = false)
  extends VariableIdentifier(name,touchTyp,null) {

  if (default == null) {
    default = RichExpression(new Constant("valid",touchTyp,null))
  }
}

case class RichExpression(thisExpr : Expression) {

  override def toString:String = thisExpr.toString()

  def <= (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.<=, TBoolean.typ))

  def >= (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.>=,TBoolean.typ))

  def < (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.<, TBoolean.typ))

  def > (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.>, TBoolean.typ))

  def equal (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.==, TBoolean.typ))

  def unequal (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.!=, TBoolean.typ))

  def + (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.+, TNumber.typ))

  def * (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.*, TNumber.typ))

  def - (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.-, TNumber.typ))

  def / (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator./, TNumber.typ))

  def or (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryNondeterministicExpression(thisExpr,thatExpr,NondeterministicOperator.or,TNumber.typ))

  def ndTo (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryNondeterministicExpression(thisExpr,thatExpr,NondeterministicOperator.to,TNumber.typ))

  def && (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryBooleanExpression(thisExpr,thatExpr,BooleanOperator.&&,TBoolean.typ))

  def || (thatExpr : RichExpression) : RichExpression =
    RichExpression(new BinaryBooleanExpression(thisExpr,thatExpr,BooleanOperator.||,TBoolean.typ))

  def not () : RichExpression =
    RichExpression(new NegatedBooleanExpression(thisExpr))
}