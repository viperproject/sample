package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, NativeMethodSemantics, Type}
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression
import ch.ethz.inf.pm.sample.abstractdomain.BinaryBooleanExpression
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.compiler.{TouchException, TouchType}
import collection.immutable.Range.Inclusive

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


trait RichNativeSemantics extends NativeMethodSemantics {

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

  def CheckInRangeInclusive[S <: State[S]](expr:RichExpression, low:RichExpression, high:RichExpression, method:String, parameter:String)(implicit s:S, pp:ProgramPoint) {
    Error(expr < low,method+": Parameter "+parameter+" ("+expr+") may be less than the lowest allowed value ("+low+")")(s,pp)
    Error(expr > high,method+": Parameter "+parameter+" ("+expr+") may be less than the highest allowed value "+high+")")(s,pp)
  }

  /*-- Creating objects --*/

  /**
   * Creates a new Object of type typ, and initializes its fields with the given arguments.
   */
  def New[S <: State[S]](typ:TouchType, initials:Map[Identifier,RichExpression])(implicit s:S, pp:ProgramPoint): S = {
    var curState = s.createObject(typ,pp)
    val obj = curState.getExpression()

    // Assign fields with given arguments
    for (f <- typ.getPossibleFields()) {
      val a = initials.get(f) match {
        case None =>
          val tF = f.asInstanceOf[TouchField]
          //if (tF.isSummaryNode) {
            // Summary Nodes are always initialized to an object with all bottom
          //  NewSummary[S](tF.touchTyp)
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

  /*-- Misc --*/

  def Return[S <: State[S]](e:Expression)(implicit state:S, pp:ProgramPoint):S = {
    state.setExpression(toRichExpression(e))
  }

  def SetEnv[S <: State[S]](id:EnvironmentIdentifier,value:ExpressionSet)(implicit state:S, pp:ProgramPoint): S = {
    state.assignVariable(new ExpressionSet(id.typ).add(id),value)
  }

  /*-- Reading and writing of fields --*/

  def AssignField[S <: State[S]](obj:ExpressionSet,field:Identifier,value:ExpressionSet)(implicit state:S, pp:ProgramPoint): S = {
    state.assignField(List(obj),field.getName(),value)
  }

  def Field[S <: State[S]](obj:RichExpression, field:VariableIdentifier)(implicit state:S, pp:ProgramPoint):RichExpression = {
    state.getFieldValue(List(obj),field.getName(),field.getType()).getExpression()
  }

  def JoinField[S <: State[S]](thiss:RichExpression,field:TouchField,value:RichExpression)(implicit s:S, pp:ProgramPoint): S = {
    AssignField[S](thiss,field,Field[S](thiss,field) or value)
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
        typ.getPossibleFieldsSorted().find(_.getName() == method) match {
          case Some(field) =>
            val fieldValue = Field[S](this0,field.asInstanceOf[VariableIdentifier])
            val stateWithExpr = Return[S](fieldValue)
            Some(stateWithExpr)
          case None => None
        }
      else None

    fieldResult match {
      case Some(res) => res
      case None => Unimplemented[S](typ.toString+"."+method)
    }

  }


  /*-- Constants --*/

  def True(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant("true",TBoolean.typ,pp))
  def False(implicit pp:ProgramPoint) : RichExpression = toRichExpression(Constant("false",TBoolean.typ,pp))
  def Top(typ:TouchType): RichExpression = toRichExpression(new ExpressionSet(typ).top())
  def Bottom(typ:TouchType): RichExpression = toRichExpression(new ExpressionSet(typ).bottom())
  def Invalid(typ:Type)(implicit pp:ProgramPoint) :RichExpression = RichExpression(new Constant("invalid",typ,pp))
  def Valid(typ:Type)(implicit pp:ProgramPoint) :RichExpression = RichExpression(new Constant("valid",typ,pp))

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

  implicit def toExpression(value:RichExpression) : Expression =
    value.thisExpr

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
}