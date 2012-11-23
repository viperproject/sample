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

  def Expr[S <: State[S]](e:Expression)(implicit state:S, pp:ProgramPoint):S = {
    state.setExpression(toRichExpression(e))
  }

  def Field[S <: State[S]](obj:RichExpression, field:VariableIdentifier)(implicit state:S, pp:ProgramPoint):RichExpression = {
    state.getFieldValue(List(obj),field.getName(),field.getType()).getExpression()
  }

  def exprOf[S <: State[S]](s:S):(Expression,S) = {
    val exp = s.getExpression().setOfExpressions.head
    (exp,s)
  }

  def invalid(typ:Type):RichExpression = {
    RichExpression(new Constant("invalid",typ,null))
  }

  def valid(typ:Type):RichExpression = {
    RichExpression(new Constant("valid",typ,null))
  }

  def updateState[S <: State[S]](s:S,e:ExpressionSet):ExpressionSet = {
    val numOfExpressions = (e.setOfExpressions.size)
    if (numOfExpressions != 1) throw new TouchException("Generic Abstract State fucked me over again!")
    toExpressionSet(e.setOfExpressions.head)
  }

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

  /**
   * Creates a new Object of type typ, and initializes its fields with the given arguments.
   */
  def New[S <: State[S]](typ:TouchType, initials:Map[Identifier,ExpressionSet])(implicit s:S, pp:ProgramPoint): S = {
    val (obj,state1) = exprOf(s.createObject(typ,pp))
    //if(state1.top().lessEqual(state1)) return state1
    var curState = state1;

    // Assign fields with given arguments
    for (f <- typ.getPossibleFields()) {
      val a = initials.get(f) match {
        case None => toExpressionSet(f.asInstanceOf[TouchField].default)
        case Some(s) => s
      }
      curState = curState.assignField(List(toExpressionSet(obj)),f.getName(),updateState(curState,a))
    }

    // Make sure that our value is "valid"  now
    curState = curState.assignVariable(toExpressionSet(obj),valid(typ))

    curState.setExpression(toExpressionSet(obj))
  }

  def New[S <: State[S]](typ:TouchType, args:RichExpression*)(implicit s:S, pp:ProgramPoint): S = {
    New[S](typ,(typ.getPossibleFieldsSorted() zip (args map { x:RichExpression => toExpressionSet(x)} toList)).toMap)(s,pp)
  }

  def New[S <: State[S]](typ:TouchType)(implicit s:S, pp:ProgramPoint): S = {
    New[S](typ,Map.empty[Identifier,ExpressionSet])(s,pp)
  }

  def Skip[S <: State[S]](implicit state:S, pp:ProgramPoint): S = state

  def SetEnv[S <: State[S]](id:EnvironmentIdentifier,value:ExpressionSet)(implicit state:S, pp:ProgramPoint): S = {
    state.assignVariable(new ExpressionSet(id.typ).add(id),value)
  }

  def AssignField[S <: State[S]](obj:ExpressionSet,field:Identifier,value:ExpressionSet)(implicit state:S, pp:ProgramPoint): S = {
    state.assignField(List(obj),field.getName(),value)
  }

  def Top[S <: State[S]](typ:TouchType)(implicit s:S, pp:ProgramPoint): S = {
    s.setExpression(new ExpressionSet(typ).top())
  }

  def Unimplemented[S <: State[S]](method:String)(implicit state:S, pp:ProgramPoint): S = {
    println(method+" not implemented, unsound from now on, at "+pp)
    Skip[S]
  }

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

//  implicit def toSymbolicAbstractValue[S <: State[S]](value:RichExpression)(implicit s:S) : SymbolicAbstractValue[S] =
//    new SymbolicAbstractValue[S](value.thisExpr,s)

  implicit def toRichExpression(value:Expression) : RichExpression =
    RichExpression(value)

  implicit def toExpression(value:RichExpression) : Expression =
    value.thisExpr

  implicit def toExpressionSet(value:RichExpression) : ExpressionSet =
    (new ExpressionSet(value.thisExpr.getType())).add(value.thisExpr)


}

class TouchField(name:String, typ:TouchType, var default: RichExpression = null) extends VariableIdentifier(name,typ,null) {

  if (default == null) {
    default = RichExpression(new Constant("valid",typ,null))
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