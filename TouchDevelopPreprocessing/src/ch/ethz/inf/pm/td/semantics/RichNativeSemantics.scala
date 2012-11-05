package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, NativeMethodSemantics, Type}
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression
import ch.ethz.inf.pm.sample.abstractdomain.BinaryBooleanExpression
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.td.compiler.{TouchException, TouchType}

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

trait RichNativeSemantics  extends NativeMethodSemantics {

  var seenErrors = Set[(String,ProgramPoint)]()

  val Boolean = TouchType("Boolean",false)
  val Number = TouchType("Number",false)

  def stateWith[S <: State[S]](s:S,e:Expression):Option[S] = {
    Some(s.setExpression(toRichExpression(e)))
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

  def Error[S <: State[S]](expr:RichExpression, message:String)(implicit s:S, pp:ProgramPoint) {
    if (!seenErrors.contains((message,pp))) {
      // The next line is a fix for "bottom" expression, where I don't know where they come from.
      val errorState = s.assume( expr ).setExpression(new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(SystemParameters.typ.top(),pp)))
      if(!errorState.lessEqual(s.bottom())) {
        println(message+" at line "+pp.getLine()+", column "+pp.getColumn())
        seenErrors += ((message,pp))
      }
    }
  }

  /**
   * Creates a new Object of type typ, and initializes its fields with the given arguments.
   */
  def New[S <: State[S]](typ:TouchType, args:List[ExpressionSet])(implicit s:S, pp:ProgramPoint): S = {
    val (obj,state1) = exprOf(s.createObject(typ,pp))
    var curState = state1;

    // Assign fields with given aruments
    for ((f,a) <- typ.getPossibleFields() zip args) {
      curState = curState.assignField(List(toExpressionSet(obj)),f.getName(),updateState(curState,a))
    }
    // Make sure that our value is "valid"  now
    curState = curState.assignVariable(toExpressionSet(obj),valid(typ))

    curState.setExpression(toExpressionSet(obj))
  }

  def New[S <: State[S]](typ:TouchType, args:RichExpression*)(implicit s:S, pp:ProgramPoint): S = {
    New[S](typ,args map { x:RichExpression => toExpressionSet(x)} toList)(s,pp)
  }

  def Top[S <: State[S]](typ:TouchType)(implicit s:S, pp:ProgramPoint): S = {
    s.setExpression(new ExpressionSet(typ).top())
  }

  implicit def toRichExpression[S <: State[S]](value:ExpressionSet) : RichExpression = {
    val numOfExpressions = (value.setOfExpressions.size)
    if (numOfExpressions > 1) throw new TouchException("Generic Abstract State fucked me over again!")
    else if (numOfExpressions < 1 ) toRichExpression(0).to(toRichExpression(1000)) // TODO TODO TODO TODO
    else RichExpression(value.setOfExpressions.head)
  }

  implicit def toRichExpression(value:Int) : RichExpression =
    RichExpression(new Constant(value.toString,Number,null))

  implicit def toRichExpression(value:Double) : RichExpression =
    RichExpression(new Constant(value.toString,Number,null))

//  implicit def toSymbolicAbstractValue[S <: State[S]](value:RichExpression)(implicit s:S) : SymbolicAbstractValue[S] =
//    new SymbolicAbstractValue[S](value.thisExpr,s)

  implicit def toRichExpression(value:Expression) : RichExpression =
    RichExpression(value)

  implicit def toExpression(value:RichExpression) : Expression =
    value.thisExpr

  implicit def toExpressionSet(value:RichExpression) : ExpressionSet =
    (new ExpressionSet(value.thisExpr.getType())).add(value.thisExpr)

  case class RichExpression(thisExpr : Expression) {

    implicit def toExpression(value:RichExpression) : Expression =
      value.thisExpr

    def <= (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.<=, Boolean))

    def >= (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.>=,Boolean))

    def < (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.<, Boolean))

    def > (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.>, Boolean))

    def equal (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.==, Boolean))

    def unequal (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.!=, Boolean))

    def + (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.+, Number))

    def * (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.*, Number))

    def - (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.-, Number))

    def / (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator./, Number))

    def or (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryNondeterministicExpression(thisExpr,thatExpr,NondeterministicOperator.or,Number))

    def to (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryNondeterministicExpression(thisExpr,thatExpr,NondeterministicOperator.to,Number))

    def && (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryBooleanExpression(thisExpr,thatExpr,BooleanOperator.&&,Boolean))

    def || (thatExpr : RichExpression) : RichExpression =
      RichExpression(new BinaryBooleanExpression(thisExpr,thatExpr,BooleanOperator.||,Boolean))
  }

}

