package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, Type}
import ch.ethz.inf.pm.sample.property.Property
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph
import com.sun.xml.internal.bind.v2.model.core.NonElement
import com.sun.org.apache.bcel.internal.generic.VariableLengthInstruction
import scala.Array

object Settings {
  def symbolicInt[T <: SymbolicInt[T, S], S <: SymbolicValue[S]] : T =
    new LinearSum(
      new Summation(Map.empty[S, Coefficient[T, S]], new IntervalsSymbolicValues("a", SymbolicContractTypes.min).asInstanceOf[S]),
      0,
      new IntervalsSymbolicValues("a", SymbolicContractTypes.min).asInstanceOf[S]).asInstanceOf[T];
}

/*
sealed abstract class ValueOrZero[+T] {
  def get : T;
}
case object Zero extends ValueOrZero[Nothing] {
  def get = throw new SemanticException("Not allowed")
}
case class Value[T](val g : T) extends ValueOrZero[T] {
  if(g==null) throw new SymbolicIntException("Not allowed");
  def get=g;
} */

object DBMSetting {
  private var idToIndex : Map[Identifier, Int] = Map.empty;
  private var nextFreeInt : Int = 0;

  def containID(id : Identifier) : Boolean = idToIndex.keySet.contains(id)

  def getIndex(id : Identifier) : Int =
    if(this.idToIndex.keySet.contains(id))
      this.idToIndex.apply(id);
    else {this.idToIndex=this.idToIndex+((id, nextFreeInt)); nextFreeInt=nextFreeInt+1; nextFreeInt-1;}
}

class SymbolicDBM[T <: SymbolicInt[T, S], S <: SymbolicValue[S]] extends SimplifiedSemanticDomain[SymbolicDBM[T, S]] {
  var matrix : Map[(Int, Int), T] = Map.empty;

  private def this(m : Map[(Int, Int), T]) = {
    this();
    matrix=m;
  }

  def removeVariable(variable: Identifier) : SymbolicDBM[T, S] = {
    if(DBMSetting.containID(variable)) {
      var newMatrix : Map[(Int, Int), T] = matrix;
      val index = DBMSetting.getIndex(variable);
      for((id1, id2) <- matrix.keySet)
        if(id1==index || id2==index)
          newMatrix=newMatrix-((id1, id2));
      return new SymbolicDBM(newMatrix);
    }
    else return this;
  }

  def setToTop(variable: Identifier) = this.removeVariable(variable)

  def createVariable(variable: Identifier, typ: Type) = this;

  def assume(expr: Expression) = this;

  def assign(variable: Identifier, expr: Expression) = expr match {
    case BinaryArithmeticExpression(left, right, op, returntyp) =>
      if(right.isInstanceOf[Constant] &&
        left.isInstanceOf[Identifier] &&
        (op.equals(ArithmeticOperator.+) || op.equals(ArithmeticOperator.-))
      ) {
          var k : Int = Integer.parseInt(right.asInstanceOf[Constant].constant);
          if(op.equals(ArithmeticOperator.-)) k = 0 - k;
          if(! left.equals(variable))
            this.removeVariable(variable).assume(new BinaryArithmeticExpression(variable, expr, ArithmeticOperator.==, returntyp));
          else if(! DBMSetting.containID(variable))
            this.removeVariable(variable)
          else {
            var newMatrix : Map[(Int, Int), T] = matrix;
            val index = DBMSetting.getIndex(variable);
            for((id1, id2) <- matrix.keySet) {
              val oldValue = matrix.apply((id1, id2));
              if(id1==index)
                newMatrix=newMatrix+(((id1, id2), oldValue.+(oldValue, oldValue.factory(k))));
              if(id2==index)
                newMatrix=newMatrix+(((id1, id2), oldValue.+(oldValue, oldValue.factory(0-k))));
            }
            new SymbolicDBM(newMatrix);
          }
      }
      else this.removeVariable(variable);
  }


  override def factory() = new SymbolicDBM[T, S]();


}
       /*
object SymbolicContractTypes extends Enumeration {
  val min = Value("min");
  val max = Value("max");
}

class IntervalsSymbolicValues(val method : String, val typ : SymbolicContractTypes.Value) extends SymbolicValue[IntervalsSymbolicValues] {
  override def equals(o : Any) = o match {
    case x : IntervalsSymbolicValues => method.equals(x.method) && typ.equals(x.typ);
    case _ => false;
  }

  def <=(a: IntervalsSymbolicValues, b : IntervalsSymbolicValues) : Boolean =
    a.method.equals(b.method) && a.typ==SymbolicContractTypes.min && b.typ==SymbolicContractTypes.max

  override def toString() = "C( "+method+", "+typ+")"
}

class SymbolicIntervalsAnalysis[T <: SymbolicInt[T, S], S <: SymbolicValue[S]] extends SemanticAnalysis[SymbolicIntervals[T, S]] {

  override def getNativeMethodsSemantics() : List[NativeMethodSemantics] = List(SymbolicIntervalsNativeMethodSemantics)

  override def reset() = Unit;

  override def getLabel() = "Symbolic intervals inference"

  override def parameters() : List[(String, Any)] = Nil;

  override def setParameter(label : String, value : Any) : Unit = throw new SymbolicIntException("Paramters not supported")

  override def getInitialState() : SymbolicIntervals[T, S] = new SymbolicIntervals[T, S]();

  override def getProperties() : Set[Property] = Set(ShowGraph);
}*/