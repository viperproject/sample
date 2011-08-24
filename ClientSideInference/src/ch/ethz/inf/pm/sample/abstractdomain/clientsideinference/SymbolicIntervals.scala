package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, Type}
import ch.ethz.inf.pm.sample.property.Property
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph
import com.sun.xml.internal.bind.v2.model.core.NonElement
import com.sun.org.apache.bcel.internal.generic.VariableLengthInstruction

object Settings {
  def symbolicInt[T <: SymbolicInt[T, S], S <: SymbolicValue[S]] : T =
    new LinearSum(
      new Summation(Map.empty[S, Coefficient[T, S]], new IntervalsSymbolicValues("a", SymbolicContractTypes.min).asInstanceOf[S]),
      0,
      new IntervalsSymbolicValues("a", SymbolicContractTypes.min).asInstanceOf[S]).asInstanceOf[T];
}


sealed abstract class BoundOrInfinity[+T] {
  def get : T;
}
case object PlusInfinity extends BoundOrInfinity[Nothing] {
  def get = throw new SemanticException("Not allowed")
}
case object MinusInfinity extends BoundOrInfinity[Nothing] {
  def get = throw new SemanticException("Not allowed")
}
case class Bound[T](val g : T) extends BoundOrInfinity[T] {
  if(g==null) throw new SymbolicIntException("Not allowed");
  def get=g;
}

class SingleSymbolicInterval[T <: SymbolicInt[T, S], S <: SymbolicValue[S]](val l : BoundOrInfinity[T], val r : BoundOrInfinity[T]) extends Lattice[SingleSymbolicInterval[T, S]]{
  if(l==PlusInfinity || r==MinusInfinity) throw new SymbolicIntException("Not allowed");

  private def isTop= ! isBottom && l == MinusInfinity && r == PlusInfinity
  private var isBottom=false;

  def lessEqual(r: SingleSymbolicInterval[T, S]) : Boolean = {
    if(this.isBottom) return true;
    if(r.isBottom) return false;
    if(r.isTop) return true;
    if(this.isTop) return false;

    (r.l==MinusInfinity || (this.l!=MinusInfinity && r.l.get.<=(r.l.get, this.l.get))) &&
    (this.r==PlusInfinity || (r.r!=PlusInfinity &&  this.r.get.<=(this.r.get, r.r.get)))
  };

  def widening(left: SingleSymbolicInterval[T, S], right: SingleSymbolicInterval[T, S]) : SingleSymbolicInterval[T, S] = {
    if(left.isTop || right.isTop) return top();
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    if(right.lessEqual(left)) return left;
    var newleft =
      if(left.l==MinusInfinity || right.l==MinusInfinity) MinusInfinity
      else if(left.l.get.equals(right.l.get)) Bound(left.l.get)
      else MinusInfinity;
    var newright =
      if(left.r==PlusInfinity || right.r==PlusInfinity) PlusInfinity
      else if(left.r.get.equals(right.r.get)) Bound(left.r.get)
      else PlusInfinity;
    return new SingleSymbolicInterval[T, S](newleft, newright);
  }

  def glb(left: SingleSymbolicInterval[T, S], right: SingleSymbolicInterval[T, S]) : SingleSymbolicInterval[T, S] = {
    if(left.isBottom || right.isBottom) return bottom();
    if(left.isTop) return right;
    if(right.isTop) return left;
    if(left.equals(right)) return left;
    var newleft =
      if(left.l==MinusInfinity) right.l
      else if(right.l==MinusInfinity) left.l
      else Bound(left.l.get.max(left.l.get, right.l.get))
    var newright =
      if(left.r==PlusInfinity) right.r
      else if(right.r==PlusInfinity) left.r
      else Bound(left.r.get.min(left.r.get, right.r.get))
    return new SingleSymbolicInterval(newleft, newright)
  }

  def lub(left: SingleSymbolicInterval[T, S], right: SingleSymbolicInterval[T, S]) : SingleSymbolicInterval[T, S] = {
    if(left.isTop || right.isTop) return top();
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    if(left.equals(right)) return left;
    var newleft =
      if(left.l==MinusInfinity || right.l==MinusInfinity) MinusInfinity
      else Bound(left.l.get.min(left.l.get, right.l.get))
    var newright =
      if(left.r==PlusInfinity || right.r==PlusInfinity) PlusInfinity
      else Bound(left.r.get.max(left.r.get, right.r.get))
    return new SingleSymbolicInterval(newleft, newright)
  }

  def bottom() = {
    val result : SingleSymbolicInterval[T, S] = new SingleSymbolicInterval(MinusInfinity, PlusInfinity);
    result.isBottom=true;
    result;
  }

  def top() = new SingleSymbolicInterval(MinusInfinity, PlusInfinity);

  def factory() = this.top();

  override def equals(o : Any) : Boolean = o match {
    case x : SingleSymbolicInterval[T, S] =>
      if(this.isTop && x.isTop) return true;
      if(this.isBottom && x.isBottom) return true;
      if(this.isTop || x.isTop || this.isBottom || x.isBottom) return false;
      this.l.equals(x.l) && this.r.equals(x.r)
    case _ => false;
  }

  override def toString() : String = {
    if(isTop) return "T";
    if(isBottom) return "_|_";
    var result = "[";
    l match {
      case MinusInfinity => result = result + "-inf ... "
      case Bound(s) => result = result + s.toString + " ... "
    }
    r match {
      case PlusInfinity => result = result + "+inf]"
      case Bound(s) => result = result + s.toString + "]"
    }
    result
  }
}

class SymbolicIntervals[T <: SymbolicInt[T, S], S <: SymbolicValue[S]] extends SimplifiedSemanticDomain[SymbolicIntervals[T, S]] with BoxedDomain[SingleSymbolicInterval[T, S], SymbolicIntervals[T, S]] {
  def removeVariable(variable: Identifier) : SymbolicIntervals[T, S] = {
    val result=this.factory()
    result.value=this.value.-(variable);
    return result;
  }

  def setToTop(variable: Identifier) = this.removeVariable(variable)

  def createVariable(variable: Identifier, typ: Type) = this;

  def assume(expr: Expression) = this;

  def assign(variable: Identifier, expr: Expression) = {
    var result = this.factory();
    result.value=this.value+((variable, this.eval(expr)));
    result;
  }

  private def +(l : BoundOrInfinity[T], r : BoundOrInfinity[T]) = {
    if(l==PlusInfinity || r==PlusInfinity) PlusInfinity;
    else if(l==MinusInfinity || r==MinusInfinity) MinusInfinity ;
    else new Bound(l.get.+(l.get, r.get));
  }

  private def -(l : BoundOrInfinity[T], r : BoundOrInfinity[T]) = {
    if(l==PlusInfinity || r==MinusInfinity) PlusInfinity;
    else if(l==MinusInfinity || r==PlusInfinity) MinusInfinity;
    else new Bound(l.get.-(l.get, r.get));
  }

  private def *(l : BoundOrInfinity[T], r : BoundOrInfinity[T]) = {
    //TODO: this is unsound, +inf*-1!
    if(l==PlusInfinity || r==PlusInfinity) PlusInfinity;
    else if(l==MinusInfinity || r==MinusInfinity) MinusInfinity;
    else {
      val result = l.get.*(l.get, r.get);
      if(result==null)
        PlusInfinity
      else new Bound(result);
    }
  }

  private def min(l : BoundOrInfinity[T], r : BoundOrInfinity[T]) = {
    if(l==MinusInfinity || r==MinusInfinity) MinusInfinity;
    else if(l==PlusInfinity) r;
    else if(r==PlusInfinity) l;
    else new Bound(l.get.min(l.get, r.get));
  }

  private def max(l : BoundOrInfinity[T], r : BoundOrInfinity[T]) = {
    if(l==PlusInfinity || r==PlusInfinity) PlusInfinity;
    else if(l==MinusInfinity) r;
    else if(r==MinusInfinity) l;
    else new Bound(l.get.max(l.get, r.get));
  }

  private def eval(expr : Expression) : SingleSymbolicInterval[T, S] = expr match {
    case AbstractMethodCall(thisExpr, parameters, calledMethod, retType) =>
      new SingleSymbolicInterval(
        Bound(Settings.symbolicInt.factory(new IntervalsSymbolicValues(calledMethod, SymbolicContractTypes.min))),
        Bound(Settings.symbolicInt.factory(new IntervalsSymbolicValues(calledMethod, SymbolicContractTypes.max)))
      )
    case BinaryArithmeticExpression(left, right, op, returntyp) =>
      val l = this.eval(left)
      val r = this.eval(right)
      if(l.equals(l.bottom()) || r.equals(r.bottom())) return l.bottom();
      if(l.equals(l.top()) || r.equals(r.top())) return l.top();
      op match {
        case ArithmeticOperator.+ => return new SingleSymbolicInterval[T, S](this.+(l.l, r.l), this.+(l.r, r.r))
        case ArithmeticOperator.- => return combineArithmeticOperator(l, r, this.-(_, _))
        case ArithmeticOperator.* => return combineArithmeticOperator(l, r, this.*(_, _))
      }
    case x : VariableIdentifier => this.get(x);
    case Constant(v, typ, pp) =>  new SingleSymbolicInterval(
        Bound(Settings.symbolicInt.factory(Integer.parseInt(v))),
        Bound(Settings.symbolicInt.factory(Integer.parseInt(v)))
      )

  }

  private def combineArithmeticOperator(l : SingleSymbolicInterval[T, S], r : SingleSymbolicInterval[T, S], f : (BoundOrInfinity[T], BoundOrInfinity[T]) => BoundOrInfinity[T]) : SingleSymbolicInterval[T, S] = {
          val c1 = f(l.l, r.l)
          val c2 = f(l.l, r.r)
          val c3 = f(l.r, r.l)
          val c4 = f(l.r, r.r)
          val min1 = this.min(c1, c2)
          val min2 = this.min(c3, c4)
          val min = this.min(min1, min2);
          val max1 = this.max(c1, c2)
          val max2 = this.max(c3, c4)
          val max = this.max(max1, max2);
          return new SingleSymbolicInterval[T, S]( if(min == PlusInfinity) MinusInfinity else min, if(max == MinusInfinity) PlusInfinity else max)
  }

  def get(key: Identifier) = this.value.get(key) match {
    case Some(s) => s;
    case None => new SingleSymbolicInterval[T, S](MinusInfinity, PlusInfinity);
  }

  override def factory() = new SymbolicIntervals[T, S]();


}

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
}