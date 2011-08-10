package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, Type}
import ch.ethz.inf.pm.sample.property.Property
import math.Ordering.Unit
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph
import reflect.AnyValManifest

object Settings {
  def symbolicInt[T <: SymbolicInt[T]] : T = new LinearSum(new Summation(Map.empty), 0).asInstanceOf[T];
}

class SingleSymbolicInterval[T <: SymbolicInt[T]](val l : T, val r : T) extends Lattice[SingleSymbolicInterval[T]]{
  private var isTop=false;
  private var isBottom=false;

  def this() = {
     this(null.asInstanceOf[T], null.asInstanceOf[T])
  }

  def lessEqual(r: SingleSymbolicInterval[T]) : Boolean = {
    if(this.isBottom) return true;
    if(r.isBottom) return false;
    if(r.isTop) return true;
    if(this.isTop) return false;
    r.l.<=(r.l, this.l) && this.r.<=(this.r, r.r)
  };

  def widening(left: SingleSymbolicInterval[T], right: SingleSymbolicInterval[T]) : SingleSymbolicInterval[T] = {
    if(left.isTop || right.isTop) return top();
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    if(right.lessEqual(left)) return left;
    else return top();//TODO: increase the precision of this operator
  }

  def glb(left: SingleSymbolicInterval[T], right: SingleSymbolicInterval[T]) : SingleSymbolicInterval[T] = {
    if(left.isBottom || right.isBottom) return bottom();
    if(left.isTop) return right;
    if(right.isTop) return left;
    return new SingleSymbolicInterval(left.l.max(left.l, right.l), left.r.min(left.r, right.r))
  }

  def lub(left: SingleSymbolicInterval[T], right: SingleSymbolicInterval[T]) : SingleSymbolicInterval[T] = {
    if(left.isTop || right.isTop) return top();
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    return new SingleSymbolicInterval(left.l.min(left.l, right.l), left.r.max(left.r, right.r))
  }

  def bottom() = {
    val result : SingleSymbolicInterval[T] = new SingleSymbolicInterval(null.asInstanceOf[T], null.asInstanceOf[T]);
    result.isBottom=true;
    result;
  }

  def top() = {
    val result : SingleSymbolicInterval[T] = new SingleSymbolicInterval(null.asInstanceOf[T], null.asInstanceOf[T]);
    result.isTop=true;
    result;
  }

  def factory() = this.top();

  override def equals(o : Any) : Boolean = o match {
    case x : SingleSymbolicInterval[T] =>
      if(this.isTop && x.isTop) return true;
      if(this.isBottom && x.isBottom) return true;
      if(this.isTop || x.isTop || this.isBottom || x.isBottom) return false;
      this.l.equals(x.l) && this.r.equals(x.r)
    case _ => false;
  }

  override def toString() : String = {
    if(isTop) return "T";
    if(isBottom) return "_|_";
    "["+l.toString+" ... "+r.toString()+"]"
  }
}

class SymbolicIntervals[T <: SymbolicInt[T]] extends SimplifiedSemanticDomain[SymbolicIntervals[T]] with BoxedDomain[SingleSymbolicInterval[T], SymbolicIntervals[T]] {
  def removeVariable(variable: Identifier) : SymbolicIntervals[T] = {
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

  private def eval(expr : Expression) : SingleSymbolicInterval[T] = expr match {
    case AbstractMethodCall(thisExpr, parameters, calledMethod, retType) =>
      new SingleSymbolicInterval(
        Settings.symbolicInt.factory(new IntervalsSymbolicValues(calledMethod, SymbolicContractTypes.min)),
        Settings.symbolicInt.factory(new IntervalsSymbolicValues(calledMethod, SymbolicContractTypes.max))
      )
    case BinaryArithmeticExpression(left, right, op, returntyp) =>
      val l = this.eval(left)
      val r = this.eval(right)
      if(l.equals(l.bottom()) || r.equals(r.bottom())) return l.bottom();
      if(l.equals(l.top()) || r.equals(r.top())) return l.top();
      op match {
        case ArithmeticOperator.+ => return new SingleSymbolicInterval[T](l.l.+(l.l, r.l), r.r.+(l.r, r.r))
        case ArithmeticOperator.- =>
          val c1 = l.l.-(l.l, r.l)
          val c2 = l.l.-(l.l, r.r)
          val c3 = l.l.-(l.r, r.l)
          val c4 = l.l.-(l.r, r.r)
          val min1 = l.l.min(c1, c2)
          val min2 = l.l.min(c3, c4)
          val min = min1.min(min1, min2);
          val max1 = l.l.max(c1, c2)
          val max2 = l.l.max(c3, c4)
          val max = max1.max(max1, max2);
          return new SingleSymbolicInterval[T](min, max)
      }
    case x : VariableIdentifier => this.get(x);
    case Constant(v, typ, pp) =>  new SingleSymbolicInterval(
        Settings.symbolicInt.factory(Integer.parseInt(v)),
        Settings.symbolicInt.factory(Integer.parseInt(v))
      )

  }

  def get(key: Identifier) = this.value.get(key) match {
    case Some(s) => s;
    case None => new SingleSymbolicInterval[T]().top();
  }

  override def factory() = new SymbolicIntervals[T]();


}

object SymbolicContractTypes extends Enumeration {
  val min = Value("min");
  val max = Value("max");
}

class IntervalsSymbolicValues(val method : String, val typ : SymbolicContractTypes.Value) extends SymbolicValue {
  override def equals(o : Any) = o match {
    case x : IntervalsSymbolicValues => method.equals(x.method) && typ.equals(x.typ);
    case _ => false;
  }
  override def toString() = "C( "+method+", "+typ+")"
}

class SymbolicIntervalsAnalysis[T <: SymbolicInt[T]] extends SemanticAnalysis[SymbolicIntervals[T]] {

  override def getNativeMethodsSemantics() : List[NativeMethodSemantics] = List(SymbolicIntervalsNativeMethodSemantics)

  override def reset() = Unit;

  override def getLabel() = "Symbolic intervals inference"

  override def parameters() : List[(String, Any)] = Nil;

  override def setParameter(label : String, value : Any) : Unit = throw new SymbolicIntException("Paramters not supported")

  override def getInitialState() : SymbolicIntervals[T] = new SymbolicIntervals[T]();

  override def getProperties() : Set[Property] = Set(ShowGraph);
}