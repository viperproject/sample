package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._

sealed abstract class BoundOrInfinity[+T] {
  def get : T;
}
case object PlusInfinity extends BoundOrInfinity[Nothing] {
  def get = throw new SemanticException("Not allowed")
}
case object MinusInfinity extends BoundOrInfinity[Nothing] {
  def get = throw new SemanticException("Not allowed")
}
case class Bound[T <: SymbolicInt[T, S], S <: SymbolicValue[S]](val g : T) extends BoundOrInfinity[T] {
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
      else if(left.l.get.equals(right.l.get)) Bound[T,S](left.l.get)
      else MinusInfinity;
    var newright =
      if(left.r==PlusInfinity || right.r==PlusInfinity) PlusInfinity
      else if(left.r.get.equals(right.r.get)) Bound[T,S](left.r.get)
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
      else Bound[T,S](left.l.get.max(left.l.get, right.l.get))
    var newright =
      if(left.r==PlusInfinity) right.r
      else if(right.r==PlusInfinity) left.r
      else Bound[T,S](left.r.get.min(left.r.get, right.r.get))
    return new SingleSymbolicInterval(newleft, newright)
  }

  def lub(left: SingleSymbolicInterval[T, S], right: SingleSymbolicInterval[T, S]) : SingleSymbolicInterval[T, S] = {
    if(left.isTop || right.isTop) return top();
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    if(left.equals(right)) return left;
    var newleft =
      if(left.l==MinusInfinity || right.l==MinusInfinity) MinusInfinity
      else Bound[T,S](left.l.get.min(left.l.get, right.l.get))
    var newright =
      if(left.r==PlusInfinity || right.r==PlusInfinity) PlusInfinity
      else Bound[T,S](left.r.get.max(left.r.get, right.r.get))
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

  override def createVariableForArgument(variable : Identifier, typ : Type, path : List[String]) : (SymbolicIntervals[T, S], Map[Identifier, List[String]]) = {
    if(! typ.isNumericalType()) return super.createVariableForArgument(variable, typ, path);
    if(typ.isNumericalType() && ! path.isEmpty) throw new SymbolicDBMException("Not yet supported");
    var (state, d2) = super.createVariableForArgument(variable, typ, path);
    state.value=state.value+((variable, new SingleSymbolicInterval(
        Bound(SymbolicSettings.symbolicInt.factory(new IntervalsSymbolicValues(SystemParameters.currentClass, SystemParameters.currentMethod, TypeOfContracts.precondition, variable, SymbolicContractTypes.min))),
        Bound(SymbolicSettings.symbolicInt.factory(new IntervalsSymbolicValues(SystemParameters.currentClass, SystemParameters.currentMethod, TypeOfContracts.precondition, variable, SymbolicContractTypes.max)))
      )))
    (state, d2)
  }

  def setToTop(variable: Identifier) = this.removeVariable(variable)

  def createVariable(variable: Identifier, typ: Type) = this;

  def assume(expr: Expression) = this;

  def assign(variable: Identifier, expr: Expression) = {
    var result = this.factory();
    result.value=this.value+((variable, this.eval(expr, variable)));
    result;
  }

  private def +(l : BoundOrInfinity[T], r : BoundOrInfinity[T]) = {
    if(l==PlusInfinity || r==PlusInfinity) PlusInfinity;
    else if(l==MinusInfinity || r==MinusInfinity) MinusInfinity ;
    else new Bound[T,S](l.get.+(l.get, r.get));
  }

  private def -(l : BoundOrInfinity[T], r : BoundOrInfinity[T]) = {
    if(l==PlusInfinity || r==MinusInfinity) PlusInfinity;
    else if(l==MinusInfinity || r==PlusInfinity) MinusInfinity;
    else new Bound[T,S](l.get.-(l.get, r.get));
  }

  private def *(l : BoundOrInfinity[T], r : BoundOrInfinity[T]) = {
    //TODO: this is unsound, +inf*-1!
    if(l==PlusInfinity || r==PlusInfinity) PlusInfinity;
    else if(l==MinusInfinity || r==MinusInfinity) MinusInfinity;
    else {
      val result = l.get.*(l.get, r.get);
      if(result==null)
        PlusInfinity
      else new Bound[T,S](result);
    }
  }

  private def min(l : BoundOrInfinity[T], r : BoundOrInfinity[T]) = {
    if(l==MinusInfinity || r==MinusInfinity) MinusInfinity;
    else if(l==PlusInfinity) r;
    else if(r==PlusInfinity) l;
    else new Bound[T,S](l.get.min(l.get, r.get));
  }

  private def max(l : BoundOrInfinity[T], r : BoundOrInfinity[T]) = {
    if(l==PlusInfinity || r==PlusInfinity) PlusInfinity;
    else if(l==MinusInfinity) r;
    else if(r==MinusInfinity) l;
    else new Bound[T,S](l.get.max(l.get, r.get));
  }

  def eval(expr : Expression, id : Identifier) : SingleSymbolicInterval[T, S] = expr match {
    case AbstractMethodCall(thisExpr, parameters, calledMethod, retType) =>
      //TODO: thisExpr.getName() could be wrong since it could refer to the implementation of the method on a superclass
      new SingleSymbolicInterval(
        Bound(SymbolicSettings.symbolicInt.factory(new IntervalsSymbolicValues(thisExpr.getType(), calledMethod, TypeOfContracts.postcondition, new VariableIdentifier("result", id.getType(), id.getProgramPoint()), SymbolicContractTypes.min))),
        Bound(SymbolicSettings.symbolicInt.factory(new IntervalsSymbolicValues(thisExpr.getType(), calledMethod, TypeOfContracts.postcondition, new VariableIdentifier("result", id.getType(), id.getProgramPoint()), SymbolicContractTypes.max)))
      )
    case BinaryArithmeticExpression(left, right, op, returntyp) =>
      val l = this.eval(left, id)
      val r = this.eval(right, id)
      if(l.equals(l.bottom()) || r.equals(r.bottom())) return l.bottom();
      if(l.equals(l.top()) || r.equals(r.top())) return l.top();
      op match {
        case ArithmeticOperator.+ => return new SingleSymbolicInterval[T, S](this.+(l.l, r.l), this.+(l.r, r.r))
        case ArithmeticOperator.- => return combineArithmeticOperator(l, r, this.-(_, _))
        case ArithmeticOperator.* => return combineArithmeticOperator(l, r, this.*(_, _))
      }
    case x : VariableIdentifier => this.get(x);
    case Constant(v, typ, pp) =>  new SingleSymbolicInterval(
        Bound(SymbolicSettings.symbolicInt.factory(Integer.parseInt(v))),
        Bound(SymbolicSettings.symbolicInt.factory(Integer.parseInt(v)))
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

class IntervalsSymbolicValues(val classe : Type, val method : String, val typeOfContracts : TypeOfContracts.Value, val id : Identifier, val typ : SymbolicContractTypes.Value) extends SymbolicValue[IntervalsSymbolicValues] {
  override def equals(o : Any) = o match {
    case x : IntervalsSymbolicValues =>
      id.equals(x.id) && classe.equals(x.classe) && method.equals(x.method) && typeOfContracts.equals(x.typeOfContracts)&& typ.equals(x.typ);
    case _ => false;
  }

  override def hashCode() : Int = method.hashCode();

  def <=(a: IntervalsSymbolicValues, b : IntervalsSymbolicValues) : Boolean =
    a.method.equals(b.method) && a.typ==SymbolicContractTypes.min && b.typ==SymbolicContractTypes.max

  override def toString() = typeOfContracts+"( "+classe+", "+method+", "+id+", "+typ+")"
}

class SymbolicIntervalsAnalysis[T <: SymbolicInt[T, IntervalsSymbolicValues]] extends SemanticAnalysis[SymbolicIntervals[T, IntervalsSymbolicValues]] {

  override def getNativeMethodsSemantics() : List[NativeMethodSemantics] = List(SymbolicNativeMethodSemantics)

  override def reset() = Unit;

  override def getLabel() = "Symbolic intervals inference"

  override def parameters() : List[(String, Any)] = Nil;

  override def setParameter(label : String, value : Any) : Unit = throw new SymbolicIntException("Paramters not supported")

  override def getInitialState() : SymbolicIntervals[T, IntervalsSymbolicValues] = new SymbolicIntervals[T, IntervalsSymbolicValues]();

  override def getProperties() : Set[Property] = Set(ShowGraph, new SymbolicIntervalsProperty(new ConstraintsInference[IntervalsSymbolicValues, T]()));
}


class SymbolicIntervalsProperty[T <: SymbolicInt[T, IntervalsSymbolicValues]](val c : ConstraintsInference[IntervalsSymbolicValues, T]) extends Property {
  val v = new SymbolicIntervalsMethodCallVisitor[T](c);
  val property1 = new SingleStatementProperty(v)

  def getLabel() : String = "Contract Inference for Symbolic Intervals";

  def finalizeChecking(printer : OutputCollector) : Unit = c.printConstraints()

  def check[S <: State[S]](classe : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = {
    property1.check(classe, methodName, result, printer);
    def exitState = result.exitState();
    for(exp <- exitState.getExpression().getExpressions()) {
      val state = SymbolicIntervalsUtility .castState(exitState.getExpression().get(exp))
      val value = state._1.getSemanticDomain().asInstanceOf[SymbolicIntervals[T, IntervalsSymbolicValues]].eval(exp, new VariableIdentifier("result", exp.getType(), exp.getProgramPoint()));
      value.l match {
        case Bound(t) => c.addConstraint(new Geq[IntervalsSymbolicValues](t.toArithmeticExpression(), new Multiply[IntervalsSymbolicValues](new SimpleVal[IntervalsSymbolicValues](1), new IntervalsSymbolicValues(classe, methodName, TypeOfContracts.postcondition, new VariableIdentifier("result", exp.getType(), exp.getProgramPoint()), SymbolicContractTypes.min))))
        case _ => printer.add(new WarningMethod(classe, methodName, "Symbolic intervals have no information on the lower bound of the returned value at the end of the method. Therefore we cannot enforce any constraint on that."))
      }

      value.r match {
        case Bound(t) => c.addConstraint(new Geq[IntervalsSymbolicValues](new Multiply[IntervalsSymbolicValues](new SimpleVal[IntervalsSymbolicValues](1), new IntervalsSymbolicValues(classe, methodName, TypeOfContracts.postcondition, new VariableIdentifier("result", exp.getType(), exp.getProgramPoint()), SymbolicContractTypes.max)), t.toArithmeticExpression()))
        case _ => printer.add(new WarningMethod(classe, methodName, "Symbolic intervals have no information on the upper bound of the returned value at the end of the method. Therefore we cannot enforce any constraint on that."))
      }
    }
  }
}

class SymbolicIntervalsMethodCallVisitor[T <: SymbolicInt[T, IntervalsSymbolicValues]](val c : ConstraintsInference[IntervalsSymbolicValues, T]) extends Visitor {

  def getLabel() : String = "Contract Inference for Symbolic Intervals";

  def checkSingleStatement[S1 <: State[S1]](state : S1, statement : Statement, printer : OutputCollector) : Unit = statement match {
    case x : MethodCall =>
      val resultState = x.forwardSemantics(state);
      for(exp <- resultState.getExpression().getExpressions())
        exp match {
          case AbstractMethodCall(thisExpr, parameters, calledMethod, retType) =>
            val symbolicIntervalsState : SymbolicIntervals[T, IntervalsSymbolicValues] = SymbolicIntervalsUtility.castState(resultState.getExpression().get(exp))._1.getSemanticDomain().asInstanceOf[SymbolicIntervals[T, IntervalsSymbolicValues]];
            val (classe, renaming) = SymbolicSettings.rename(calledMethod, thisExpr, parameters, true)
            for(i <- 0 to parameters.size-1) {
              val (expr, nameInMethod) = renaming.apply(i);
              val value = symbolicIntervalsState.get(nameInMethod);
              value.l match {
                case Bound(t) => c.addConstraint(new Geq[IntervalsSymbolicValues](t.toArithmeticExpression(), SymbolicIntervalsUtility.toArithmeticExpression(expr, classe, calledMethod, SymbolicContractTypes.min, TypeOfContracts.precondition)))
                case _ => printer.add(new WarningProgramPoint(statement.getPC(), "Symbolic intervals have no information on the lower bound of variable "+nameInMethod+" that is passed as argument to "+calledMethod+". Therefore we cannot enforce any constraint on that."))
              }
              value.r match {
                case Bound(t) => c.addConstraint(new Geq[IntervalsSymbolicValues](SymbolicIntervalsUtility.toArithmeticExpression(expr, classe, calledMethod, SymbolicContractTypes.max, TypeOfContracts.precondition), t.toArithmeticExpression()))
                case _ => printer.add(new WarningProgramPoint(statement.getPC(), "Symbolic intervals have no information on the upper bound of variable "+nameInMethod+" that is passed as argument to "+calledMethod+". Therefore we cannot enforce any constraint on that."))
              }
            }
          case _ =>
        }
    case _ =>
  }

}

object SymbolicIntervalsUtility {

  def toArithmeticExpression(exp : Expression, className : Type, methodName : String, minmax : SymbolicContractTypes.Value, typ : TypeOfContracts.Value) : ArithmeticExpression[IntervalsSymbolicValues] = exp match {
    case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.+, returntyp) =>
      return new Add(new Multiply[IntervalsSymbolicValues](new SimpleVal[IntervalsSymbolicValues](1), new IntervalsSymbolicValues(className, methodName, typ, left, minmax)), new SimpleVal[IntervalsSymbolicValues](Integer.parseInt(right.constant)))
    case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.+, returntyp) =>
      return new Add(new Multiply[IntervalsSymbolicValues](new SimpleVal[IntervalsSymbolicValues](1), new IntervalsSymbolicValues(className, methodName, typ, left, minmax)), new SimpleVal[IntervalsSymbolicValues](0-Integer.parseInt(right.constant)))
    case id : Identifier =>
      return new Multiply[IntervalsSymbolicValues](new SimpleVal[IntervalsSymbolicValues](1), new IntervalsSymbolicValues(className, methodName, typ, id, minmax))
  }

  def castState[S1 <: State[S1], H <: HeapDomain[H, I], I <: HeapIdentifier[I], T <: SymbolicInt[T, IntervalsSymbolicValues]](state : S1) = state.asInstanceOf[GenericAbstractState[SymbolicIntervals[T, IntervalsSymbolicValues], H, I]];
}