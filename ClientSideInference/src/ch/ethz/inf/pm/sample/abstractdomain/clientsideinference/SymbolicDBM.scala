package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{Throw, Type}


object DBMSetting {
  private var idToIndex : Map[Identifier, Int] = Map.empty;
  private var nextFreeInt : Int = 0;

  def containID(id : Identifier) : Boolean = idToIndex.keySet.contains(id)

  def getIDS() = idToIndex.keySet

  def getIndex(id : Identifier) : Int =
    if(this.idToIndex.keySet.contains(id))
      this.idToIndex.apply(id);
    else {this.idToIndex=this.idToIndex+((id, nextFreeInt)); nextFreeInt=nextFreeInt+1; nextFreeInt-1;}
}

class SymbolicDBM[T <: SymbolicInt[T, S], S <: SymbolicValue[S]] extends SimplifiedSemanticDomain[SymbolicDBM[T, S]] {
  var matrix : Map[(Int, Int), T] = Map.empty;
  var isBottom : Boolean = false;

  private def this(m : Map[(Int, Int), T]) = {
    this();
    matrix=m;
  }
  private def this(b : Boolean) = {
    this();
    this.isBottom=b;
  }
  def removeVariable(variable: Identifier) : SymbolicDBM[T, S] = {
    if(isBottom) return this;
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

  def setToTop(variable: Identifier) =
    if(isBottom) this;
    else this.removeVariable(variable)

  def createVariable(variable: Identifier, typ: Type) = this;

  def assume(expr: Expression) = this;

  def assign(variable: Identifier, expr: Expression) =
    if(isBottom) this;
    else
      expr match {
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

  def top() : SymbolicDBM[T,S] = new SymbolicDBM[T, S]();

  override def factory() = new SymbolicDBM[T, S]();

  def lub(left: SymbolicDBM[T,S], right: SymbolicDBM[T,S]) : SymbolicDBM[T,S] = {
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    var newMatrix : Map[(Int, Int), T] = matrix;
    for(id <- left.matrix.keySet**(right.matrix.keySet))
      newMatrix=newMatrix+((id, left.matrix.apply(id).min(left.matrix.apply(id), right.matrix.apply(id))));
    return new SymbolicDBM(newMatrix);
  }

  def glb(left: SymbolicDBM[T,S], right: SymbolicDBM[T,S]) : SymbolicDBM[T,S] = {
    if(left.isBottom || right.isBottom) return this.bottom();
    var newMatrix : Map[(Int, Int), T] = matrix;
    for(id <- left.matrix.keySet**(right.matrix.keySet))
      newMatrix=newMatrix+((id, left.matrix.apply(id).max(left.matrix.apply(id), right.matrix.apply(id))));
    for(id <- left.matrix.keySet--(right.matrix.keySet))
      newMatrix=newMatrix+((id, left.matrix.apply(id)));
    for(id <- right.matrix.keySet--(left.matrix.keySet))
      newMatrix=newMatrix+((id, right.matrix.apply(id)));
    return new SymbolicDBM(newMatrix);
  }

  def widening(left: SymbolicDBM[T,S], right: SymbolicDBM[T,S]) : SymbolicDBM[T,S] = {
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    var newMatrix : Map[(Int, Int), T] = matrix;
    for(id <- left.matrix.keySet**(right.matrix.keySet)) {
      if(left.matrix.apply(id).equals(right.matrix.apply(id)))
        newMatrix=newMatrix+((id, left.matrix.apply(id)));
    }
    return new SymbolicDBM(newMatrix);
  }

  def lessEqual(r: SymbolicDBM[T,S]) : Boolean =  {
    if(this.isBottom) return true;
    if(r.isBottom) return false;
    var newMatrix : Map[(Int, Int), T] = matrix;
    for(id <- r.matrix.keySet)
      if(! this.matrix.keySet.contains(id) || ! this.matrix.apply(id).<=(this.matrix.apply(id), r.matrix.apply(id)))
        return false;
    return true;
  }

  def bottom() : SymbolicDBM[T,S] = new SymbolicDBM(true);

  def getIds() = DBMSetting.getIDS();

  def getStringOfId(id: Identifier) : String = throw new SymbolicDBMException("TODO")
  def merge(f: Replacement) : SymbolicDBM[T,S] = throw new SymbolicDBMException("Merge not yet supported")

}


class SymbolicDBMException(s : String) extends Exception(s)

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