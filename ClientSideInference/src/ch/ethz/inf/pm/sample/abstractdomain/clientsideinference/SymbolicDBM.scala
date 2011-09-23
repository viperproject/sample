package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, Type}
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph
import ch.ethz.inf.pm.sample.property.Property
import com.sun.org.omg.CORBA.IdentifierHelper

object DBMSetting {
  private var idToIndex : Map[Identifier, Int] = Map.empty;
  private var nextFreeInt : Int = 0;

  def containID(id : Identifier) : Boolean = idToIndex.keySet.contains(id)

  def getIDS() = idToIndex.keySet

  def getIndex(id : Identifier) : Int =
    if(this.idToIndex.keySet.contains(id))
      this.idToIndex.apply(id);
    else {this.idToIndex=this.idToIndex+((id, nextFreeInt)); nextFreeInt=nextFreeInt+1; nextFreeInt-1;}

  def getId(index : Int) : Identifier = {
    for(id <- idToIndex.keySet)
      if(idToIndex.apply(id)==index)
        return id;
    throw new SymbolicDBMException("Unknown index")
  }

  def symbolicInt[T <: SymbolicInt[T, S], S <: SymbolicValue[S]] : T =
    new LinearSum(
      new Summation(Map.empty[S, Coefficient[T, S]], new IntervalsSymbolicValues("a", SymbolicContractTypes.min).asInstanceOf[S]),
      0,
      new IntervalsSymbolicValues("a", SymbolicContractTypes.min).asInstanceOf[S]).asInstanceOf[T];

}

class SymbolicDBM[T <: SymbolicInt[T, S], S <: SymbolicValue[S]]() extends SimplifiedSemanticDomain[SymbolicDBM[T, S]] {
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

  def assume(expr: Expression) : SymbolicDBM[T, S] =
    if(isBottom) this;
    else
      expr match {
        case BinaryArithmeticExpression(
          BinaryArithmeticExpression(id1, id2, ArithmeticOperator.-, r),
          Constant(c, ty, pp),
          ArithmeticOperator.<=,
          returntyp) if id1.isInstanceOf[Identifier] && id2.isInstanceOf[Identifier] =>
            val index1 = DBMSetting.getIndex(id1.asInstanceOf[Identifier]);
            val index2 = DBMSetting.getIndex(id2.asInstanceOf[Identifier]);
            if(! matrix.keySet.contains((index1, index2)))
              return new SymbolicDBM(matrix.+(((index1, index2), DBMSetting.symbolicInt.factory(Integer.parseInt(c)))));
            else {
              val oldValue : T = matrix.apply((index1, index2));
              val newValue = oldValue.min(oldValue, DBMSetting.symbolicInt.factory(Integer.parseInt(c)));
              return new SymbolicDBM(matrix.+(((index1, index2), newValue)));
            }
        case _ => return this;
      }


  def assign(variable: Identifier, expr: Expression) : SymbolicDBM[T, S] =
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
        case AbstractMethodCall(thisExpr, parameters, calledMethod, retType) =>
          val assignedIndex = DBMSetting.getIndex(variable);
          var newMatrix = this.matrix;

          for(p <- parameters)
            p match {
              case x : Identifier =>
                val pIndex = DBMSetting.getIndex(x);
                //Rename is missing here!
                newMatrix = newMatrix +
                            (((assignedIndex, pIndex), DBMSetting.symbolicInt.factory(new DBMSymbolicValue(calledMethod, variable, p)))) +
                            (((pIndex, assignedIndex), DBMSetting.symbolicInt.factory(new DBMSymbolicValue(calledMethod, p, variable))))

            }
          return new SymbolicDBM(newMatrix)

  }

  def top() : SymbolicDBM[T,S] = new SymbolicDBM[T, S]();

  override def factory() = new SymbolicDBM[T, S]();

  def lub(left: SymbolicDBM[T,S], right: SymbolicDBM[T,S]) : SymbolicDBM[T,S] = {
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    var newMatrix : Map[(Int, Int), T] = Map.empty;
    for(id <- left.matrix.keySet**(right.matrix.keySet))
      newMatrix=newMatrix+((id, left.matrix.apply(id).max(left.matrix.apply(id), right.matrix.apply(id))));
    return new SymbolicDBM(newMatrix);
  }

  def glb(left: SymbolicDBM[T,S], right: SymbolicDBM[T,S]) : SymbolicDBM[T,S] = {
    if(left.isBottom || right.isBottom) return this.bottom();
    var newMatrix : Map[(Int, Int), T] = Map.empty;
    for(id <- left.matrix.keySet**(right.matrix.keySet))
      newMatrix=newMatrix+((id, left.matrix.apply(id).min(left.matrix.apply(id), right.matrix.apply(id))));
    for(id <- left.matrix.keySet--(right.matrix.keySet))
      newMatrix=newMatrix+((id, left.matrix.apply(id)));
    for(id <- right.matrix.keySet--(left.matrix.keySet))
      newMatrix=newMatrix+((id, right.matrix.apply(id)));
    return new SymbolicDBM(newMatrix);
  }

  def widening(left: SymbolicDBM[T,S], right: SymbolicDBM[T,S]) : SymbolicDBM[T,S] = {
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    var newMatrix : Map[(Int, Int), T] = Map.empty;
    for(id <- left.matrix.keySet**(right.matrix.keySet)) {
      if(left.matrix.apply(id).equals(right.matrix.apply(id)))
        newMatrix=newMatrix+((id, left.matrix.apply(id)));
    }
    return new SymbolicDBM(newMatrix);
  }

  def lessEqual(r: SymbolicDBM[T,S]) : Boolean =  {
    if(this.isBottom) return true;
    if(r.isBottom) return false;
    if(this.matrix.equals(r.matrix)) return true;
    var newMatrix : Map[(Int, Int), T] = matrix;
    for(id <- r.matrix.keySet)
      if(! this.matrix.keySet.contains(id) || ! this.matrix.apply(id).<=(this.matrix.apply(id), r.matrix.apply(id)))
        return false;
    return true;
  }

  def bottom() : SymbolicDBM[T,S] = new SymbolicDBM(true);

  def getIds() = DBMSetting.getIDS();

  def getStringOfId(id: Identifier) : String = {
    if(! DBMSetting.containID(id))
      return "T";
    var result : String = "";
    val index = DBMSetting.getIndex(id);
    for((id1, id2) <- this.matrix.keySet)
      if(id1==index || id2==index)
        result = result + constraintToString(id1, id2, this.matrix.apply((id1, id2)))+" ; ";
    if(result.equals("")) return "T";
    else return result;
  }

  override def toString() : String = {
    var result = "";
    for(key <- matrix.keySet)
      result = result + constraintToString (key._1, key._2, this.matrix.apply(key)) + " ;\n";
    return result;
  }

  private def constraintToString(v1 : Int, v2 : Int, v : T) : String = DBMSetting.getId(v1)+" - "+DBMSetting.getId(v2) + " < " + v.toString;

  def merge(f: Replacement) : SymbolicDBM[T,S] =
    if(f.isEmpty()) return this
    else throw new SymbolicDBMException("Merge not yet supported")

}


class SymbolicDBMException(s : String) extends Exception(s)

class DBMSymbolicValue(val method : String, val v1 : Identifier, val v2 : Identifier) extends SymbolicValue[DBMSymbolicValue] {
  override def equals(o : Any) = o match {
    case x : DBMSymbolicValue => method.equals(x.method) && v1.equals(x.v1) && v2.equals(x.v2);
    case _ => false;
  }

  def <=(a: DBMSymbolicValue, b : DBMSymbolicValue) : Boolean = false;

  override def toString() = "C( "+method+", "+v1+", "+v2+")"
}

class SymbolicDBMAnalysis[T <: SymbolicInt[T, S], S <: SymbolicValue[S]] extends SemanticAnalysis[SymbolicDBM[T, S]] {

  override def getNativeMethodsSemantics() : List[NativeMethodSemantics] = List(SymbolicNativeMethodSemantics)

  override def reset() = Unit;

  override def getLabel() = "Symbolic DBM inference"

  override def parameters() : List[(String, Any)] = Nil;

  override def setParameter(label : String, value : Any) : Unit = throw new SymbolicIntException("Paramters not supported")

  override def getInitialState() : SymbolicDBM[T, S] = new SymbolicDBM[T, S]();

  override def getProperties() : Set[Property] = Set(ShowGraph);
}