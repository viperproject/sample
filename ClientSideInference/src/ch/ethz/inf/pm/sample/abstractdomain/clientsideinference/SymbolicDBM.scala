package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property._

object DBMSetting {
  private var idToIndex : Map[Identifier, Int] = Map.empty;
  private var nextFreeInt : Int = 0;

  def reset() = {
    this.nextFreeInt=0;
    this.idToIndex=Map.empty;
  }

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
}

class SymbolicDBM[T <: SymbolicInt[T, S], S <: SymbolicValue[S]]() extends SimplifiedSemanticDomain[SymbolicDBM[T, S]] {
  var matrix : Map[(Int, Int), T] = Map.empty;
  var isBottom : Boolean = false;

  def get(v1 : Identifier, v2 : Identifier) : Option[T] = matrix.get((DBMSetting.getIndex(v1), DBMSetting.getIndex(v2)))

  override def createVariableForArgument(variable : Identifier, typ : Type, path : List[String]) : (SymbolicDBM[T, S], Map[Identifier, List[String]]) = {
    if(! typ.isNumericalType()) return super.createVariableForArgument(variable, typ, path);
    if(typ.isNumericalType() && ! path.isEmpty) throw new SymbolicDBMException("Not yet supported");
    var (state, d2) = super.createVariableForArgument(variable, typ, path);
    var resultMatrix = state.matrix;
    val varIndex = DBMSetting.getIndex(variable);
    //We suppose that the variables already created are all parameters of the current method
    for(id <- DBMSetting.getIDS()) {
      if(! id.equals(variable)) {
        val otherVarIndex = DBMSetting.getIndex(id);
        resultMatrix=resultMatrix+
            (((varIndex, otherVarIndex),
              SymbolicSettings.symbolicInt.factory(new DBMSymbolicValue(
                      SystemParameters.currentClass,
                      SystemParameters.currentMethod,
                      TypeOfContracts.precondition,
                      variable,
                      id
                      )
              )))+
            (((otherVarIndex, varIndex),
              SymbolicSettings.symbolicInt.factory(new DBMSymbolicValue(
                      SystemParameters.currentClass,
                      SystemParameters.currentMethod,
                      TypeOfContracts.precondition,
                      id,
                      variable
                      )
              )))
      }
    }
    (new SymbolicDBM(resultMatrix), d2)
  }

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
              return new SymbolicDBM(matrix.+(((index1, index2), SymbolicSettings.symbolicInt.factory(Integer.parseInt(c)))));
            else {
              val oldValue : T = matrix.apply((index1, index2));
              val newValue = oldValue.min(oldValue, SymbolicSettings.symbolicInt.factory(Integer.parseInt(c)));
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
          var newMatrix : Map[(Int, Int), T] = this.matrix;
          val (classe, renaming) = SymbolicSettings.rename(calledMethod, thisExpr, parameters, true)
          for(i <- 0 to parameters.size-1) {
            val (expr, nameInMethod) = renaming.apply(i);
            newMatrix=newMatrix++this.transformToSymbSum(classe, expr, variable, new VariableIdentifier("result", variable.getType(), variable.getProgramPoint()), calledMethod, nameInMethod, TypeOfContracts.postcondition);
          }
          return new SymbolicDBM(newMatrix)
        case _ => return this.removeVariable(variable);

  }

  private def transformToSymbSum(classe : Type, expr : Expression, variable : Identifier, otherVar : Identifier, calledMethod : String, nameInMethod : Identifier, typ : TypeOfContracts.Value) : Map[(Int, Int), T] = expr match {
    case p : Identifier =>
      val assignedIndex = DBMSetting.getIndex(variable);
      val pIndex = DBMSetting.getIndex(nameInMethod);
      return Map.empty+
        (((assignedIndex, pIndex), SymbolicSettings.symbolicInt.factory(new DBMSymbolicValue(classe, calledMethod, typ, otherVar, p)))) +
        (((pIndex, assignedIndex), SymbolicSettings.symbolicInt.factory(new DBMSymbolicValue(classe, calledMethod, typ, p, otherVar))))
    //Check that
    case BinaryArithmeticExpression(p : Identifier, right : Constant, ArithmeticOperator.-, returntyp) =>
      val assignedIndex = DBMSetting.getIndex(variable);
      val pIndex = DBMSetting.getIndex(nameInMethod);
      var symbInt1 : T = SymbolicSettings.symbolicInt.factory(new DBMSymbolicValue(classe, calledMethod, typ, otherVar, p));
      symbInt1=symbInt1.+(symbInt1, SymbolicSettings.symbolicInt.factory(Integer.parseInt(right.constant)));
      var symbInt2 : T = SymbolicSettings.symbolicInt.factory(new DBMSymbolicValue(classe, calledMethod, typ, p, otherVar));
      symbInt2=symbInt2.-(symbInt2, SymbolicSettings.symbolicInt.factory(Integer.parseInt(right.constant)));
      return Map.empty+(((assignedIndex, pIndex), symbInt1))+(((pIndex, assignedIndex), symbInt2));
    case BinaryArithmeticExpression(p : Identifier, right : Constant, ArithmeticOperator.+, returntyp) =>
      val assignedIndex = DBMSetting.getIndex(variable);
      val pIndex = DBMSetting.getIndex(nameInMethod);
      var symbInt1 : T = SymbolicSettings.symbolicInt.factory(new DBMSymbolicValue(classe, calledMethod, typ, otherVar, p));
      symbInt1=symbInt1.-(symbInt1, SymbolicSettings.symbolicInt.factory(Integer.parseInt(right.constant)));
      var symbInt2 : T = SymbolicSettings.symbolicInt.factory(new DBMSymbolicValue(classe, calledMethod, typ, p, otherVar));
      symbInt2=symbInt2.+(symbInt2, SymbolicSettings.symbolicInt.factory(Integer.parseInt(right.constant)));
      return Map.empty+(((assignedIndex, pIndex), symbInt1))+(((pIndex, assignedIndex), symbInt2));
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

class DBMSymbolicValue(val classe : Type, val method : String, var typ : TypeOfContracts.Value, val v1 : Identifier, val v2 : Identifier) extends SymbolicValue[DBMSymbolicValue] {

  def <=(a: DBMSymbolicValue, b : DBMSymbolicValue) : Boolean = false;

  override def toString() = typ+"( "+classe+", "+method+", "+v1+", "+v2+")"

  override def hashCode() : Int = v1.hashCode();

  override def equals(o : Any) = o match {
    case x : DBMSymbolicValue =>
      this.v1.equals(x.v1) && this.v2.equals(x.v2) && this.method.equals(x.method) && this.classe.equals(x.classe) && this.typ.equals(x.typ)
    case _ => false
  }
}

class SymbolicDBMAnalysis[T <: SymbolicInt[T, DBMSymbolicValue]] extends SemanticAnalysis[SymbolicDBM[T, DBMSymbolicValue]] {

  override def getNativeMethodsSemantics() : List[NativeMethodSemantics] = List(SymbolicNativeMethodSemantics)

  override def reset() = DBMSetting.reset();

  override def getLabel() = "Symbolic DBM inference"

  override def parameters() : List[(String, Any)] = Nil;

  override def setParameter(label : String, value : Any) : Unit = throw new SymbolicIntException("Paramters not supported")

  override def getInitialState() : SymbolicDBM[T, DBMSymbolicValue] = new SymbolicDBM[T, DBMSymbolicValue]();

  override def getProperties() : Set[Property] = Set(ShowGraph, new SymbolicDBMProperty(new ConstraintsInference[DBMSymbolicValue, T]()));
}

class SymbolicDBMException(s : String) extends Exception(s)


class SymbolicDBMProperty[T <: SymbolicInt[T, DBMSymbolicValue]](val c : ConstraintsInference[DBMSymbolicValue, T]) extends Property {
  val v = new SymbolicDBMMethodCallVisitor[T](c);
  val property1 = new SingleStatementProperty(v)

  def getLabel() : String = "Contract Inference for Symbolic DBMs";

  def finalizeChecking(printer : OutputCollector) : Unit = c.printConstraints()

  def check[S <: State[S]](classe : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) : Unit = {
    property1.check(classe, methodName, result, printer);
    def exitState = result.exitState();
    for(exp <- exitState.getExpression().getExpressions()) {
      val state = SymbolicDBMUtility.castState(exitState.getExpression().get(exp))
      //val value = state._1.getSemanticDomain().asInstanceOf[SymbolicDBM[T, DBMSymbolicValue]].eval(exp, new VariableIdentifier("result", exp.getType(), exp.getProgramPoint()));
      /*value.l match {
        case Bound(t) => c.addConstraint(new Geq[DBMSymbolicValue](t.toArithmeticExpression(), new Multiply[DBMSymbolicValue](new SimpleVal[DBMSymbolicValue](1), new IntervalsSymbolicValues(classe, methodName, TypeOfContracts.postcondition, new VariableIdentifier("result", exp.getType(), exp.getProgramPoint()), SymbolicContractTypes.min))))
        case _ => printer.add(new WarningMethod(classe, methodName, "Symbolic intervals have no information on the lower bound of the returned value at the end of the method. Therefore we cannot enforce any constraint on that."))
      }

      value.r match {
        case Bound(t) => c.addConstraint(new Geq[DBMSymbolicValue](new Multiply[DBMSymbolicValue](new SimpleVal[DBMSymbolicValue](1), new IntervalsSymbolicValues(classe, methodName, TypeOfContracts.postcondition, new VariableIdentifier("result", exp.getType(), exp.getProgramPoint()), SymbolicContractTypes.max)), t.toArithmeticExpression()))
        case _ => printer.add(new WarningMethod(classe, methodName, "Symbolic intervals have no information on the upper bound of the returned value at the end of the method. Therefore we cannot enforce any constraint on that."))
      } */
    }
  }
}

class SymbolicDBMMethodCallVisitor[T <: SymbolicInt[T, DBMSymbolicValue]](val c : ConstraintsInference[DBMSymbolicValue, T]) extends Visitor {

  def getLabel() : String = "Contract Inference for Symbolic DBMs";

  def checkSingleStatement[S1 <: State[S1]](state : S1, statement : Statement, printer : OutputCollector) : Unit = statement match {
    case x : MethodCall =>
      val resultState = x.forwardSemantics(state);
      for(exp <- resultState.getExpression().getExpressions())
        exp match {
          case AbstractMethodCall(thisExpr, parameters, calledMethod, retType) =>
            val symbolicDBMState : SymbolicDBM[T, DBMSymbolicValue] = SymbolicDBMUtility.castState(resultState.getExpression().get(exp))._1.getSemanticDomain().asInstanceOf[SymbolicDBM[T, DBMSymbolicValue]];
            //val (classeswapped, renamingswapped) = SymbolicSettings.rename(calledMethod, thisExpr, parameters, true)
            val (classe, renaming) = SymbolicSettings.rename(calledMethod, thisExpr, parameters, false)
            for(i <- 0 to parameters.size-1) {
              val (v1expr, v1NameInMethod) = renaming.apply(i);
              for(j <- 0 to parameters.size-1)
                if(j!=i) {
                  val (v2expr, v2NameInMethod) = renaming.apply(j);
                  symbolicDBMState.get(v1NameInMethod, v2NameInMethod) match {
                    case Some(intsum) =>
                      var overallSum = intsum;
                      overallSum=overallSum.+(overallSum, overallSum.factory(SymbolicDBMUtility.extractInt(v1expr, true)));
                      overallSum=overallSum.+(overallSum, overallSum.factory(SymbolicDBMUtility.extractInt(v2expr, false)));
                      c.addConstraint(new Geq[DBMSymbolicValue](overallSum.factory(new DBMSymbolicValue(classe, calledMethod, TypeOfContracts.precondition, SymbolicDBMUtility.extractId(v1expr), SymbolicDBMUtility.extractId(v2expr))).toArithmeticExpression(), overallSum.toArithmeticExpression()))
                    case None => printer.add(new WarningProgramPoint(statement.getPC(), "Symbolic DBM have no information on "+v1NameInMethod+"-"+v2NameInMethod+"<k that are passed as arguments to "+calledMethod+". Therefore we cannot enforce any constraint on that."))
                  }
                  symbolicDBMState.get(v2NameInMethod, v1NameInMethod) match {
                    case Some(intsum) =>
                      var overallSum = intsum;
                      overallSum=overallSum.+(overallSum, overallSum.factory(SymbolicDBMUtility.extractInt(v2expr, true)));
                      overallSum=overallSum.+(overallSum, overallSum.factory(SymbolicDBMUtility.extractInt(v1expr, false)));
                      c.addConstraint(new Geq[DBMSymbolicValue](overallSum.factory(new DBMSymbolicValue(classe, calledMethod, TypeOfContracts.precondition, SymbolicDBMUtility.extractId(v2expr), SymbolicDBMUtility.extractId(v1expr))).toArithmeticExpression(), overallSum.toArithmeticExpression()))
                    case None => printer.add(new WarningProgramPoint(statement.getPC(), "Symbolic DBM have no information on "+v2NameInMethod+"-"+v1NameInMethod+"<k that are passed as arguments to "+calledMethod+". Therefore we cannot enforce any constraint on that."))
                  }
                }
              //val value = symbolicDBMState.get(nameInMethod);
              /*value.l match {
                case Bound(t) => c.addConstraint(new Geq[IntervalsSymbolicValues](t.toArithmeticExpression(), SymbolicIntervalsUtility.toArithmeticExpression(expr, classe, calledMethod, SymbolicContractTypes.min, TypeOfContracts.precondition)))
                case _ => printer.add(new WarningProgramPoint(statement.getPC(), "Symbolic intervals have no information on the lower bound of variable "+nameInMethod+" that is passed as argument to "+calledMethod+". Therefore we cannot enforce any constraint on that."))
              }
              value.r match {
                case Bound(t) => c.addConstraint(new Geq[IntervalsSymbolicValues](SymbolicIntervalsUtility.toArithmeticExpression(expr, classe, calledMethod, SymbolicContractTypes.max, TypeOfContracts.precondition), t.toArithmeticExpression()))
                case _ => printer.add(new WarningProgramPoint(statement.getPC(), "Symbolic intervals have no information on the upper bound of variable "+nameInMethod+" that is passed as argument to "+calledMethod+". Therefore we cannot enforce any constraint on that."))
              }*/
            }
          case _ =>
        }
    case _ =>
  }

}

object SymbolicDBMUtility {

  def extractInt(exp : Expression, swap : Boolean) : Int = exp match {
    case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.+, returntyp) =>
      return if(swap) 0-Integer.parseInt(right.constant) else Integer.parseInt(right.constant)
    case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.-, returntyp) =>
      return if(swap) Integer.parseInt(right.constant) else 0-Integer.parseInt(right.constant)
    case id : Identifier =>
      return 0
  }

  def extractId(exp : Expression) : Identifier = exp match {
    case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.+, returntyp) =>
      return left;
    case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.-, returntyp) =>
      return left;
    case id : Identifier =>
      return id
  }

  /*def toArithmeticExpression(exp : Expression, className : Type, methodName : String, minmax : SymbolicContractTypes.Value, typ : TypeOfContracts.Value) : ArithmeticExpression[DBMSymbolicValue] = exp match {
    case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.+, returntyp) =>
      return new Add(new Multiply[DBMSymbolicValue](new SimpleVal[DBMSymbolicValue](1), new IntervalsSymbolicValues(className, methodName, typ, left, minmax)), new SimpleVal[DBMSymbolicValue](Integer.parseInt(right.constant)))
    case BinaryArithmeticExpression(left : Identifier, right : Constant, ArithmeticOperator.+, returntyp) =>
      return new Add(new Multiply[DBMSymbolicValue](new SimpleVal[DBMSymbolicValue](1), new IntervalsSymbolicValues(className, methodName, typ, left, minmax)), new SimpleVal[DBMSymbolicValue](0-Integer.parseInt(right.constant)))
    case id : Identifier =>
      return new Multiply[DBMSymbolicValue](new SimpleVal[IntervalsSymbolicValues](1), new IntervalsSymbolicValues(className, methodName, typ, id, minmax))
  }*/

  def castState[S1 <: State[S1], H <: HeapDomain[H, I], I <: HeapIdentifier[I], T <: SymbolicInt[T, DBMSymbolicValue]](state : S1) = state.asInstanceOf[GenericAbstractState[SymbolicDBM[T, DBMSymbolicValue], H, I]];
}