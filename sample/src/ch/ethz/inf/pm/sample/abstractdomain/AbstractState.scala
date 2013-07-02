package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._
import util.HeapIdSetFunctionalLifting

object ExpressionFactory {

  def createVariable(variable : Variable, ty : Type, pp : ProgramPoint): ExpressionSet= {
    var result = new ExpressionSet(ty)
    result=result.add(new VariableIdentifier(variable.getName(), ty, pp, variable.id.scope))
    result
  }

  def createBinaryExpression(left : ExpressionSet, right : ExpressionSet, op : ArithmeticOperator.Value, ty : Type): ExpressionSet= {
    var result = new ExpressionSet(ty)
    for(expleft <- left.getSetOfExpressions)
      for(expright <- right.getSetOfExpressions)
        result=result.add(new BinaryArithmeticExpression(expleft, expright, op, ty))
    result
  }

  def createReferenceComparisonExpression(left : ExpressionSet, right : ExpressionSet, op : ArithmeticOperator.Value, ty : Type): ExpressionSet= {
    var result = new ExpressionSet(ty)
    for(expleft <- left.getSetOfExpressions)
      for(expright <- right.getSetOfExpressions)
        result=result.add(new ReferenceComparisonExpression(expleft, expright, op, ty))
    result
  }

  def createBooleanBinaryExpression(left : ExpressionSet, right : ExpressionSet, op : BooleanOperator.Value, ty : Type): ExpressionSet= {
    var result = new ExpressionSet(ty)
    for(expleft <- left.getSetOfExpressions)
      for(expright <- right.getSetOfExpressions)
        result=result.add(new BinaryBooleanExpression(expleft, expright, op, ty))
    result
  }

  def createNondeterministicBinaryExpression(left : ExpressionSet, right : ExpressionSet, op : NondeterministicOperator.Value, ty : Type): ExpressionSet= {
    var result = new ExpressionSet(ty)
    for(expleft <- left.getSetOfExpressions)
      for(expright <- right.getSetOfExpressions)
        result=result.add(new BinaryNondeterministicExpression(expleft, expright, op, ty))
    result
  }

  def createUnaryExpression(v : ExpressionSet, op : ArithmeticOperator.Value, ty : Type): ExpressionSet= {
    var result = new ExpressionSet(ty)
    for(expleft <- v.getSetOfExpressions)
      result=result.add(new UnaryArithmeticExpression(expleft, op, ty))
    result
  }

  def createNegatedBooleanExpression(v : ExpressionSet): ExpressionSet= {
    var result = new ExpressionSet(v.getType())
    for(expleft <- v.getSetOfExpressions)
      result=result.add(new NegatedBooleanExpression(expleft))
    result
  }

  def createAbstractOperator(thisExpr : ExpressionSet, parameters : List[ExpressionSet], typeParameters : List[Type], op : AbstractOperatorIdentifiers.Value, ty : Type) : ExpressionSet = {
    var result = new ExpressionSet(ty)
    val combination = combineListValue(parameters)
    for(thisexp <- thisExpr.getSetOfExpressions)
      result=result.add(new AbstractOperator(thisexp, combination, typeParameters, op, ty))
    result
  }

  private def combineListValue(list : List[ExpressionSet]) : Set[List[Expression]] = list match {
    case Nil => Set.empty+(Nil)
    case x :: xs =>
      val previous : Set[List[Expression]] = combineListValue(xs)
      var result : Set[List[Expression]] = Set.empty
      for(expr <- x.getSetOfExpressions)
        for(l <- previous)
          result = result + (expr :: l)
      result
  }

}

class ExpressionSet(typ : Type) extends CartesianProductDomain[Type, SetOfExpressions, ExpressionSet](typ, new SetOfExpressions()) {

  def getType() : Type = this._1.glb(this._1, this.computeType())

  def getSetOfExpressions = this._2.value

  private def this(typ : Type, s : SetOfExpressions) = {
    this(typ)
    this.d2=s
  }

  def isTop = this._2.isTop

  def isBottom = this._2.isBottom

  private def computeType() : Type = {
    if(this._2.isTop) return SystemParameters.typ.top()
    var typ : Type = null
    for(t <- this.getSetOfExpressions)
      typ=if(typ==null) t.getType() else typ.lub(t.getType(), typ)
    if(typ==null) SystemParameters.typ.top()
    else typ
  }

  def add(exp : Expression) : ExpressionSet = {
    val v2 :SetOfExpressions = this._2.add(exp)
    new ExpressionSet(typ, v2)
  }

  def add(expr : ExpressionSet) : ExpressionSet = {
    var set = this._2
    for (exp <- expr.getSetOfExpressions) set = set.add(exp)
    val typ = this._1.glb(this._1,expr.getType())
    new ExpressionSet(typ,set)
  }

  def not() : ExpressionSet = {
    var result : SetOfExpressions = this._2.factory()
    for(key <- getSetOfExpressions)
      result=result.add(new NegatedBooleanExpression(key))
    new ExpressionSet(getType(), result)
  }

  def factory() : ExpressionSet =
    new ExpressionSet(
      if(getType()==null) {if(SystemParameters.typ!=null) SystemParameters.typ.top(); else null} else getType().top(),
      new SetOfExpressions()
    )

  override def toString:String = "Type "+d1.toString()+": "+d2.toString()

  def merge(r:Replacement) : ExpressionSet = {

    if (r.isEmpty()) return this

    val newSet = new SetOfExpressions()
    newSet.value = (for ((froms,tos) <- r.value; from <- froms; to <- tos) yield {
      this._2.value.map( _.replace(from,to) )
    }).flatten.toSet

    new ExpressionSet(typ,newSet)

  }

}

class SetOfExpressions extends SetDomain[Expression, SetOfExpressions] {
  def factory() : SetOfExpressions = new SetOfExpressions()
  override def bottom() = super.bottom()
}


class AbstractState[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](state : HeapAndAnotherDomain[N, H, I], expr : ExpressionSet)
  extends CartesianProductDomain[HeapAndAnotherDomain[N, H, I], ExpressionSet, AbstractState[N,H,I]](state, expr)
  with State[AbstractState[N,H,I]]
  with SingleLineRepresentation
  with LatticeWithReplacement[AbstractState[N,H,I]]
  {

  def factory() = new AbstractState(this._1.factory(), this._2.factory())
  override def bottom() = new AbstractState(this._1.bottom(), this._2.bottom())
  def isBottom : Boolean = this._1.lessEqual(this._1.bottom()) || this._2.lessEqual(this._2.bottom())
  def getStringOfId(id : Identifier) : String = this._1.getStringOfId(id)
  def getState = this._1

  def getSemanticDomain = this._1._1
  def getHeapDomain = this._1._2

  def before(pp : ProgramPoint) = this

  def createArray(length : ExpressionSet, typ : Type, pp : ProgramPoint) : AbstractState[N,H,I] =  {
    if(this.isBottom) return this
    var result = this.bottom().d1
    var heapId : HeapIdSetDomain[I] = null

    for(exp <- length.getSetOfExpressions) {
      var (createdLocation, newHeap, rep)=this._1._2.createArray(exp, typ, pp, this._1._1)
      result=result.lub(result, new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap))
      heapId = heapId match {
        case null => createdLocation
        case _ => heapId.lub(heapId, createdLocation)
      }
    }
    if(heapId == null) this.bottom()
    else this.setExpression(new ExpressionSet(typ).add(heapId)).setState(result)
  }

  def getArrayLength(array : ExpressionSet) : AbstractState[N,H,I] =  {
    if(this.isBottom) return this
    var result = this.bottom().d1
    var heapId : HeapIdSetDomain[I] = null

    for(exp <- array.getSetOfExpressions) {
      exp match {
        case id : Assignable =>
          var (createdLocation, newHeap, rep)=this._1._2.getArrayLength(id)
          result=result.lub(result, new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap))
          heapId = heapId match {
            case null => createdLocation
            case _ => heapId.lub(heapId, createdLocation)
          }
        case ids : HeapIdSetDomain[I] =>
          var (createdLocation, newHeap, rep)=HeapIdSetFunctionalLifting.applyGetFieldId(ids, this._1, this._1._2.getArrayLength(_))
          result=result.lub(result, new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap))
          heapId = heapId match {
            case null => createdLocation
            case _ => heapId.lub(heapId, createdLocation)
          }
        case _ => throw new SymbolicSemanticException("Not allowed")
      }
    }
    if(heapId == null) this.bottom()
    else this.setExpression(new ExpressionSet(SystemParameters.getType().top()).add(heapId)).setState(result)
  }

  def createObject(typ : Type, pp : ProgramPoint, fields : Option[Set[Identifier]] = None) : AbstractState[N,H,I] =  {

    if(this.isBottom) return this

    // It discharges on the heap analysis the creation of the object and its fields
    val (createdLocation, newHeap, rep) = this._1._2.createObject(typ, pp)
    var result = new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap)

    // Create all variables involved representing the object
    result=HeapIdSetFunctionalLifting.applyToSetHeapId(result, createdLocation, result.createVariable(_, typ))
    var result2 = result
    for(field <- fields.orElse(Some(typ.getPossibleFields())).get) {
      val (ids, state, rep2) = HeapIdSetFunctionalLifting.applyGetFieldId(createdLocation, result2, result2._2.getFieldIdentifier(_, field.getName(), field.getType(), field.getProgramPoint()))
      result2=HeapIdSetFunctionalLifting.applyToSetHeapId(result2, ids, new HeapAndAnotherDomain[N, H, I](result2._1.merge(rep2), state).createVariable(_, field.getType()))
    }

    this.setExpression(new ExpressionSet(typ).add(createdLocation)).setState(result2)

  }

  def getExpression() : ExpressionSet = getResult()

  def removeExpression() : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    new AbstractState(this._1, new ExpressionSet(SystemParameters.typ.top()))
  }

  def createVariable(x : ExpressionSet, typ : Type, pp : ProgramPoint) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    if(x.getSetOfExpressions.size != 1 || x.getSetOfExpressions.head.isInstanceOf[VariableIdentifier]==false)
      throw new SymbolicSemanticException("Cannot declare multiple variables together")
    var result=this.bottom()
    for(el <- x.getSetOfExpressions) {
      //For each variable that is potentially created, it computes its semantics and it considers the upper bound
      el match {
        case variable : Assignable => {
          for(assigned <- x.getSetOfExpressions) {
            val done=new AbstractState[N,H,I](this._1.createVariable(variable, typ), this._2)
            result=result.lub(result, done)
            result=result.setExpression(new ExpressionSet(typ).add(new UnitExpression(variable.getType().top(), pp)))
          }
        }
        case _ => throw new SymbolicSemanticException("I can assign only variables")
      }
    }
    result
  }

  override def createVariableForArgument(x : ExpressionSet, typ : Type) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    if(x.getSetOfExpressions.size != 1 || x.getSetOfExpressions.head.isInstanceOf[VariableIdentifier]==false)
      throw new SymbolicSemanticException("Cannot declare multiple variables together")
    var result=this.bottom()
    for(el <- x.getSetOfExpressions) {
      //For each variable that is potentially a parameter, it computes its semantics and it considers the upper bound
      el match {
        case variable : Assignable => {
          for(assigned <- x.getSetOfExpressions) {
            val r = this._1.createVariableForArgument(variable, typ, Nil)
            val left = r._1
            val done=new AbstractState[N,H,I](left, this._2)
            result=result.lub(result, done)
            result=result.setExpression(new ExpressionSet(typ).add(new UnitExpression(variable.getType().top(), variable.getProgramPoint)))
          }
        }
        case _ => throw new SymbolicSemanticException("I can assign only variables")
      }
    }
    result
  }

  def assignVariable(x : ExpressionSet, right : ExpressionSet) : AbstractState[N,H,I] = {
    if(this.isBottom) return bottom()
    if(right.isTop)
      return this.setVariableToTop(x).removeExpression()
    if(right.isTop) return top()
    var result : AbstractState[N,H,I] = this.bottom()
    for(el <- x.getSetOfExpressions) {
      //For each variable that is potentially assigned, it computes its semantics and it considers the upper bound
      el match {
        case variable : Assignable => {
          for(assigned <- right.getSetOfExpressions) {
            val done=new AbstractState[N,H,I](this._1.assign(variable, assigned), this._2)
            result=result.lub(result, done)
            result=result.setExpression(new ExpressionSet(variable.getType().top()).add(new UnitExpression(variable.getType().top(), variable.getProgramPoint)))
          }
        }
        case ids : HeapIdSetDomain[I]=> {
          for(assigned <- right.getSetOfExpressions) {
            val done=new AbstractState[N,H,I](HeapIdSetFunctionalLifting.applyToSetHeapId(this._1, ids, this._1.assign(_, assigned)), this._2)
            result=result.lub(result, done)
            result=result.setExpression(new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(ids.getType().top(), ids.getProgramPoint)))
          }
        }
        case _ => throw new SymbolicSemanticException("I can assign only variables here")
      }
    }
    result
  }

  def assignField(x : List[ExpressionSet], field : String, right : ExpressionSet) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    var result : Option[AbstractState[N,H,I]] = None
    if(right.isTop) {
      var t : AbstractState[N,H,I] = this.getFieldValue(x, field, right.getType())
      return t.setVariableToTop(t.getExpression).removeExpression()
    }
    for(obj <- x) {
      for(el <- obj.getSetOfExpressions) {
        //For each variable that is potentially assigned, it computes its semantics and it considers the upper bound
        el match {
          case variable : Identifier => {
            for(assigned <- right.getSetOfExpressions) {
              val done=new AbstractState[N,H,I](this._1.assignField(variable, field, assigned, right.getType(), variable.getProgramPoint() ), this._2)
              if(result==None)
                result=Some(done)
              else result=Some(done.lub(result.get, done))
              //initial=initial.setExpression(new ExpressionSet(new UnitExpression(variable.getType().bottom(), variable.getProgramPoint), this.removeExpression()))
            }
          }
          case heapid : HeapIdSetDomain[I] => {
            for(assigned <- right.getSetOfExpressions) {
              val done=new AbstractState[N,H,I](HeapIdSetFunctionalLifting.applyToSetHeapId(this._1, heapid, this._1.assignField(_, field, assigned, right.getType(), heapid.getProgramPoint() )), this._2)
              if(result==None)
                result=Some(done)
              else result=Some(done.lub(result.get, done))
            }
          }

          case _ => throw new SymbolicSemanticException("I can assign only variables and heap ids here")
        }
      }
    }
    if(result==None)
      throw new SymbolicSemanticException(("You should assign something to something"))
    result.get.removeExpression()
  }

  def backwardAssignVariable(x : ExpressionSet, right : ExpressionSet) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    if(right.isTop) return top()
    var result : AbstractState[N,H,I] = this.bottom()
    for(el <- x.getSetOfExpressions) {
      //For each variable that is potentially assigned, it computes its semantics and it considers the upper bound
      el match {
        case variable : Assignable => {
          for(assigned <- right.getSetOfExpressions) {
            val done=new AbstractState[N,H,I](this._1.backwardAssign(variable, assigned), this._2)
            result=result.lub(result, done)
            result=result.setExpression(new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(variable.getType().top(), variable.getProgramPoint)))
          }
        }
        case _ => throw new SymbolicSemanticException("I can assign only variables")
      }
    }
    result
  }

  override def setArgument(x : ExpressionSet, right : ExpressionSet) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    if(right.isTop) return top()
    var result : AbstractState[N,H,I] = this.bottom()
    for(el <- x.getSetOfExpressions) {

      //For each parameter that is set, it computes its semantics and it considers the upper bound
      el match {
        case variable : Assignable => {
          for(assigned <- right.getSetOfExpressions) {
            val done=new AbstractState[N,H,I](this._1.setArgument(variable, assigned), this._2)
            result=result.lub(result, done)
            result=result.setExpression(new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(variable.getType().top(), variable.getProgramPoint)))
          }
        }
        case _ => throw new SymbolicSemanticException("I can assign only variables")
      }
    }
    result
  }

  def removeVariable(x : ExpressionSet) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    var result : AbstractState[N,H,I] = this.bottom()
    for(el <- x.getSetOfExpressions) {
      //For each variable that is potentially removed, it computes its semantics and it considers the upper bound
      el match {
        case variable : Assignable => {
          for(previousState <- x.getSetOfExpressions) {
            val done=new AbstractState[N,H,I](this._1.removeVariable(variable), new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(variable.getType().top(), variable.getProgramPoint)))
            result=result.lub(result, done)
            result=result.setExpression(new ExpressionSet(SystemParameters.typ.top()).add(new UnitExpression(variable.getType().top(), variable.getProgramPoint)))
          }
        }
        case _ => throw new SymbolicSemanticException("I can remove only variables")
      }
    }
    result
  }

  def throws(throwed : ExpressionSet) : AbstractState[N,H,I] = this.bottom() //TODO: Support exceptions

  def evalConstant(value : String, typ : Type, pp : ProgramPoint) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    this.setExpression(new ExpressionSet(typ).add(new Constant(value, typ, pp)))
  }

  def getVariableValue(id : Assignable) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    val state = new AbstractState(this._1.access(id), this.removeExpression().getExpression())
    new AbstractState(state._1, new ExpressionSet(id.getType()).add(id.asInstanceOf[Expression]))
  }

  def backwardGetVariableValue(id : Assignable) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    val state = new AbstractState(this._1.backwardAccess(id), this.removeExpression().getExpression())
    new AbstractState(state._1, new ExpressionSet(id.getType()).add(id.asInstanceOf[Expression]))
  }

  def getType(variable : Identifier) : Type = {//TODO: is this correct???
    variable.getType()
  }

  def getFieldValue(objs: List[ExpressionSet], field: String, typ: Type): AbstractState[N, H, I] = {
    if (this.isBottom) return this
    var result: AbstractState[N, H, I] = this.bottom()
    for (obj: ExpressionSet <- objs) {
      //For each object that is potentially accessed, it computes the semantics of the field access and it considers the upper bound
      for (expr <- obj.getSetOfExpressions) {
        if (expr.isInstanceOf[Assignable] || expr.isInstanceOf[HeapIdSetDomain[I]]) {
          val (heapid, newHeap, rep) =
            if (expr.isInstanceOf[Assignable])
              this._1._2.getFieldIdentifier(expr.asInstanceOf[Assignable], field, typ, expr.getProgramPoint())
            else HeapIdSetFunctionalLifting.applyGetFieldId(expr.asInstanceOf[HeapIdSetDomain[I]], this._1,
              this._1._2.getFieldIdentifier(_, field, typ, expr.getProgramPoint()))

          var result2 = new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap)
          val accessed = if (heapid.isTop) result2.top() else HeapIdSetFunctionalLifting.applyToSetHeapId(result2, heapid, result2.access(_))
          val state = new AbstractState(accessed, new ExpressionSet(typ).add(heapid))
          result = result.lub(result, state)
        }
      }
    }
    result
  }

  def getArrayCell(obj : ExpressionSet, index : ExpressionSet, typ : Type) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    var result : AbstractState[N,H,I] = this.bottom()
    //For each object that is potentially accessed, it computes the semantics of the field access and it considers the upper bound
    for(expr <- obj.getSetOfExpressions) {
      if(! expr.isInstanceOf[Assignable]) throw new SymbolicSemanticException("Only assignable objects should be here")
      for(indexexpr <- index.getSetOfExpressions) {
        val (heapid, newHeap, rep) = this._1._2.getArrayCell(expr.asInstanceOf[Assignable], indexexpr, this._1._1, typ)
        var result2=new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap)
        val accessed=HeapIdSetFunctionalLifting.applyToSetHeapId(result2, heapid, result2.access(_))
        val state=new AbstractState(accessed, new ExpressionSet(typ).add(heapid))
        result=result.lub(result, state)
      }
    }
    result
  }

  def assignArrayCell(obj : ExpressionSet, index : ExpressionSet, right : ExpressionSet, typ : Type) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    var result : Option[AbstractState[N,H,I]] = None
    if(right.isTop) {
      var t : AbstractState[N,H,I] = this.getArrayCell(obj, index, right.getType())
      return t.setVariableToTop(t.getExpression).removeExpression()
    }
    for(el <- obj.getSetOfExpressions) {
      //For each variable that is potentially assigned, it computes its semantics and it considers the upper bound
      el match {
        case variable : Assignable => {
          for(indexexpr <- index.getSetOfExpressions) {
            for(assigned <- right.getSetOfExpressions) {
              val done=new AbstractState[N,H,I](this._1.assignArrayCell(variable, indexexpr, assigned, right.getType()), this._2)
              if(result==None)
                result=Some(done)
              else result=Some(done.lub(result.get, done))
            }
          }
        }
        case _ => throw new SymbolicSemanticException("I can assign only variables and heap ids here")
      }
    }
    if(result==None)
      throw new SymbolicSemanticException(("You should assign something to something"))
    result.get.removeExpression()
  }

  def backwardGetFieldValue(objs : List[ExpressionSet], field : String, typ : Type) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    var result : AbstractState[N,H,I] = this.bottom()
    for(obj : ExpressionSet <- objs) {
      //For each object that is potentially accessed, it computes the backward semantics of the field access and it considers the upper bound
      for(expr <- obj.getSetOfExpressions) {
        if(! expr.isInstanceOf[Assignable]) throw new SymbolicSemanticException("Only assignable objects should be here")
        val (heapid, newHeap, rep) = this._1._2.getFieldIdentifier(expr.asInstanceOf[Assignable], field, typ, expr.getProgramPoint())
        val result2=new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap)
        val accessed = HeapIdSetFunctionalLifting.applyToSetHeapId(result2, heapid, result2.backwardAccess(_))
        val state=new AbstractState(accessed, new ExpressionSet(typ).add(heapid))
        result=result.lub(result, state)
      }
    }
    result
  }

  def setVariableToTop(x : ExpressionSet) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    var result : AbstractState[N,H,I] = this.bottom()
    for(expr <- x.getSetOfExpressions) {
      //For each variable that is forgotten, it computes the semantics and it considers the upper bound
      if(expr.isInstanceOf[Assignable]) {
        val variable : Assignable = expr.asInstanceOf[Assignable]
        result=result.lub(result, new AbstractState(this._1.setToTop(variable), this._2))
      }
      else if(expr.isInstanceOf[HeapIdSetDomain[I]]) {
        val variable : HeapIdSetDomain[I] = expr.asInstanceOf[HeapIdSetDomain[I]]
        result=result.lub(result, new AbstractState(HeapIdSetFunctionalLifting.applyToSetHeapId(this._1, variable, this._1.setToTop(_)), this._2))

      }
      else throw new SymbolicSemanticException("Something assignable expected here")
    }
    result
  }

  def assert(cond : ExpressionSet) : AbstractState[N,H,I] = this //TODO

  def assume(cond : ExpressionSet) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    var d1 = this._1
    var isFirst = true
    for(expr <- cond.getSetOfExpressions) {
      //For each expression that is assumed, it computes the semantics and it considers the upper bound
      if(isFirst) {
        d1=this._1.assume(expr)
        isFirst=false
      }
      else {
        d1=d1.lub(d1, this._1.assume(expr))
      }
    }
    new AbstractState[N,H,I](d1, this._2)
  }

  def testTrue() : AbstractState[N,H,I] = {
    var result=this.assume(this.getExpression())
    result.removeExpression()
  }

  def testFalse() : AbstractState[N,H,I] = {
    var result=this.assume(this.getExpression().not())
    result.removeExpression()
  }

  def setExpression(value : ExpressionSet) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    new AbstractState(this._1, value)
  }

  def setState(value : HeapAndAnotherDomain[N, H, I]) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    new AbstractState(value, this._2)
  }

  private def getResult() : ExpressionSet = this._2

  override def toSingleLineString() : String = {
    if(isBottom) "_|_"
    else this._1.toString+";\nExpr.: "+this._2.toString
  }
  override def toString() : String = {
    if(isBottom) "_|_"
    else this._1.toString+"\nExpression:\n"+ToStringUtilities.indent(this._2.toString)
  }

  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp:Type, tpp: ProgramPoint, fields : Option[Set[Identifier]] = None): AbstractState[N, H, I] = {

    if (this.isBottom) return this

    // It discharges on the heap analysis the creation of the object and its fields
    val (createdLocation, newHeap, rep) = this.d1.createCollection(collTyp, keyTyp, valueTyp, lengthTyp, tpp)
    var result = new HeapAndAnotherDomain[N, H, I](newHeap._1.merge(rep), newHeap._2)

    // Create all variables involved representing the object
    result=HeapIdSetFunctionalLifting.applyToSetHeapId(result, createdLocation, result.createVariable(_, collTyp))
    var result2 = result
    for(field <- fields.orElse(Some(collTyp.getPossibleFields())).get) {
      val (ids, state, rep2) = HeapIdSetFunctionalLifting.applyGetFieldId(createdLocation, result2, result2._2.getFieldIdentifier(_, field.getName(), field.getType(), field.getProgramPoint()))
      result2=HeapIdSetFunctionalLifting.applyToSetHeapId(result2, ids, new HeapAndAnotherDomain[N, H, I](result2._1.merge(rep2), state).createVariable(_, field.getType()))
    }

    this.setExpression(new ExpressionSet(collTyp).add(createdLocation)).setState(result2)

  }

  def insertCollectionValue(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet, pp: ProgramPoint): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def insertCollectionValue(key: Expression, right: Expression, pp: ProgramPoint)(collection: Assignable):AbstractState[N, H, I] = {
      val newHeapAndSemantic = this.d1.insertCollectionElement(collection, key, right, pp)
      val newState = new AbstractState[N, H, I](newHeapAndSemantic, this.d2)
      newState.lub(this, newState)
    }

    var result = this.factory()
    result.d1 = this.d1
    result.d2 = this.d2
    for (collection <- collectionSet.getSetOfExpressions;
         key <- keySet.getSetOfExpressions;
         right <- rightSet.getSetOfExpressions) {
      collection match {
        case id: Assignable => result = result.lub(result, insertCollectionValue(key, right, pp)(id))
        case set: HeapIdSetDomain[I] => result = result.lub(result, HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, insertCollectionValue(key, right, pp)))
        case _ => ()
      }
    }

    result.removeExpression()
  }

  def removeCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet, valueTyp: Type): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def removeCollectionValue(result: AbstractState[N, H, I], key: Expression, valueTyp: Type) (collection: Assignable): AbstractState[N, H, I] = {
      val newHeapAndSemantic = result.d1.removeCollectionElementByKey(collection, key, valueTyp)
      new AbstractState[N, H, I](newHeapAndSemantic, result.d2)
    }

    var result = this
    for (collection <- collectionSet.getSetOfExpressions;
         key <- keySet.getSetOfExpressions) {
      collection match {
        case id: Assignable => result = removeCollectionValue(result, key, valueTyp)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, removeCollectionValue(result, key, valueTyp))
        case _ => ()
      }
    }

    result.removeExpression()
  }

  def removeCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, keyTyp: Type): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def removeCollectionValue(result: AbstractState[N, H, I], value: Expression, keyTyp: Type) (collection: Assignable) = {
      val newHeapAndSemantic = result.d1.removeCollectionElementByValue(collection, value, keyTyp)
      new AbstractState[N, H, I](newHeapAndSemantic, result.d2)
    }

    var result = this
    for (collection <- collectionSet.getSetOfExpressions;
         value <- valueSet.getSetOfExpressions) {
      collection match {
        case id: Assignable => result = removeCollectionValue(result, value, keyTyp)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), set, removeCollectionValue(result, value, keyTyp))
      }
    }

    result.removeExpression()
  }

  def clearCollection(collectionSet: ExpressionSet, keyTyp: Type, valueTyp: Type): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def clearCollection(result: AbstractState[N, H, I])(collection: Assignable) = {
      val newHeapAndSemantic = result.d1.clearCollection(collection, keyTyp, valueTyp)
      new AbstractState[N, H, I](newHeapAndSemantic, result.d2)
    }

    var result = this
    for (collection <- collectionSet.getSetOfExpressions) {
      collection match {
        case id: Assignable => result = clearCollection(result)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, clearCollection(result))
        case _ => ()
      }
    }

    result.removeExpression()
  }

  def assignAllCollectionKeys(collectionSet: ExpressionSet, valueSet: ExpressionSet, keyTyp: Type): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def invalidateCollectionKeys(result:AbstractState[N, H, I], value: Expression)(collection: Assignable) = {
      val newHeapAndSemantic = result.d1.assignAllCollectionKeys(collection, value, keyTyp)
      new AbstractState[N,H,I](newHeapAndSemantic, result.d2)
    }

    var result = this
    for (collection <- collectionSet.getSetOfExpressions;
         value <- valueSet.getSetOfExpressions) {
      collection match {
        case id:Assignable => result = invalidateCollectionKeys(result, value)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), set, invalidateCollectionKeys(result, value))
      }
    }

    result.removeExpression()
  }

  def getCollectionKeyByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): AbstractState[N, H, I] = {
    if(this.isBottom) return this

    def getCollectionKey(result:AbstractState[N, H, I], key: Expression)(collection: Assignable): AbstractState[N, H, I] = {
      val (newHeapAndSemantic, ids) = this.d1.getCollectionKeyByKey(collection, key)

      var expressions = new ExpressionSet(ids.getType()).bottom()
      if (!ids.isBottom) {
        expressions = new ExpressionSet(ids.getType()).add(ids)
      }

      val newState = new AbstractState(newHeapAndSemantic, expressions)
      newState.lub(newState, result)
    }

    var result: AbstractState[N, H, I] = this.bottom()

    for (expr <- collectionSet.getSetOfExpressions;
         keyExpr <- keySet.getSetOfExpressions) {
      expr match {
        case id: Assignable => result = getCollectionKey(result, keyExpr)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(this, set, getCollectionKey(result, keyExpr))
        case _ => ()
      }
    }

    result
  }

  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet, valueTyp: Type): AbstractState[N, H, I] = {
    if(this.isBottom) return this

    def getCollectionValue(result:AbstractState[N, H, I], key: Expression)(collection: Assignable): AbstractState[N, H, I] = {
      val (newHeapAndSemantic, ids) = this.d1.getCollectionValueByKey(collection, key, valueTyp)

      var expressions = new ExpressionSet(ids.getType())
      if (!ids.isBottom) {
        expressions = new ExpressionSet(ids.getType()).add(ids)
      }

      val newState = new AbstractState(newHeapAndSemantic, expressions)
      newState.lub(newState, result)
    }

    var result: AbstractState[N, H, I] = this.bottom()

    for (expr <- collectionSet.getSetOfExpressions;
         keyExpr <- keySet.getSetOfExpressions) {
      expr match {
        case id: Assignable => result = getCollectionValue(result, keyExpr)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(this, set, getCollectionValue(result, keyExpr))
        case _ => ()
      }
    }

    result
  }

  def getCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet): AbstractState[N, H, I] = {
    if(this.isBottom) return this

    def getCollectionValue(result:AbstractState[N, H, I], value: Expression)(collection: Assignable): AbstractState[N, H, I] = {
      val (newHeapAndSemantic, ids) = this.d1.getCollectionValueByValue(collection, value)

      var expressions = new ExpressionSet(ids.getType()).bottom()
      if(!ids.isBottom){
        expressions = new ExpressionSet(ids.getType()).add(ids)
      }

      val newState = new AbstractState(newHeapAndSemantic, expressions)
      newState.lub(newState, result)
    }

    var result: AbstractState[N, H, I] = this.bottom()

    for (expr <- collectionSet.getSetOfExpressions;
         valueExpr <- valueSet.getSetOfExpressions) {
      expr match {
        case id: Assignable => result = getCollectionValue(result, valueExpr)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(this, set, getCollectionValue(result, valueExpr))
        case _ => ()
      }
    }

    result
  }

  def copyCollection(fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet, keyTyp: Type, valueTyp: Type): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def copyCollection(result: AbstractState[N, H, I], toCollection:Expression, keyTyp: Type, valueTyp: Type)(fromCollection: Assignable) = {

      def copyCollection(result: AbstractState[N, H, I], fromCollection: Assignable, keyTyp: Type, valueTyp: Type)(toCollection: Assignable) = {
        val newHeapAndSemantic = result.d1.copyCollection(fromCollection, toCollection, keyTyp, valueTyp)
        val done = new AbstractState[N, H, I](newHeapAndSemantic, result.d2)
        result.lub(result, done)
      }

      toCollection match {
        case id: Assignable => copyCollection(result, fromCollection, keyTyp, valueTyp)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), set, copyCollection(result, fromCollection, keyTyp, valueTyp))
      }
    }

    var result = this
    for (fromCollection <- fromCollectionSet.getSetOfExpressions;
         toCollection <- toCollectionSet.getSetOfExpressions) {
      fromCollection match {
        case id: Assignable => result = copyCollection(result, toCollection, keyTyp, valueTyp)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), set, copyCollection(result, toCollection, keyTyp, valueTyp))
      }
    }

    result.removeExpression()
  }

  def extractCollectionKeys(fromCollectionSet: ExpressionSet, newKeyValueSet: ExpressionSet, collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def extractCollectionKeys(result: AbstractState[N, H, I], newKeyValue:Expression, collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint)(fromCollection: Assignable):  AbstractState[N, H, I] = {
      val (newHeapAndSemantic, collectionIds) = result.d1.extractCollectionKeys(fromCollection, newKeyValue, collTyp, keyTyp, valueTyp, lengthTyp, pp)
      if (collectionIds == null) return this.bottom()

      new AbstractState[N, H, I](newHeapAndSemantic, new ExpressionSet(collTyp).add(collectionIds))
    }

    var result = this.factory()
    result.d1 = this.d1
    result.d2 = this.d2
    for (fromCollection <- fromCollectionSet.getSetOfExpressions;
         newKeyValue <- newKeyValueSet.getSetOfExpressions) {
      fromCollection match {
        case id: Assignable => result = extractCollectionKeys(result, newKeyValue, collTyp, keyTyp, valueTyp, lengthTyp, pp)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), set, extractCollectionKeys(result, newKeyValue, collTyp, keyTyp, valueTyp, lengthTyp, pp))
      }
    }

    result
  }

  def getCollectionLength(collectionSet: ExpressionSet): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    var result = this.bottom().d1
    var heapId: HeapIdSetDomain[I] = null

    def getCollectionLength(id:Assignable):HeapIdSetDomain[I] = {
      val createdLocation = this._1._2.getCollectionLength(id)
      result = result.lub(result, new HeapAndAnotherDomain[N, H, I](this._1._1, this._1._2))
      heapId = heapId match {
        case null => createdLocation
        case _ => heapId.lub(heapId, createdLocation)
      }
      heapId
    }

    for (collection <- collectionSet.getSetOfExpressions) {
      collection match {
        case id: Assignable => getCollectionLength(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(set.bottom(), set, getCollectionLength)
        case _ => ()
      }
    }
    if (heapId == null) this.bottom()
    else this.setExpression(new ExpressionSet(SystemParameters.getType().top()).add(heapId)).setState(result)
  }

  def isSummaryCollection(collectionSet: ExpressionSet): Boolean = {

    for (collection <- collectionSet.getSetOfExpressions) {
      collection match {
        case id:I =>
          if (this.d1.isSummaryCollection(id)) return true
        case set: HeapIdSetDomain[I] =>
          for (id <- set.value) {
            if (this.d1.isSummaryCollection(id)) return true
          }
        case _ => ()
      }
    }

    return false
  }

  /**
   * Removes all variables satisfying filter
   */
  def pruneVariables(filter:VariableIdentifier => Boolean) : AbstractState[N, H, I] = {

    var curState = this._1
    for (id <- this._1.getIds()) {
      id match {

        case va:VariableIdentifier =>
          if (filter(va)) {
            curState = curState.removeVariable(id)
          }

        case _ => ()

      }
    }

    new AbstractState(curState, this._2)
  }

  /**
   * Performs abstract garbage collection
   */
  def pruneUnreachableHeap() : AbstractState[N, H, I] = {

    // TODO
    //    val unreachable = this._1._2.getUnreachableHeap
    //    pruneVariables({
    //      case a:I => unreachable.contains(a)
    //      case _ => false
    //    })

    this

  }

  override def lubWithReplacement(l : AbstractState[N,H,I], r : AbstractState[N,H,I]) : (AbstractState[N,H,I],Replacement) = {
    if (l.isBottom) return (r,new Replacement())
    if (r.isBottom) return (l,new Replacement())
    val result : AbstractState[N,H,I] = this.factory()
    val (d, rep) =d1.lubWithReplacement(l.d1, r.d1)
    result.d1=d
    val s = d2.lub(l.d2, r.d2)
    if (!rep.isEmpty())
      result.d2 = s.merge(rep)
    else
      result.d2 = s
    (result,rep)
  }

  override def lub(l : AbstractState[N,H,I], r : AbstractState[N,H,I]) : AbstractState[N,H,I] = lubWithReplacement(l,r)._1

  override def glbWithReplacement(l : AbstractState[N,H,I], r : AbstractState[N,H,I]) : (AbstractState[N,H,I],Replacement) = {
    if (l.isBottom || r.isBottom) return (bottom(),new Replacement())
    val result : AbstractState[N,H,I] = this.factory()
    val (d, rep) =d1.glbWithReplacement(l.d1, r.d1)
    result.d1=d
    val s = d2.glb(l.d2, r.d2)
    result.d2= s.merge(rep)
    (result,rep)
  }

  override def glb(l : AbstractState[N,H,I], r : AbstractState[N,H,I]) : AbstractState[N,H,I] = glbWithReplacement(l,r)._1

  override def wideningWithReplacement(l : AbstractState[N,H,I], r : AbstractState[N,H,I]) : (AbstractState[N,H,I],Replacement) = {
    if (l.isBottom) return (r,new Replacement())
    if (r.isBottom) return (l,new Replacement())
    val result : AbstractState[N,H,I] = this.factory()
    val (d, rep) =d1.wideningWithReplacement(l.d1, r.d1)
    result.d1=d
    val s = d2.widening(l.d2, r.d2)
    result.d2= s.merge(rep)
    (result,rep)
  }

  override def widening(l : AbstractState[N,H,I], r : AbstractState[N,H,I]) : AbstractState[N,H,I] = wideningWithReplacement(l,r)._1
  
}