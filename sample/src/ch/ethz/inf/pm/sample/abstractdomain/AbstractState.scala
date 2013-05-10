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

}

class SetOfExpressions extends SetDomain[Expression, SetOfExpressions] {
  def factory() : SetOfExpressions = new SetOfExpressions()
  override def bottom() = super.bottom()
}


class AbstractState[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](state : HeapAndAnotherDomain[N, H, I], expr : ExpressionSet) extends
  CartesianProductDomain[HeapAndAnotherDomain[N, H, I], ExpressionSet, AbstractState[N,H,I]](state, expr) with State[AbstractState[N,H,I]] with SingleLineRepresentation {

  def factory() = new AbstractState(this._1.factory(), this._2.factory())
  override def bottom() = new AbstractState(this._1.bottom(), this._2.bottom())
  def isBottom : Boolean = this._1.equals(this._1.bottom()) || this._2.equals(this._2.bottom())
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


  def createObject(typ : Type, pp : ProgramPoint, createFields : Boolean = true) : AbstractState[N,H,I] =  {
    if(this.isBottom) return this
    //It discharges on the heap analysis the creation of the object and its fields
    var (createdLocation, newHeap, rep)=this._1._2.createObject(typ, pp)
    var result=new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap)

    result=HeapIdSetFunctionalLifting.applyToSetHeapId(result, createdLocation, result.createVariable(_, typ))
    var result2 = result

    if(createFields) {
      for(field <- typ.getPossibleFields()) {
        val (ids, state, rep2) = HeapIdSetFunctionalLifting.applyGetFieldId(createdLocation, result2, result2._2.getFieldIdentifier(_, field.getName(), field.getType(), field.getProgramPoint()))

        result2=HeapIdSetFunctionalLifting.applyToSetHeapId(result2, ids, new HeapAndAnotherDomain[N, H, I](result2._1.merge(rep2), state).createVariable(_, field.getType()))
      }
    }
    //val (h, rep2) = result2._2.endOfAssignment()
    //result2 = new HeapAndAnotherDomain[N, H, I](result2._1.merge(rep2), h)
    //An object could have no fields, so that's acceptable to have result3==None here
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
    if(this.isBottom) return this
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
		          //result=result.setExpression(new ExpressionSet(new UnitExpression(variable.getType().bottom(), variable.getProgramPoint), this.removeExpression()))
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
        var result2=new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap)
        var accessed = HeapIdSetFunctionalLifting.applyToSetHeapId(result2, heapid, result2.backwardAccess(_))
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

  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp:Type, tpp: ProgramPoint): AbstractState[N, H, I] = {
    if (this.isBottom) return this
    val (heapId, newHeap, rep) = this._1._2.createCollection(collTyp, keyTyp, valueTyp, lengthTyp, tpp, this._1._1)
    val result = new HeapAndAnotherDomain[N, H, I](rep, newHeap)
    if (heapId == null) this.bottom()
    else this.setExpression(new ExpressionSet(collTyp).add(heapId)).setState(result)
  }

  def assignCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet): AbstractState[N, H, I] = {
    if (this.isBottom) return this
    var result = this.bottom()
    if (rightSet.isTop) {
      val t: AbstractState[N, H, I] = this.getCollectionCell(collectionSet, keySet)
      return t.setVariableToTop(t.getExpression()).removeExpression()
    }

    def assignCollectionCell(result:AbstractState[N,H,I],key:Expression,right:Expression)(variable:Assignable):AbstractState[N,H,I] = {
      val assigned = this._1.assignCollectionCell(variable, key, right)
      val done = new AbstractState[N, H, I](assigned, this._2)
      done.lub(result, done)
    }

    for (collection <- collectionSet.getSetOfExpressions;
         key <- keySet.getSetOfExpressions;
         right <- rightSet.getSetOfExpressions) {
      collection match {
        case variable: Assignable => result = assignCollectionCell(result,key,right)(variable)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(this,set,assignCollectionCell(result,key,right))
        case _ => throw new SymbolicSemanticException("I can assign only variables and heap ids here")
      }
    }
    result.removeExpression()
  }

  def insertCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet): AbstractState[N, H, I] = {
    if (this.isBottom) return this
    var result = this.bottom().d1

    def insertCollectionCell(result:HeapAndAnotherDomain[N, H, I],key:Expression,right:Expression)(id:Assignable):HeapAndAnotherDomain[N, H, I] = {
      val (newHeap, rep) = this._1._2.insertCollectionCell(id,key,right, this._1._1)
      result.lub(result, new HeapAndAnotherDomain[N, H, I](rep, newHeap))
    }

    for (collection <- collectionSet.getSetOfExpressions;
         key <- keySet.getSetOfExpressions;
         right <- rightSet.getSetOfExpressions) {
      collection match {
        case id: Assignable => result = insertCollectionCell(result,key,right)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(this._1, set,insertCollectionCell(result,key,right))
        case _ => ()
      }
    }

    this.removeExpression().setState(result)
  }

  def removeCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet): AbstractState[N, H, I] = {
    if (this.isBottom) return this
    var result = this.bottom().d1

    def removeCollectionCell(result:HeapAndAnotherDomain[N, H, I],key:Expression)(id:Assignable):HeapAndAnotherDomain[N, H, I] = {
      val (newHeap, rep) = this._1._2.removeCollectionCell(id,key,this._1._1)
      result.lub(result, new HeapAndAnotherDomain[N, H, I](rep, newHeap))
    }

    for (collection <- collectionSet.getSetOfExpressions;
         key <- keySet.getSetOfExpressions) {
      collection match {
        case id: Assignable => result = removeCollectionCell(result,key)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(this._1, set,removeCollectionCell(result,key))
        case _ => ()
      }
    }

    this.removeExpression().setState(result)
  }

  def getCollectionCell(collectionSet: ExpressionSet, keySet: ExpressionSet): AbstractState[N, H, I] = {
    if (this.isBottom) return this
    var result: AbstractState[N, H, I] = this.bottom()

    def getCollectionCell(keyExpr:Expression)(assignable:Assignable):AbstractState[N, H, I] = {
      val (heapID, newHeap, rep) = this._1._2.getCollectionCell(assignable, keyExpr, this._1._1)
      val result2 = new HeapAndAnotherDomain[N, H, I](rep, newHeap)
      val accessed = HeapIdSetFunctionalLifting.applyToSetHeapId(this._1, heapID, result2.access(_))
      val state = new AbstractState(accessed, new ExpressionSet(heapID.getType()).add(heapID))
      result = result.lub(result, state)
      result
    }

    for (expr <- collectionSet.getSetOfExpressions;
         keyExpr <- keySet.getSetOfExpressions) {
      expr match {
        case id: Assignable => getCollectionCell(keyExpr)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this, set, getCollectionCell(keyExpr))
        case _ => ()
      }
    }
    result
  }

  def clearCollection(collectionSet: ExpressionSet) : AbstractState[N, H, I] = {
    if (this.isBottom) return this
    var result = this.bottom().d1

    def clearCollection(result:HeapAndAnotherDomain[N, H, I])(id:Assignable):HeapAndAnotherDomain[N, H, I] = {
      val (newHeap, rep) = this._1._2.clearCollection(id,this._1._1)
      result.lub(result, new HeapAndAnotherDomain[N, H, I](rep, newHeap))
    }

    for (collection <- collectionSet.getSetOfExpressions) {
      collection match {
        case id: Assignable => result = clearCollection(result)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(this._1, set,clearCollection(result))
        case _ => ()
      }
    }

    this.removeExpression().setState(result)
  }


  def getCollectionLength(collectionSet: ExpressionSet): AbstractState[N, H, I] = {
    if (this.isBottom) return this
    var result = this.bottom().d1
    var heapId: HeapIdSetDomain[I] = null

    def getCollectionLength(id:Assignable):HeapIdSetDomain[I] = {
      val (createdLocation, newHeap, rep) = this._1._2.getCollectionLength(id,this._1._1)
      result = result.lub(result, new HeapAndAnotherDomain[N, H, I](rep, newHeap))
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

}