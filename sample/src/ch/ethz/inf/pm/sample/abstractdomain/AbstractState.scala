package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._
import util.HeapIdSetFunctionalLifting

object ExpressionFactory {

  def createCollectionContains(collectionSet: ExpressionSet, keySet: ExpressionSet, valueSet: ExpressionSet, ty: Type, pp: ProgramPoint) = {
    var result = new ExpressionSet(ty)
    for(collection <- collectionSet.getSetOfExpressions;
        key <- keySet.getSetOfExpressions;
        value <- valueSet.getSetOfExpressions) {
      result = result.add(new CollectionContainsExpression(collection, key, value, ty, pp))
    }

    result
  }

  def createVariable(variable : Variable, ty : Type, pp : ProgramPoint): ExpressionSet= {
    var result = new ExpressionSet(ty)
    result=result.add(new VariableIdentifier(variable.getName, ty, pp, variable.id.scope))
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
    val combinations = combineListValue(parameters)
    for(thisexp <- thisExpr.getSetOfExpressions)
      for (combination <- combinations)
        result=result.add(new AbstractOperator(thisexp, combination, typeParameters, op, ty))
    result
  }

  lazy val unitExpr = ExpressionSet(new UnitExpression(SystemParameters.typ.top(), DummyProgramPoint))

  def createUnitExpression(pp: ProgramPoint): ExpressionSet = {
    ExpressionSet(new UnitExpression(SystemParameters.typ.top(), pp))
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

case class ExpressionSet(initialTyp : Type, s : SetOfExpressions = new SetOfExpressions)
  extends CartesianProductDomain[Type, SetOfExpressions, ExpressionSet](initialTyp, s) {

  override def factory() : ExpressionSet =
    new ExpressionSet(
      if(getType()==null) {if(SystemParameters.typ!=null) SystemParameters.typ.top(); else null} else getType().top(),
      new SetOfExpressions()
    )

  def factory(a:Type,b:SetOfExpressions) = new ExpressionSet(a,b)

  def getType() : Type = this._1.glb(this.computeType())

  def getSetOfExpressions = this._2.value

  def isTop = this._2.isTop

  def isBottom = this._2.isBottom

  private def computeType() : Type = {
    if(this._2.isTop) return SystemParameters.typ.top()
    var typ : Type = null
    for(t <- this.getSetOfExpressions)
      typ=if(typ==null) t.getType else typ.lub(t.getType)
    if(typ==null) SystemParameters.typ.top()
    else typ
  }

  def add(exp : Expression) : ExpressionSet = {
    val v2 :SetOfExpressions = this._2.add(exp)
    val typ = this._1.glb(exp.getType)
    new ExpressionSet(typ, v2)
  }

  def add(expr : ExpressionSet) : ExpressionSet = {
    var set = this._2
    for (exp <- expr.getSetOfExpressions) set = set.add(exp)
    val typ = this._1.glb(expr.getType())
    new ExpressionSet(typ,set)
  }

  def not() : ExpressionSet = {
    var result : SetOfExpressions = this._2.factory()
    for(key <- getSetOfExpressions)
      result=result.add(new NegatedBooleanExpression(key))
    new ExpressionSet(getType(), result)
  }

  override def toString:String = "Type "+_1.toString+": "+_1.toString

  def merge(r:Replacement) : ExpressionSet = {

    if (r.isEmpty()) return this

    var newSet = this._2.value

    for ((froms,tos) <- r.value; from <- froms) {
      // HACK: replace does not work with replacements that remove variables ({h1,h2} -> {})
      if(!tos.isEmpty)
        newSet = (for (to <- tos) yield { newSet.map( _.replace(from,to) ) }).flatten.toSet
    }

    new ExpressionSet(getType(),new SetOfExpressions(newSet))

  }

}

object ExpressionSet {
  /** Creates a singleton `ExpressionSet` from a given expression. */
  def apply(exp: Expression): ExpressionSet =
    new ExpressionSet(exp.getType).add(exp)

  /** Creates an empty `ExpressionSet` whose type is top. */
  def apply(): ExpressionSet =
    new ExpressionSet(SystemParameters.typ.top())
}

class SetOfExpressions(_value: Set[Expression] = Set.empty[Expression], _isTop: Boolean = false, _isBottom: Boolean = false)
  extends SetDomain[Expression, SetOfExpressions](_value,_isTop,_isBottom) {

  def setFactory (_value: Set[Expression] = Set.empty[Expression], _isTop: Boolean = false, _isBottom: Boolean = false): SetOfExpressions
    = new SetOfExpressions(_value,_isTop,_isBottom)

}





class AbstractState[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](state : HeapAndAnotherDomain[N, H, I], initialExpression : ExpressionSet)
  extends CartesianProductDomain[HeapAndAnotherDomain[N, H, I], ExpressionSet, AbstractState[N,H,I]](state, initialExpression)
  with SimpleState[AbstractState[N,H,I]]
  with SingleLineRepresentation
  with LatticeWithReplacement[AbstractState[N,H,I]]
  {

  def factory(a:HeapAndAnotherDomain[N, H, I],b:ExpressionSet) = new AbstractState(a,b)
  override def bottom() = new AbstractState(this._1.bottom(), this._2.bottom())
  def isBottom : Boolean = this._1.lessEqual(this._1.bottom()) || this._2.lessEqual(this._2.bottom())
  def getStringOfId(id : Identifier) : String = this._1.getStringOfId(id)
  def getState = this._1

  def getSemanticDomain = this._1._1
  def getHeapDomain = this._1._2

  def before(pp : ProgramPoint) = this

  def createObject(typ : Type, pp : ProgramPoint, fields : Option[Set[Identifier]] = None) : AbstractState[N,H,I] =  {

    if(this.isBottom) return this

    // It discharges on the heap analysis the creation of the object and its fields
    val (createdLocation, newHeap, rep) = this._1._2.createObject(typ, pp)
    var result = new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap)

    // Create all variables involved representing the object
    result=HeapIdSetFunctionalLifting.applyToSetHeapId(result, createdLocation, result.createVariable(_, typ))
    var result2 = result
    for (field <- fields.orElse(Some(typ.possibleFields)).get) {
      val (ids, state, rep2) = HeapIdSetFunctionalLifting.applyGetFieldId(createdLocation, result2, result2._2.getFieldIdentifier(_, field.getName, field.getType, field.pp))
      result2=HeapIdSetFunctionalLifting.applyToSetHeapId(result2, ids, new HeapAndAnotherDomain[N, H, I](result2._1.merge(rep2), state).createVariable(_, field.getType))
    }

    this.setExpression(new ExpressionSet(typ).add(createdLocation)).setState(result2)

  }

  def getExpression : ExpressionSet = getResult()

  def removeExpression() : AbstractState[N,H,I] = {
    if(this.isBottom) return  new AbstractState(this._1, this._2.bottom())
    new AbstractState(this._1, ExpressionSet())
  }

  def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): AbstractState[N, H, I] =
    new AbstractState[N, H, I](_1.createVariable(variable, typ), _2)

  def createVariableForArgument(variable: VariableIdentifier, typ: Type): AbstractState[N, H, I] =
    new AbstractState[N, H, I](_1.createVariableForArgument(variable, typ, Nil)._1, _2)

  def assignVariable(left: Expression, right: Expression): AbstractState[N, H, I] = {
    left match {
      case variable: Assignable =>
         new AbstractState[N, H, I](this._1.assign(variable, right), this._2)
      case ids: HeapIdSetDomain[I] =>
         new AbstractState[N, H, I](HeapIdSetFunctionalLifting.applyToSetHeapId(this._1, ids, this._1.assign(_, right)), this._2)
      case _ =>
        throw new SymbolicSemanticException("I can assign only variables here")
    }
  }

  def assignField(obj: Expression, field: String, right: Expression): AbstractState[N, H, I] = {
    obj match {
      case variable : Identifier =>
        new AbstractState[N,H,I](this._1.assignField(variable, field, right, right.getType, variable.pp ), this._2)
      case heapid : HeapIdSetDomain[I] =>
        new AbstractState[N,H,I](HeapIdSetFunctionalLifting.applyToSetHeapId(this._1, heapid, this._1.assignField(_, field, right, right.getType, heapid.pp )), this._2)
      case _ =>
        throw new SymbolicSemanticException("I can assign only variables and heap ids here")
    }
  }

  override def backwardAssignVariable(x : ExpressionSet, right : ExpressionSet) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    if(right.isTop) return top()
    var result : AbstractState[N,H,I] = this.bottom()
    for(el <- x.getSetOfExpressions) {
      //For each variable that is potentially assigned, it computes its semantics and it considers the upper bound
      el match {
        case variable : Assignable => {
          for(assigned <- right.getSetOfExpressions) {
            val done=new AbstractState[N,H,I](this._1.backwardAssign(variable, assigned), this._2)
            result=result.lub(done)
            result=result.setExpression(ExpressionSet(new UnitExpression(variable.getType.top(), variable.pp)))
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
            result=result.lub(done)
            result=result.setExpression(ExpressionSet(new UnitExpression(variable.getType.top(), variable.pp)))
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
            val done=new AbstractState[N,H,I](this._1.removeVariable(variable), ExpressionSet(new UnitExpression(variable.getType.top(), variable.pp)))
            result=result.lub(done)
            result=result.setExpression(ExpressionSet(new UnitExpression(variable.getType.top(), variable.pp)))
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
    this.setExpression(ExpressionSet(new Constant(value, typ, pp)))
  }

  def getVariableValue(id : Assignable) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    val state = new AbstractState(this._1.access(id), this.removeExpression().getExpression)
    new AbstractState(state._1, ExpressionSet(id.asInstanceOf[Expression]))
  }

  def backwardGetVariableValue(id : Assignable) : AbstractState[N,H,I] = {
    if(this.isBottom) return this
    val state = new AbstractState(this._1.backwardAccess(id), this.removeExpression().getExpression)
    new AbstractState(state._1, ExpressionSet(id.asInstanceOf[Expression]))
  }

  def getType(variable : Identifier) : Type = {//TODO: is this correct???
    variable.getType
  }

  def getFieldValue(obj: ExpressionSet, field: String, typ: Type): AbstractState[N, H, I] = {
    if (this.isBottom) return this
    var result = this.bottom()
    for (expr <- obj.getSetOfExpressions) {
      if (expr.isInstanceOf[Assignable] || expr.isInstanceOf[HeapIdSetDomain[I]]) {
        val (heapid, newHeap, rep) =
          if (expr.isInstanceOf[Assignable])
            this._1._2.getFieldIdentifier(expr.asInstanceOf[Assignable], field, typ, expr.pp)
          else HeapIdSetFunctionalLifting.applyGetFieldId(expr.asInstanceOf[HeapIdSetDomain[I]], this._1,
            this._1._2.getFieldIdentifier(_, field, typ, expr.pp))

        val result2 = new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap)
        val accessed = if (heapid.isTop) result2.top() else HeapIdSetFunctionalLifting.applyToSetHeapId(result2, heapid, result2.access(_))
        val state = new AbstractState(accessed, new ExpressionSet(typ).add(heapid))
        result = result.lub(state)
      }
    }
    result
  }

  def backwardGetFieldValue(obj: ExpressionSet, field: String, typ: Type): AbstractState[N, H, I] = {
    if(this.isBottom) return this
    var result : AbstractState[N,H,I] = this.bottom()
    for(expr <- obj.getSetOfExpressions) {
      if(! expr.isInstanceOf[Assignable]) throw new SymbolicSemanticException("Only assignable objects should be here")
      val (heapid, newHeap, rep) = this._1._2.getFieldIdentifier(expr.asInstanceOf[Assignable], field, typ, expr.pp)
      val result2=new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap)
      val accessed = HeapIdSetFunctionalLifting.applyToSetHeapId(result2, heapid, result2.backwardAccess)
      val state=new AbstractState(accessed, new ExpressionSet(typ).add(heapid))
      result=result.lub(state)
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
        result=result.lub(new AbstractState(this._1.setToTop(variable), this._2))
      }
      else if(expr.isInstanceOf[HeapIdSetDomain[I]]) {
        val variable : HeapIdSetDomain[I] = expr.asInstanceOf[HeapIdSetDomain[I]]
        result=result.lub(new AbstractState(HeapIdSetFunctionalLifting.applyToSetHeapId(this._1, variable, this._1.setToTop(_)), this._2))

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
    var rep = new Replacement()
    for(expr <- cond.getSetOfExpressions) {
      //For each expression that is assumed, it computes the semantics and it considers the upper bound
      if(isFirst) {
        val (d, r) = this._1.assume(expr)
        d1= d
        rep = rep ++ r
        isFirst=false
      }
      else {
        val(d, r) = this._1.assume(expr)
        d1=d1.lub(d)
        rep = rep ++ r
      }
    }
    new AbstractState[N,H,I](d1, this._2.merge(rep))
  }

  def testTrue() : AbstractState[N,H,I] = {
    var result=this.assume(this.getExpression)
    result.removeExpression()
  }

  def testFalse() : AbstractState[N,H,I] = {
    var result=this.assume(this.getExpression.not())
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
  override def toString : String = {
    if(isBottom) "_|_"
    else this._1.toString+"\nExpression:\n"+ToStringUtilities.indent(this._2.toString)
  }

  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp:Type, keyCollectionTyp:Option[Type], tpp: ProgramPoint, fields : Option[Set[Identifier]] = None): AbstractState[N, H, I] = {

    if (this.isBottom) return this

    // It discharges on the heap analysis the creation of the object and its fields
    val (createdLocation, heapAndSemantics, _) = this._1.createCollection(collTyp, keyTyp, valueTyp, lengthTyp, None, keyCollectionTyp, tpp)
    var resHeapAndSemantics = heapAndSemantics

    // Create all variables involved representing the object
    resHeapAndSemantics=HeapIdSetFunctionalLifting.applyToSetHeapId(resHeapAndSemantics, createdLocation, resHeapAndSemantics.createVariable(_, collTyp))

    for (field <- fields.orElse(Some(collTyp.possibleFields)).get) {
      val (ids, state, rep2) = HeapIdSetFunctionalLifting.applyGetFieldId(createdLocation, resHeapAndSemantics, resHeapAndSemantics._2.getFieldIdentifier(_, field.getName, field.getType, field.pp))
      resHeapAndSemantics=HeapIdSetFunctionalLifting.applyToSetHeapId(resHeapAndSemantics, ids, new HeapAndAnotherDomain[N, H, I](resHeapAndSemantics._1.merge(rep2), state).createVariable(_, field.getType))
    }

    this.factory(resHeapAndSemantics,new ExpressionSet(collTyp).add(createdLocation))
  }

  def getCollectionValue(valueIds: ExpressionSet): AbstractState[N, H, I] = {
    if(this.isBottom) return this

    def getCollectionValue(result: AbstractState[N, H, I])(valueId: Assignable) = {
      val (newHeapAndSemantic, ids) = result._1.getCollectionValue(valueId)
      val newExpressions = ExpressionSet(ids)
      new AbstractState[N, H, I](newHeapAndSemantic, newExpressions)
    }

    var result = this.bottom()

    for (valueId <- valueIds.getSetOfExpressions) {
      val newState = valueId match {
        case id: Assignable => getCollectionValue(this)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, getCollectionValue(this))
        case _ => this.bottom()
      }

      result = result.lub(newState)
    }

    result
  }

  def getSummaryCollectionIfExists(collectionSet: ExpressionSet) : AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def getSummaryCollectionIfExists(result: AbstractState[N, H, I])(collection: Assignable): AbstractState[N, H, I] = {
      val (newHeapAndSemantic, ids) = result._1.getSummaryCollectionIfExists(collection)

      var expressions = new ExpressionSet(ids.getType).bottom()
      if (!ids.isBottom) {
        expressions = ExpressionSet(ids)
      }

      new AbstractState[N,H,I](newHeapAndSemantic, expressions)
    }

    var result = this.bottom()
    for (collection <- collectionSet.getSetOfExpressions) {
      val newState = collection match {
        case id: Assignable => getSummaryCollectionIfExists(this)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, getSummaryCollectionIfExists(this))
        case _ => this.bottom()
      }

      result = result.lub(newState)
    }

    result
  }

  def insertCollectionTopElement(collectionSet: ExpressionSet, keyTopSet: ExpressionSet, valueTopSet: ExpressionSet, pp: ProgramPoint): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def insertCollectionTopElement(result: AbstractState[N, H, I], keyTop: Expression, valueTop: Expression, pp: ProgramPoint)(collection: Assignable): AbstractState[N, H, I] = {
      val newHeapAndSemantic = result._1.insertCollectionTopElement(collection, keyTop, valueTop, pp)
      new AbstractState[N, H, I](newHeapAndSemantic, this._2)
    }

    var result = this.bottom()
    for (collection <- collectionSet.getSetOfExpressions;
        keyTop <- keyTopSet.getSetOfExpressions;
        valueTop <- valueTopSet.getSetOfExpressions) {
      val newState = collection match {
        case id: Assignable => insertCollectionTopElement(this, keyTop, valueTop, pp)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, insertCollectionTopElement(this, keyTop, valueTop, pp))
        case _ => this.bottom()
      }
      result = result.lub(newState)
    }
    result.removeExpression()
  }

  def insertCollectionElement(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet, pp: ProgramPoint): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def insertCollectionValue(result: AbstractState[N, H, I], key: Expression, right: Expression, pp: ProgramPoint)(collection: Assignable):AbstractState[N, H, I] = {
      val (newHeapAndSemantic, rep) = result._1.insertCollectionElement(collection, key, right, pp)
      new AbstractState[N, H, I](newHeapAndSemantic, this._2.merge(rep))
    }

    var result = this.bottom()
    for (collection <- collectionSet.getSetOfExpressions;
         key <- keySet.getSetOfExpressions;
         right <- rightSet.getSetOfExpressions) {
      val newState = collection match {
        case id: Assignable => insertCollectionValue(this, key, right, pp)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, insertCollectionValue(this, key, right, pp))
        case _ => this.bottom()
      }
      result = result.lub(newState)
    }

    result.removeExpression()
  }

  def removeCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def removeCollectionValue(result: AbstractState[N, H, I], key: Expression) (collection: Assignable): AbstractState[N, H, I] = {
      val newHeapAndSemantic = result._1.removeCollectionElementByKey(collection, key)
      new AbstractState[N, H, I](newHeapAndSemantic, result._2)
    }

    var result = this.bottom()
    for (collection <- collectionSet.getSetOfExpressions;
         key <- keySet.getSetOfExpressions) {
      val newState = collection match {
        case id: Assignable => removeCollectionValue(this, key)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, removeCollectionValue(this, key))
        case _ => this.bottom()
      }
      result = result.lub(newState)
    }

    result.removeExpression()
  }

  def removeFirstCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet) = {

    def removeFirstCollectionValue(result: AbstractState[N, H, I], value: Expression) (collection: Assignable) = {
      val newHeapAndSemantic = result._1.removeFirstCollectionElementByValue(collection, value)
      new AbstractState[N, H, I](newHeapAndSemantic, result._2)
    }

    var result = this.bottom()
    for (collection <- collectionSet.getSetOfExpressions;
        value <- valueSet.getSetOfExpressions) {
      val newState = collection match {
        case id: Assignable => removeFirstCollectionValue(this, value)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, removeFirstCollectionValue(this, value))
        case _ => this.bottom()
      }
      result = result.lub(newState)
    }

    result.removeExpression()
  }

  def clearCollection(collectionSet: ExpressionSet): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def clearCollection(result: AbstractState[N, H, I])(collection: Assignable) = {
      val newHeapAndSemantic = result._1.clearCollection(collection)
      new AbstractState[N, H, I](newHeapAndSemantic, result._2)
    }

    var result = this.bottom
    for (collection <- collectionSet.getSetOfExpressions) {
      val newState = collection match {
        case id: Assignable => clearCollection(this)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, clearCollection(this))
        case _ => this.bottom()
      }

      result = result.lub(newState)
    }

    result.removeExpression()
  }

  def assignAllCollectionKeys(collectionSet: ExpressionSet, valueSet: ExpressionSet): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def invalidateCollectionKeys(result:AbstractState[N, H, I], value: Expression)(collection: Assignable) = {
      val newHeapAndSemantic = result._1.assignAllCollectionKeys(collection, value)
      new AbstractState[N,H,I](newHeapAndSemantic, result._2)
    }

    var result = this.bottom()
    for (collection <- collectionSet.getSetOfExpressions;
         value <- valueSet.getSetOfExpressions) {
      val newState = collection match {
        case id:Assignable => invalidateCollectionKeys(this, value)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, invalidateCollectionKeys(this, value))
        case _ => this.bottom()
      }

      result = result.lub(newState)
    }

    result.removeExpression()
  }

  def collectionContainsKey(collectionSet: ExpressionSet, keySet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def collectionContainsKey(initial: AbstractState[N, H, I], key: Expression, booleanTyp: Type, pp: ProgramPoint)(collection: Assignable): AbstractState[N, H, I] = {
      key match {
        case set:HeapIdSetDomain[I] =>
          var result = initial
          for (id <- set.value) {
            val newState = collectionContainsKey(result, id, booleanTyp, pp)(collection)
            result = result.lub(newState)
          }
          result
        case _ =>
          val containsKey = initial._1.collectionContainsKey(collection, key)

          var expression: Expression = new BinaryNondeterministicExpression(Constant("true", booleanTyp, pp), Constant("false", booleanTyp, pp), NondeterministicOperator.or, booleanTyp)

          if (containsKey.equals(BooleanDomain.domTrue)) {
            expression = Constant("true", booleanTyp, pp)
          }
          else if (containsKey.equals(BooleanDomain.domFalse)) {
            expression = Constant("false", booleanTyp, pp)
          }

          new AbstractState[N, H, I](initial._1, new ExpressionSet(booleanTyp).add(expression))
      }
    }

    var result = this.bottom()
    for(collection <- collectionSet.getSetOfExpressions;
        key <- keySet.getSetOfExpressions) {
      val newState = collection match {
        case id: Assignable => collectionContainsKey(this, key, booleanTyp, pp)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, collectionContainsKey(this, key, booleanTyp, pp))
        case _ => this.bottom()
      }

      result = result.lub(newState)
    }
    result
  }

  def collectionContainsValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, booleanTyp: Type, pp: ProgramPoint): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def collectionContainsValue(result: AbstractState[N, H, I], value: Expression, booleanTyp: Type, pp: ProgramPoint)(collection: Assignable) = {
      val containsValue = result._1.collectionContainsValue(collection, value)

      var expression: Expression = new BinaryNondeterministicExpression(Constant("true", booleanTyp, pp), Constant("false", booleanTyp, pp), NondeterministicOperator.or, booleanTyp)

      if (containsValue.equals(BooleanDomain.domTrue)) {
        expression = Constant("true", booleanTyp, pp)
      }
      else if (containsValue.equals(BooleanDomain.domFalse)) {
        expression = Constant("false", booleanTyp, pp)
      }

      new AbstractState[N, H, I](result._1, new ExpressionSet(booleanTyp).add(expression))
    }

    var result = this.bottom()
    for(collection <- collectionSet.getSetOfExpressions;
        value <- valueSet.getSetOfExpressions) {
      val newState = collection match {
        case id: Assignable => collectionContainsValue(this, value, booleanTyp, pp)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, collectionContainsValue(this, value, booleanTyp, pp))
        case _ => this.bottom()
      }
      result = result.lub(newState)
    }

    result
  }

  def getCollectionKeyByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): AbstractState[N, H, I] = {
    if(this.isBottom) return this

    def getCollectionKey(result:AbstractState[N, H, I], key: Expression)(collection: Assignable): AbstractState[N, H, I] = {
      val (newHeapAndSemantic, ids) = this._1.getCollectionKeyByKey(collection, key)

      var expressions = new ExpressionSet(ids.getType).bottom()
      if (!ids.isBottom) {
        expressions = ExpressionSet(ids)
      }

      new AbstractState(newHeapAndSemantic, expressions)
    }

    var result: AbstractState[N, H, I] = this.bottom()

    for (expr <- collectionSet.getSetOfExpressions;
         keyExpr <- keySet.getSetOfExpressions) {
      val newState = expr match {
        case id: Assignable => getCollectionKey(this, keyExpr)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this, set, getCollectionKey(this, keyExpr))
        case _ => this.bottom()
      }

      result = result.lub(newState)
    }

    result
  }

  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): AbstractState[N, H, I] = {
    if(this.isBottom) return this

    def getCollectionValue(result:AbstractState[N, H, I], key: Expression)(collection: Assignable): AbstractState[N, H, I] = {
      val (newHeapAndSemantic, ids) = this._1.getCollectionValueByKey(collection, key)

      var expressions = new ExpressionSet(ids.getType)
      if (!ids.isBottom) {
        expressions = ExpressionSet(ids)
      }

      new AbstractState(newHeapAndSemantic, expressions)
    }

    var result: AbstractState[N, H, I] = this.bottom()

    for (expr <- collectionSet.getSetOfExpressions;
         keyExpr <- keySet.getSetOfExpressions) {
      val newState = expr match {
        case id: Assignable => getCollectionValue(this, keyExpr)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this, set, getCollectionValue(this, keyExpr))
        case _ => this.bottom()
      }

      result = result.lub(newState)
    }

    result
  }

  def getCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet): AbstractState[N, H, I] = {
    if(this.isBottom) return this

    def getCollectionValue(result:AbstractState[N, H, I], value: Expression)(collection: Assignable): AbstractState[N, H, I] = {
      val (newHeapAndSemantic, ids) = this._1.getCollectionValueByValue(collection, value)

      var expressions = new ExpressionSet(ids.getType).bottom()
      if(!ids.isBottom){
        expressions = ExpressionSet(ids)
      }

      new AbstractState(newHeapAndSemantic, expressions)
    }

    var result: AbstractState[N, H, I] = this.bottom()

    for (expr <- collectionSet.getSetOfExpressions;
         valueExpr <- valueSet.getSetOfExpressions) {
      val newState = expr match {
        case id: Assignable => getCollectionValue(this, valueExpr)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this, set, getCollectionValue(this, valueExpr))
        case _ => this.bottom()
      }

      result = result.lub(newState)
    }

    result
  }

  def copyCollection(fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def copyCollection(result: AbstractState[N, H, I], toCollection:Expression)(fromCollection: Assignable) = {
      def copyCollection(result: AbstractState[N, H, I], fromCollection: Assignable)(toCollection: Assignable) = {
        val (newHeapAndSemantic,rep) = result._1.copyCollection(fromCollection, toCollection)
        new AbstractState[N, H, I](newHeapAndSemantic, result._2.merge(rep))
      }

      toCollection match {
        case id: Assignable => copyCollection(result, fromCollection)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), set, copyCollection(result, fromCollection))
      }
    }

    var result = this.bottom()
    for (fromCollection <- fromCollectionSet.getSetOfExpressions;
         toCollection <- toCollectionSet.getSetOfExpressions) {
      val newState = fromCollection match {
        case id: Assignable => copyCollection(this, toCollection)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, copyCollection(this, toCollection))
        case _ => this.bottom()
      }
      result = result.lub(newState)
    }

    result.removeExpression()
  }

  def extractCollectionKeys(fromCollectionSet: ExpressionSet, newKeyValueSet: ExpressionSet, fromCollectionTyp:Type, collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    def extractCollectionKeys(result: AbstractState[N, H, I], newKeyValue:Expression, collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint)(fromCollection: Assignable):  AbstractState[N, H, I] = {
      val (newHeapAndSemantic, collectionIds, rep) = result._1.extractCollectionKeys(fromCollection, newKeyValue, fromCollectionTyp, collTyp, keyTyp, valueTyp, lengthTyp, pp)
      if (collectionIds == null) return this.bottom()

      new AbstractState[N, H, I](newHeapAndSemantic, new ExpressionSet(collTyp).add(collectionIds).merge(rep))
    }

    var result = this.bottom()
    for (fromCollection <- fromCollectionSet.getSetOfExpressions;
         newKeyValue <- newKeyValueSet.getSetOfExpressions) {
      val newState = fromCollection match {
        case id: Assignable => extractCollectionKeys(this, newKeyValue, collTyp, keyTyp, valueTyp, lengthTyp, pp)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, extractCollectionKeys(this, newKeyValue, collTyp, keyTyp, valueTyp, lengthTyp, pp))
        case _ => this.bottom()
      }

      result = result.lub(newState)
    }

    result
  }

  def getOriginalCollection(collectionSet: ExpressionSet):AbstractState[N, H, I]= {
    if (this.isBottom) return this

    def getOriginalCollection(result:AbstractState[N, H, I])(collection: Assignable): AbstractState[N, H, I] = {
      val (newHeapAndSemantic, ids) = this._1.getOriginalCollection(collection)

      var expressions = new ExpressionSet(ids.getType).bottom()
      if(!ids.isBottom){
        expressions = ExpressionSet(ids)
      }

      new AbstractState(newHeapAndSemantic, expressions)
    }

    var result = this.bottom()
    for (collection <- collectionSet.getSetOfExpressions) {
      val newState = collection match {
        case id: Assignable => getOriginalCollection(this)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this, set, getOriginalCollection(this))
        case _ => this.bottom()
      }

      result = result.lub(newState)
    }

    result
  }

  def getKeysCollection(collectionSet: ExpressionSet):AbstractState[N, H, I]= {
    if (this.isBottom) return this

    def getKeysCollection(result:AbstractState[N, H, I])(collection: Assignable): AbstractState[N, H, I] = {
      val (newHeapAndSemantic, ids) = this._1.getKeysCollection(collection)

      var expressions = new ExpressionSet(ids.getType).bottom()
      if(!ids.isBottom){
        expressions = ExpressionSet(ids)
      }

      new AbstractState(newHeapAndSemantic, expressions)
    }

    var result = this.bottom()
    for (collection <- collectionSet.getSetOfExpressions) {
      val newState = collection match {
        case id: Assignable => getKeysCollection(this)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this, set, getKeysCollection(this))
        case _ => this.bottom()
      }

      result = result.lub(newState)
    }

    result
  }

  def removeCollectionKeyConnection(origCollectionSet: ExpressionSet, keyCollectionSet: ExpressionSet): AbstractState[N, H, I] = {

    def removeCollectionKeyConnection(initialState: AbstractState[N, H, I], keyCollection: Expression)(origCollection: Assignable) = {
      def removeCollectionKeyConnection(result: AbstractState[N, H, I], origCollection: Assignable)(keyCollection: Assignable) = {
        val newHeapAndSemantics = result._1.removeCollectionKeyConnection(origCollection, keyCollection)
        new AbstractState[N, H, I](newHeapAndSemantics, result._2)
      }
      var result = initialState
      keyCollection match {
        case id: Assignable => result = removeCollectionKeyConnection(result, origCollection)(id)
        case set: HeapIdSetDomain[I] => result = HeapIdSetFunctionalLifting.applyToSetHeapId(result.factory(), set, removeCollectionKeyConnection(result, origCollection))
      }

      result
    }

    var result = this.bottom()

    for (origCollection <- origCollectionSet.getSetOfExpressions;
        keysCollection <- keyCollectionSet.getSetOfExpressions) {
      val  newState = origCollection match {
        case id: Assignable => removeCollectionKeyConnection(this, keysCollection)(id)
        case set: HeapIdSetDomain[I] => HeapIdSetFunctionalLifting.applyToSetHeapId(this.factory(), set, removeCollectionKeyConnection(this, keysCollection))
      }

      result = result.lub(newState)
    }

    result.removeExpression()
  }

  def getCollectionLength(collectionSet: ExpressionSet): AbstractState[N, H, I] = {
    if (this.isBottom) return this

    var result = this.bottom()._1
    var heapId: HeapIdSetDomain[I] = null

    def getCollectionLength(id:Assignable):HeapIdSetDomain[I] = {
      val createdLocation = this._1._2.getCollectionLength(id)
      result = result.lub(new HeapAndAnotherDomain[N, H, I](this._1._1, this._1._2))
      heapId = heapId match {
        case null => createdLocation
        case _ => heapId.lub(createdLocation)
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
          if (this._1.isSummaryCollection(id)) return true
        case set: HeapIdSetDomain[I] =>
          for (id <- set.value) {
            if (this._1.isSummaryCollection(id)) return true
          }
        case _ => ()
      }
    }

    return false
  }

  /**
   * Removes all variables satisfying filter
   */
  def pruneVariables(filter:Identifier => Boolean) : AbstractState[N, H, I] = {

    var curState = this._1
    for (id <- this._1.getIds()) {
      id match {

        case va:Identifier =>
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

    val unreachable = this._1._2.getUnreachableHeap
    val pruned = pruneVariables({
      case a:I => unreachable.contains(a)
      case _ => false
    })

    // The cost of this technique is too damn high
    //pruned.optimizeSummaryNodes()

    pruned

  }

  def optimizeSummaryNodes() : AbstractState[N,H,I] = {
    val (state,replacement) = this._1.optimizeSummaryNodes()
    new AbstractState[N,H,I](state,this._2.merge(replacement))
  }

  override def lubWithReplacement(other: AbstractState[N, H, I]): (AbstractState[N, H, I], Replacement) = {
    if (this.isBottom) return (other, new Replacement())
    if (other.isBottom) return (this, new Replacement())
    val (d, rep) = this._1.lubWithReplacement(other._1)
    val s = this._2.lub(other._2)
    val result = this.factory(d, s.merge(rep))
    (result, rep)
  }

  override def lub(other: AbstractState[N, H, I]): AbstractState[N, H, I] = lubWithReplacement(other)._1

  override def glbWithReplacement(other: AbstractState[N, H, I]): (AbstractState[N, H, I], Replacement) = {
    if (this.isBottom || other.isBottom) return (bottom(), new Replacement())
    val (d, rep) = this._1.glbWithReplacement(other._1)
    val s = this._2.glb(other._2)
    val result = this.factory(d, s.merge(rep))
    (result, rep)
  }

  override def glb(other: AbstractState[N, H, I]): AbstractState[N, H, I] = glbWithReplacement(other)._1

  override def wideningWithReplacement(other: AbstractState[N, H, I]): (AbstractState[N, H, I], Replacement) = {
    if (this.isBottom) return (other, new Replacement())
    if (other.isBottom) return (this, new Replacement())
    val (d, rep) = _1.wideningWithReplacement(other._1)
    val s = this._2.widening(other._2)
    val result = this.factory(d, s.merge(rep))
    (result, rep)
  }

  override def widening(other: AbstractState[N, H, I]): AbstractState[N, H, I] = wideningWithReplacement(other)._1
}