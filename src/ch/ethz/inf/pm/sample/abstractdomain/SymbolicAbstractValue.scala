package ch.ethz.inf.pm.sample.abstractdomain
import ch.ethz.inf.pm.sample.oorepresentation._

class SymbolicAbstractValue[S <: State[S]]() extends FunctionalDomain[Expression, S, SymbolicAbstractValue[S]]() {
  var st : Option[S]=None;
  var typ : Option[Type] = None;
  
  def this(s : Option[S], t : Option[Type])  =  {
    this();
    this.st=s match {
      case None => None;
      case _ => s;
      }
    this.typ=t;
  } 
  
  def this(e : Expression, s : S)  =  {
    this(Some(s), Some(e.getType));
    this.value=this.value+((e, s))
  } 
  
  def get(key : Expression) : S = value.get(key) match {
    case None => st match {
      case Some(l) => l.top()
      case None => throw new StandardDomainException("Codomain not yet instantiated");
    }
    case Some(x) => x
  }
  
  def top(ty : Type) : SymbolicAbstractValue[S]=new SymbolicAbstractValue[S](st, Some(ty));
  
  override def top() = new SymbolicAbstractValue[S](st, typ);
  
  def isTop() = value.equals(Map.empty[Expression, S]);
  
  def not() : SymbolicAbstractValue[S] = {
    var result : SymbolicAbstractValue[S] = this.factory();
    for(key <- value.keySet)
      result=result.add(new NegatedBooleanExpression(key), value.get(key).get);
    result;
  }
  
  def getExpressions() = value.keySet;
  
 def getType[S](state : S) : Type = typ match {
   case Some(t) => return t;
   case None =>
	    if(this.isBottom)
	      throw new SymbolicSemanticException("Type not defined");
	    if(this.value.size>=1) {
		    var result : Type = this.value.elements.next._1.getType();
		    var first : Boolean = true;
		    for(el <- this.value.elements)
		      if(first) {
		        first=false;
		        result=el._1.getType;
		      }
		      else result=result.lub(result, el._1.getType);
		    return result;
	    }
	    else throw new SymbolicSemanticException("Type of 0 expressions not defined");
  }
  
 def createVariable[W <: State[W]](variable : Variable, state : W, ty : Type): SymbolicAbstractValue[S]= {
    var result = new SymbolicAbstractValue[S](this.st, Some(ty));
    result=result.add(new VariableIdentifier(variable.getName(), ty), state.asInstanceOf[S]);
    return result;
  }
 
  def createBinaryExpression[W <: State[W]](left : SymbolicAbstractValue[S], right : SymbolicAbstractValue[S], op : ArithmeticOperator.Value, state : W, ty : Type): SymbolicAbstractValue[S]= {
    var result = new SymbolicAbstractValue[S](this.st, Some(ty));
    for(expleft <- left.value)
      for(expright <- right.value)
        result=result.add(new BinaryArithmeticExpression(expleft._1, expright._1, op, ty), state.asInstanceOf[S]);
    return result;
  }
  
  def createBooleanBinaryExpression[W <: State[W]](left : SymbolicAbstractValue[S], right : SymbolicAbstractValue[S], op : BooleanOperator.Value, state : W, ty : Type): SymbolicAbstractValue[S]= {
    var result = new SymbolicAbstractValue[S](this.st, Some(ty));
    for(expleft <- left.value)
      for(expright <- right.value)
        result=result.add(new BinaryBooleanExpression(expleft._1, expright._1, op, ty), state.asInstanceOf[S]);
    return result;
  }
  
  def createUnaryExpression[W <: State[W]](left : SymbolicAbstractValue[S], op : ArithmeticOperator.Value, state : W, ty : Type): SymbolicAbstractValue[S]= {
    var result = new SymbolicAbstractValue[S](this.st, Some(ty));
    for(expleft <- left.value)
        result=result.add(new UnaryArithmeticExpression(expleft._1, op, ty), state.asInstanceOf[S]);
    return result;
  }
  
  def createAbstractOperator[W <: State[W]](thisExpr : SymbolicAbstractValue[S], parameters : List[SymbolicAbstractValue[S]], typeParameters : List[Type], method : AbstractOperatorIdentifiers.Value, state : W, ty : Type) : SymbolicAbstractValue[S] = {
    var result = new SymbolicAbstractValue[S](this.st, Some(ty));
    val combination = combineListValue(parameters); 
    for(thisexp <- thisExpr.value)
      result=result.add(new AbstractOperator(thisexp._1, combination, typeParameters, method, ty), state.asInstanceOf[S]);
    return result;
  }
  
  private def combineListValue[S <: State[S]](list : List[SymbolicAbstractValue[S]]) : Set[List[Expression]] = list match {
    case Nil => Set.empty+(Nil)
    case x :: xs => 
      val previous : Set[List[Expression]] = combineListValue(xs);
      var result : Set[List[Expression]] = Set.empty;
      for(expr <- x.value)
        for(l <- previous)
          result = result + (expr._1 :: l);
      result;
  }
  
  override def toString() = {
    var result : String = "";
    for(key <- value.keySet)
      result=result+key.toString+" -> <state>\n";
    result
  }
  
  final override def factory() : SymbolicAbstractValue[S] = new SymbolicAbstractValue(this.st, typ);
}