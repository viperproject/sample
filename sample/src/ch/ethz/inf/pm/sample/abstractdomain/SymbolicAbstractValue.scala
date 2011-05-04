package ch.ethz.inf.pm.sample.abstractdomain
import ch.ethz.inf.pm.sample.oorepresentation._

/** 
 * The representation of an expression inside an abstract state of the analysis. Since we could
 * have to consider several different expressions (e.g., because of nondeterministic statements)
 * that are produced with different abstract states. We abstract it with a function that relates
 * each expression to the abstract state in which the expression was generated. 
 *
 * @param <T> The type of the abstract state
 * @author Pietro Ferrara
 * @since 0.1
 */
class SymbolicAbstractValue[S <: State[S]]() extends FunctionalDomain[Expression, S, SymbolicAbstractValue[S]]() {
	
  //These fields are optional ONLY for the begin of the analysis. In fact, in order to create a new state we need
  //an instance of a SymbolicAbstractValue, and to create such instance we need an abstract state. So we decided
  //to set to None the abstract state just at the beginning, and then to replace it with a consistent value.
  private var st : Option[S]=None;
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
  
  private def top(ty : Type) : SymbolicAbstractValue[S]=new SymbolicAbstractValue[S](st, Some(ty));
  
  override def top() = new SymbolicAbstractValue[S](st, typ);
  
  /**
   * Returns true iff this SymbolicAbstractValue could represent ANY expression
   */
  def isTop() = value.equals(Map.empty[Expression, S]);
  
  /**
   * Negates the expressions contained in the object
   * @return A SymbolicAbstractValue in which all the expressions have been negated
   */
  def not() : SymbolicAbstractValue[S] = {
    var result : SymbolicAbstractValue[S] = this.factory();
    for(key <- value.keySet)
      result=result.add(new NegatedBooleanExpression(key), value.get(key).get);
    result;
  }
  
  /**
   * Returns all expressions contained in the current SymbolicAbstractValue
   */ 
  def getExpressions() = value.keySet;
  
  /**
   * Tries to return an instance of the Type class. If it fails, it throws a SymbolicSemanticException 
   * @return A random instance of the current Type class
   */
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
 
  /**
   * A helper function that creates an expression representing a variable  
   * @param variable The variable
   * @param state The abstract state when the expression is created
   * @param ty The type of the variable 
   * @return A SymbolicAbstractValue containing [variable -> state]
   */
 def createVariable[W <: State[W]](variable : Variable, state : W, ty : Type, pp : ProgramPoint): SymbolicAbstractValue[S]= {
    var result = new SymbolicAbstractValue[S](this.st, Some(ty));
    result=result.add(new VariableIdentifier(variable.getName(), ty, pp), state.asInstanceOf[S]);
    return result;
  }
 
  /**
   * A helper function that creates a binary expression  
   * @param left The left operand
   * @param right The right operand
   * @param op The operator
   * @param state The abstract state when the expression is created
   * @param ty The type of the value returned by the expression 
   * @return A SymbolicAbstractValue containing [left <op> right -> state]
   */
  def createBinaryExpression[W <: State[W]](left : SymbolicAbstractValue[S], right : SymbolicAbstractValue[S], op : ArithmeticOperator.Value, state : W, ty : Type): SymbolicAbstractValue[S]= {
    var result = new SymbolicAbstractValue[S](this.st, Some(ty));
    for(expleft <- left.value)
      for(expright <- right.value)
        result=result.add(new BinaryArithmeticExpression(expleft._1, expright._1, op, ty), state.asInstanceOf[S]);
    return result;
  }
  
  /**
   * A helper function that creates a boolean binary expression  
   * @param left The left operand
   * @param right The right operand
   * @param op The operator
   * @param state The abstract state when the expression is created
   * @param ty The type of the value returned by the expression 
   * @return A SymbolicAbstractValue containing [left <op> right -> state]
   */
  def createBooleanBinaryExpression[W <: State[W]](left : SymbolicAbstractValue[S], right : SymbolicAbstractValue[S], op : BooleanOperator.Value, state : W, ty : Type): SymbolicAbstractValue[S]= {
    var result = new SymbolicAbstractValue[S](this.st, Some(ty));
    for(expleft <- left.value)
      for(expright <- right.value)
        result=result.add(new BinaryBooleanExpression(expleft._1, expright._1, op, ty), state.asInstanceOf[S]);
    return result;
  }
  
  /**
   * A helper function that creates a unary expression  
   * @param v The operand
   * @param op The operator
   * @param state The abstract state when the expression is created
   * @param ty The type of the value returned by the expression 
   * @return A SymbolicAbstractValue containing [<op> v -> state]
   */
  def createUnaryExpression[W <: State[W]](v : SymbolicAbstractValue[S], op : ArithmeticOperator.Value, state : W, ty : Type): SymbolicAbstractValue[S]= {
    var result = new SymbolicAbstractValue[S](this.st, Some(ty));
    for(expleft <- v.value)
        result=result.add(new UnaryArithmeticExpression(expleft._1, op, ty), state.asInstanceOf[S]);
    return result;
  }
  
   /**
   * A helper function that creates an expression representing a generic abstract operation  
   * @param thisExpr The expression on which the operations is called
   * @param parameters All the parameters of the operation
   * @param typeParameters The generic types
   * @param op The operator
   * @param state The abstract state when the expression is created
   * @param ty The type of the value returned by the expression 
   * @return A SymbolicAbstractValue containing [this.op<typeParameter>(parameters) -> state]
   */
  def createAbstractOperator[W <: State[W]](thisExpr : SymbolicAbstractValue[S], parameters : List[SymbolicAbstractValue[S]], typeParameters : List[Type], op : AbstractOperatorIdentifiers.Value, state : W, ty : Type) : SymbolicAbstractValue[S] = {
    var result = new SymbolicAbstractValue[S](this.st, Some(ty));
    val combination = combineListValue(parameters); 
    for(thisexp <- thisExpr.value)
      result=result.add(new AbstractOperator(thisexp._1, combination, typeParameters, op, ty), state.asInstanceOf[S]);
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