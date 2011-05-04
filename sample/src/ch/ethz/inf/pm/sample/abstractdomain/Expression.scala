package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._

/** 
 * Arithmetic operators
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
object ArithmeticOperator extends Enumeration {
  val + = Value("+");
  val - = Value("-");
  val * = Value("*");
  val / = Value("/");
  val >= = Value(">=");
  val <= = Value("<=");
  val == = Value("==");
  val != = Value("!=");
  val > = Value(">");
  val < = Value("<");
}

/** 
 * Boolean operators
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
object BooleanOperator extends Enumeration {
  val && = Value("&&");
  val || = Value("||");
}


/** 
 * Abstract operators that can be used to represent any operations on different types of objects, like string concatenation and type casts
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
object AbstractOperatorIdentifiers extends Enumeration {
  val isInstanceOf = Value("isInstanceOf");
  val asInstanceOf = Value("asInstanceOf");
  val == = Value("==");
  val stringConcatenation = Value("StringConcat");
  val stringIndexof = Value("StringIndexof");
  val stringLastindexof = Value("StringLastindexof");
  val stringSubstring = Value("StringSubstring");
  val stringContains = Value("StringContains");
  val arrayApply= Value("arrayApply");
} 


/** 
 * Expressions are used to represents the values returned by statements. For instance, an assignment returns
 * a UnitExpression, while a variable access returns a VariableIdentifier and a field access returns a heap
 * identifier. Expressions represent the results of method calls and arithmetic and boolean operations as
 * well. 
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
abstract sealed class Expression(val p : ProgramPoint) {
  def getType() : Type;
  def getProgramPoint() : ProgramPoint = p;
}


/** 
 * Represents the negation of a given expression
 *  
 * @param thisExpr The negated expression
 * @author Pietro Ferrara
 * @since 0.1
 */
case class NegatedBooleanExpression(val thisExpr : Expression) extends Expression(thisExpr.getProgramPoint) {
  override def getType() = thisExpr.getType();
  override def hashCode() : Int = thisExpr.hashCode();
  override def equals(o : Any) = o match {
    case NegatedBooleanExpression(l) => thisExpr.equals(l) 
    case _ => false
  }
  override def toString() = "! " + thisExpr.toString()
}

/** 
 * Represents a generic operation, e.g., concatenation of strings. Usually, at source code level these
 * operations are encoded as method calls. 
 *  
 * @param thisExpr The object on which the expression is performed
 * @param parameters A (possibly empty) list of arguments
 * @param typeparameters A (possibly empty) list of generic types
 * @param op The identifier of the operation
 * @param returntyp The type of the value returned by the operation
 * @author Pietro Ferrara
 * @since 0.1
 */
case class AbstractOperator(val thisExpr : Expression, val parameters : Set[List[Expression]], val typeparameters : List[Type], val op : AbstractOperatorIdentifiers.Value, val returntyp : Type) extends Expression(thisExpr.getProgramPoint) {
  override def getType() = returntyp;
  override def hashCode() : Int = thisExpr.hashCode();
  override def equals(o : Any) = o match {
    case AbstractOperator(l, p, t, o, ty) => thisExpr.equals(l) && parameters.equals(p) && typeparameters.equals(t) & op.equals(o) 
    case _ => false
  }
  override def toString() = thisExpr.toString() + "." + op.toString() + ToStringUtilities.parametricTypesToString(typeparameters)+"("+ToStringUtilities.setOfListToString(parameters)+")"
}

/** 
 * A binary boolean expression, e.g. B1 && B2
 * 
 * @param left One of the operands
 * @param right The other operand
 * @param op The identifier of the operation
 * @param typ The type of the returned value
 * @author Pietro Ferrara
 * @since 0.1
 */
case class BinaryBooleanExpression(val left : Expression, val right : Expression, op : BooleanOperator.Value, val returntyp : Type) extends Expression(left.getProgramPoint) {
  override def getType() = returntyp;
  override def hashCode() : Int = left.hashCode();
  override def equals(o : Any) = o match {
    case BinaryBooleanExpression(l, r, o, ty) => left.equals(l) && right.equals(r) && op.equals(o) 
    case _ => false
  }
  override def toString() = left.toString() + op.toString() + right.toString()
}

/** 
 * A binary arithmetic expression, e.g. A1+A2 or A1>=A2
 * 
 * @param left One of the operands
 * @param right The other operand
 * @param op The identifier of the operation
 * @param typ The type of the returned value
 * @author Pietro Ferrara
 * @since 0.1
 */
case class BinaryArithmeticExpression(val left : Expression, val right : Expression, val op : ArithmeticOperator.Value, returntyp : Type) extends Expression(left.getProgramPoint) {
  override def getType() = returntyp;
  override def hashCode() : Int = left.hashCode();
  override def equals(o : Any) = o match {
    case BinaryArithmeticExpression(l, r, o, ty) => left.equals(l) && right.equals(r) && op.equals(o) 
    case _ => false
  }
  override def toString() = left.toString() + op.toString() + right.toString()
}

/** 
 * A unary arithmetic expression, e.g. -A1
 * 
 * @param left The operand
 * @param op The identifier of the operation
 * @param typ The type of the returned value
 * @author Pietro Ferrara
 * @since 0.1
 */
case class UnaryArithmeticExpression(val left : Expression, val op : ArithmeticOperator.Value, val returntyp : Type) extends Expression(left.getProgramPoint) {
  override def getType() = returntyp;
  override def hashCode() : Int = left.hashCode();
  override def equals(o : Any) = o match {
    case UnaryArithmeticExpression(l, o, ty) => left.equals(l) && op.equals(o)
    case _ => false
  }
  override def toString() = op.toString() + left.toString()
}

/** 
 * A (usually numeric) constant represented by a string 
 * 
 * @param constant The constant
 * @param typ The type of the constant
 * @author Pietro Ferrara
 * @since 0.1
 */
case class Constant(val constant : String, val typ : Type, pp : ProgramPoint) extends Expression(pp) {
  override def getType() = typ;
  override def hashCode() : Int = constant.hashCode();
  override def equals(o : Any) = o match {
    case Constant(c, t, pp) => constant.equals(c) && typ.equals(t)
    case _ => false
  }
  override def toString() = constant
}

/** 
 * An identifier, that could be a variable or a node of the abstract heap
 * 
 * @param typ The type of the identifier
 * @author Pietro Ferrara
 * @since 0.1
 */
abstract class Identifier(var typ : Type, pp : ProgramPoint) extends Expression(pp) {
	
  /**
   Returns the name of the identifier. We suppose that if two identifiers return the same name if and only
   if they are the same identifier
   @return The name of the identifier
   */
  def getName() : String;
  
  /**
   Returns the name of the field that is represented by this identifier if it is a heap identifier. 
   
   @return The name of the field pointed by this identifier
   */
  def getField() : Option[String];
  
  override def getType() : Type =  if(typ==null && SystemParameters.typ!=null) SystemParameters.typ.top(); else typ;
  
  /**
   Since an abstract identifier can be an abstract node of the heap, it can represent more than one concrete
   identifier. This function tells if a node is a summary node.  
   
   @return true iff this identifier represents exactly one variable
   */
  def representSingleVariable() : Boolean;
}

/** 
 * The identifier of a variable
 * 
 * @param name The name of the variable
 * @param typ1 The type of the variable
 * @author Pietro Ferrara
 * @since 0.1
 */
case class VariableIdentifier(var name : String, typ1 : Type, pp : ProgramPoint) extends Identifier(typ1, pp) {
	if(typ1==null) throw new Exception("The type of variables has to be specified");
  override def getName() = name.toString
  override def toString() = getName();
  override def getField() = None;
  override def hashCode() : Int = name.hashCode();
  
  //Variables always represent exactly one concrete identifier
  override def representSingleVariable()=true;
  
  override def equals(o : Any) = o match {
    case VariableIdentifier(n, t, pp) => name.equals(n) //&& typ.equals(t)
    case _ => false
  }
}

/** 
 * The heap identifier that has to be implemented by particular heap analyses
 * 
 * @param typ1 The type of the identifier
 * @author Pietro Ferrara
 * @since 0.1
 */
abstract case class HeapIdentifier[I <: HeapIdentifier[I]](typ1 : Type, pp : ProgramPoint) extends Identifier(typ1, pp)


/** 
 * The identifier of the length of an array
 * 
 * @param id The identifier of the array
 * @param typ The type (that is, integer)
 * @author Pietro Ferrara
 * @since 0.1
 */
class LengthArray(val id : Identifier, typ : Type) extends Identifier(typ, id.getProgramPoint) {
  def getName() : String = id.toString()+".length"
  def getField() : Option[String] = None;
  def representSingleVariable() = id.representSingleVariable();
  override def toString() : String = this.getName();
  override def equals(obj : Any) : Boolean = obj match {
	  case x : LengthArray => return x.id.equals(id);
	  case _ => return false;
  }
  override def hashCode() = 1;
}

/** 
 * The identifier of an array access
 * 
 * @param id The identifier of the array
 * @param index The index of the access
 * @param typ The type
 * @author Pietro Ferrara
 * @since 0.1
 */
class ArrayAccess(val id : Identifier, val index : Expression, typ : Type) extends Identifier(typ, id.getProgramPoint) {
  def getName() : String = id.toString()+"["+index.toString()+"]"
  def getField() : Option[String] = None;
  def representSingleVariable() = false //maybe id.representSingleVariable(); is more precise?
  override def toString() : String = this.getName();
  override def equals(obj : Any) : Boolean = obj match {
	  case x : ArrayAccess => return x.id.equals(id) && x.index.equals(index);
	  case _ => return false;
  }
  override def hashCode() = 1;
}

/** 
 * The identifier of the creation of an array
 * 
 * @param size The size of the array
 * @param typ The type of the value (e.g., Int[]) 
 * @author Pietro Ferrara
 * @since 0.1
 */
class ArrayCreation(val size : Expression, val typ1 : Type) extends Identifier(typ1, size.getProgramPoint) {
  def getName() : String = "new "+typ1.toString()+"("+size.toString()+")"
  def getField() : Option[String] = None;
  def representSingleVariable() = true;
  override def toString() : String = this.getName();
  override def equals(obj : Any) : Boolean = obj match {
	  case x : ArrayCreation => return x.size.equals(size);
	  case _ => return false;
  }
  override def hashCode() = 1;
}

/** 
 * The unit expression, that represents the absence of a concrete expression.
 * 
 * @param typ The unit type
 * @author Pietro Ferrara
 * @since 0.1
 */
case class UnitExpression(typ : Type, pp : ProgramPoint) extends Expression(pp) {
  override def hashCode() : Int = 0;
  override def getType() : Type = typ;
  override def equals(o : Any) = o match {
    case UnitExpression(t, pp) => true
    case _ => false
  }
  override def toString() = "Unit"
}

/** 
 * An helper object that perform some transformations to obtain simplified and standard numerical expressions.
 * 
 * @author Pietro Ferrara
 * @since 0.1
 */
object Normalizer {
  
  private def simplify(monomes : List[(Int, Identifier)]) : List[(Int, Identifier)] = monomes match {
    case Nil => Nil
    case (0, x) :: xs => simplify(xs)
    case (n, x) :: xs => 
      val (index1, monomes1) = simplifyVariable(monomes, x, n)
      return (index1, x) :: simplify(monomes)
  } 
  
  private def simplifyVariable(monomes : List[(Int, Identifier)], id : Identifier, index : Int) : (Int, List[(Int, Identifier)]) = monomes match {
    case Nil => (index, monomes)
    case (n, id1) :: xs if(id.equals(id1)) => simplifyVariable(xs, id, n+index)
    case (n, id1) :: xs => 
      val (index1, monomes1) = simplifyVariable(xs, id, index)
      return (index1, (n, id1) :: monomes1)
  } 
  
  /**
   Transforms the current expression to \sum a_i x_i + c >= 0  
   
   @param exp The conditional expression to be reduced to monomes
   @return  None if the given expression cannot be reduced to a linear form, Some(E, c) if it can be reduced to E+c>=0 (where E is \sum a_i x_i) 
   */
  def conditionalExpressionToMonomes(exp : Expression) : Option[(List[(Int, Identifier)], Int)] = exp match {
   
   case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
      op match {
        //! l>= r => l < r
        case ArithmeticOperator.>= => return conditionalExpressionToMonomes(BinaryArithmeticExpression(left, right, ArithmeticOperator.<, typ))
        //! l <= r => l > r
        case ArithmeticOperator.<= => return conditionalExpressionToMonomes(BinaryArithmeticExpression(left, right, ArithmeticOperator.>, typ))
        //! l > r => l <= r
        case ArithmeticOperator.> => return conditionalExpressionToMonomes(BinaryArithmeticExpression(left, right, ArithmeticOperator.<=, typ))
        //! l < r => l >= r
        case ArithmeticOperator.< => return conditionalExpressionToMonomes(BinaryArithmeticExpression(left, right, ArithmeticOperator.>=, typ))
        
        //== and != abstracted away
        case _ => None
      }
    
    case BinaryArithmeticExpression(left, right, op, typ) => 
      val l : Option[(List[(Int, Identifier)], Int)] = arithmeticExpressionToMonomes(left);
      val r : Option[(List[(Int, Identifier)], Int)] = arithmeticExpressionToMonomes(right);
      if(l.equals(None) || r.equals(None)) return None;
      op match {
        
        case ArithmeticOperator.>= =>
          return Some(compactOnTheLeft(l.get, r.get));
       
        //l <= r => r >= l
        case ArithmeticOperator.<= =>
          return Some(compactOnTheLeft(r.get, l.get));
        
        //l > r => l >= r+1
        case ArithmeticOperator.> =>
          val (lr, vr)=r.get
          return Some(compactOnTheLeft(l.get, (lr, vr+1)));
        
        //l < r => r >= l+1
        case ArithmeticOperator.< =>
          val (lr, vr)=l.get
          return Some(compactOnTheLeft(r.get, (lr, vr+1)));
         
        //== and != abstracted away
        case _ => None
      }
    case _ => None;
  }
    
  
  
  /**
   Transforms the current expression to \sum a_i x_i + c  
   
   @param exp The expression to be reduced to monomes
   @return  None if the given expression cannot be reduced to a linear form, Some(E, c) if it can be reduced to E+c (where E is \sum a_i x_i) 
   */
  def arithmeticExpressionToMonomes(exp : Expression) : Option[(List[(Int, Identifier)], Int)] = exp match {
    case BinaryArithmeticExpression(left, right, op, typ) => 
      val l : Option[(List[(Int, Identifier)], Int)] = arithmeticExpressionToMonomes(left);
      val r : Option[(List[(Int, Identifier)], Int)] = arithmeticExpressionToMonomes(right);
      if(l.equals(None) || r.equals(None)) return None;
      op match {
        case ArithmeticOperator.+ => return Some((l.get._1 ::: r.get._1, l.get._2+r.get._2))
        
        case ArithmeticOperator.- => return Some((l.get._1 ::: transform(r.get._1, (x : Int) => -x), l.get._2-r.get._2))
        
        case ArithmeticOperator.* =>
          if(r.get._1.equals(Nil)) return Some(transform(l.get._1, (x : Int)=> x*r.get._2), l.get._2*r.get._2);
          else if(l.get._1.equals(Nil)) return Some(transform(r.get._1, (x : Int)=> x*l.get._2), l.get._2*r.get._2);
          else return None;
        
        case ArithmeticOperator./ =>
          if(r.get._1.equals(Nil)) return Some(transform(l.get._1, (x : Int)=> x/r.get._2), l.get._2/r.get._2); 
          else return None;
        
      }
    
    case UnaryArithmeticExpression(left, op, typ) => 
      val l : Option[(List[(Int, Identifier)], Int)] = arithmeticExpressionToMonomes(left);
      if(l.equals(None)) return None;
      op match {
        case ArithmeticOperator.- => return Some(transform(l.get._1, (x : Int) => -x), -l.get._2)
        
        case _ => None
      }
      
    case Constant(c, t, pp) =>  return Some(Nil, Integer.valueOf(c).intValue())
    
    case UnitExpression(t, pp) => return None;
    
    case x : AbstractOperator => return None;
    
    case x : Identifier => return Some(((1, x.asInstanceOf[Identifier])::Nil, 0))
  }
  
  private def compactOnTheLeft(left : (List[(Int, Identifier)], Int), right : (List[(Int, Identifier)], Int)) : (List[(Int, Identifier)], Int) = (left._1 ::: transform(right._1, (x : Int)=> -x ), left._2-right._2) 
  
  private def transform(monome : List[(Int, Identifier)], f : Int => Int) : List[(Int, Identifier)] = monome match {
    case Nil => Nil;
    case (n, v) :: xs => (f(n), v) :: transform(xs, f)
  }
  
}