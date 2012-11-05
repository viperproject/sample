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
  val % = Value("%");
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
  val ==> = Value("==>");
  val <==> = Value("<==>");
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
abstract class Expression(val p : ProgramPoint) {
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

case class FalseExpression(val pp : ProgramPoint, val returntyp : Type) extends Expression(pp) {
  override def getType() = returntyp;
  override def hashCode() : Int = 0;
  override def equals(o : Any) = o match {
    case FalseExpression(pp, ty) => pp.equals(this.getProgramPoint())
    case _ => false
  }
  override def toString() = "false"
}

case class TrueExpression(val pp : ProgramPoint, val returntyp : Type) extends Expression(pp) {
  override def getType() = returntyp;
  override def hashCode() : Int = 0;
  override def equals(o : Any) = o match {
    case TrueExpression(pp, ty) => pp.equals(this.getProgramPoint())
    case _ => false
  }
  override def toString() = "true"
}

/**
 * A comparison between reference, that is, left==right or left!=right
 *
 * @param left One of the operands
 * @param right The other operand
 * @param op The identifier of the operation
 * @param typ The type of the returned value
 * @author Pietro Ferrara
 * @since 0.1
 */
case class ReferenceComparisonExpression(val left : Expression, val right : Expression, val op : ArithmeticOperator.Value, returntyp : Type) extends Expression(left.getProgramPoint) {
  override def getType() = returntyp;
  override def hashCode() : Int = left.hashCode();
  override def equals(o : Any) = o match {
    case ReferenceComparisonExpression(l, r, o, ty) => left.equals(l) && right.equals(r) && op.equals(o)
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
abstract class Identifier(var typ : Type, pp : ProgramPoint) extends Expression(pp) with Assignable {
	
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
abstract case class HeapIdentifier[I <: HeapIdentifier[I]](typ1 : Type, val pp : ProgramPoint) extends Identifier(typ1, pp)

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
 * Eliminate nondeterministic expressions before calling this
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
      // TODO: Because x != null is treated as arithmetic and it crashes with NumberFormatException (because of null)
      if (left == null || right == null || left.getType() == null || right.getType() == null ||
        !left.getType.isNumericalType || !right.getType.isNumericalType)
        return None

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
  def arithmeticExpressionToMonomes[I <: HeapIdentifier[I]](exp : Expression) : Option[(List[(Int, Identifier)], Int)] = exp match {
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

        case _ => None
        
      }
    
    case UnaryArithmeticExpression(left, op, typ) => 
      val l : Option[(List[(Int, Identifier)], Int)] = arithmeticExpressionToMonomes(left);
      if(l.equals(None)) return None;
      op match {
        case ArithmeticOperator.- => return Some(transform(l.get._1, (x : Int) => -x), -l.get._2)
        
        case _ => None
      }
      
    case Constant(c, t, pp) =>  try {
      Some(Nil, Integer.valueOf(c).intValue())
    } catch {
      case e:NumberFormatException => None
    }
    
    case UnitExpression(t, pp) => return None;
    
    case x : AbstractOperator => return None;
    
    case x : Identifier => return Some(((1, x)::Nil, 0))

    case x : HeapIdSetDomain[I] =>
      if(x.value.size!=1) return None;
        else return Some(((1, x.value.iterator.next())::Nil, 0))
  }

  private def compactOnTheLeft(left : (List[(Int, Identifier)], Int), right : (List[(Int, Identifier)], Int)) : (List[(Int, Identifier)], Int) = (left._1 ::: transform(right._1, (x : Int)=> -x ), left._2-right._2) 
  
  private def transform(monome : List[(Int, Identifier)], f : Int => Int) : List[(Int, Identifier)] = monome match {
    case Nil => Nil;
    case (n, v) :: xs => (f(n), v) :: transform(xs, f)
  }


  /**
   * This methods normalizes an arithmetic expression to a form c1*v+c2 where c1 and c2 are constants and v is an identifier.
   * If such a normalization is not possible, null is returned.
   * E.g. 3x+2+x will be normalized to 4x+2 and x+y+1 will return null.
   *
   * @param exp is an expression to be normalized
   * @return None if the expression can not be normalized, otherwise it returns Some(normExp) represeting exp in the form c1*v+c2 where c1 and c2 are constants, v is an identifier and exp == normExp == c1*v+c2.
   */
  def normalizeToCoefVarCost(exp : Expression): Option[Expression] = {
    exp match {
      case x: Constant => {
        return Some(exp);
      }
      case _ => {
        Normalizer.arithmeticExpressionToMonomes(exp) match {
          case None => {
            return None;
          }
          case Some((monomes, const)) => {
            val constExp = new Constant(const.toString, exp.getType(), exp.getProgramPoint());
            monomes.length match {
              case 0 => {
                return Some(constExp);
              }
                // TODO: this should be reimplemented - ugly
              case 1 => {
                var result : BinaryArithmeticExpression = null;
                for ((coef, id) <- monomes) {
                  val coefExp = new Constant(coef.toString, exp.getType(), exp.getProgramPoint());
                  val coefAndVarExp : BinaryArithmeticExpression = new BinaryArithmeticExpression(coefExp, id, ArithmeticOperator.*, exp.getType());
                  result = new BinaryArithmeticExpression(coefAndVarExp, constExp, ArithmeticOperator.+, exp.getType());
                }
                return Some(result);
              }
              case _ => {
                return None;
              }
            }
          }
		    }
      }
    }
	}

  /**
   * This methods returns if a given expression contains the given id. Two ids are the same if they have the same name.
   *
   * @param exp is an expression to check
   * @param id is an Identifier that we should check
   * @return true if id among identifiers in exp, false otherwise
   */
  def contains[I <: HeapIdentifier[I]](exp: Expression, id: Identifier): Boolean = exp match {

    /*
     * ARITHMETIC EXPRESSIONS
     */
    case BinaryArithmeticExpression(left, right, op, typ) => return contains[I](left, id) || contains[I](left, id);

    case UnaryArithmeticExpression(left, op, typ) => return contains[I](left, id);

    case Constant(c, t, pp) => return false;

    case UnitExpression(t, pp) => return false;

    case x : AbstractOperator => return false;

    // I assume that the identifiers are the same if they have the same name.
    case x : Identifier => return id.getName().equals(x.getName());

    case x : HeapIdSetDomain[I] => {
      if (id.isInstanceOf[I]) {
        return x.value.contains(id.asInstanceOf[I]);
      } else {
        return false;
      }
    }

    /*
     * BOOLEAN EXPRESSIONS
     */

    case BinaryBooleanExpression(left, right, op, typ) => return contains[I](left, id) || contains[I](left, id);

    case NegatedBooleanExpression(x) => return contains[I](x, id);

    /*
     * REFERENCE EXPRESSIONS
     */

    case ReferenceComparisonExpression(left, right, op, typ) => return contains[I](left, id) || contains[I](right, id);

    /*
    * NONDETERMINISTIC EXPRESSIONS
    */

    case BinaryNondeterministicExpression(left, right, op, typ) => return contains[I](left, id) || contains[I](right, id);

    case _ => return false;
  }

  /**
   * This methods substitutes every occurrence of id in exp for subExp. There is no renaming ids.
   * E.g. substitute(3x+1, x, y+1) == 3(y+1)+1
   *      substitute(3x+1, x, x+y) == 3(x+y)+1
   *      substitute(3x+1, y, y+1) == 3x+1
   *
   * @param exp is an expression to which we want to substitute id
   * @param id is an id for which we want to substitute subExp in exp
   * @param is an expression that we want to substitute exp for id
   * @return an expression in which every id in exp is substituted with subExp
   */
  def substitute[I <: HeapIdentifier[I]](exp: Expression, id: Identifier, subExp: Expression) : Expression = {
    if (exp.getType().equals(subExp.getType())) {
      throw new Exception("Can not substitute an expression of different type to an expression.");
    }

    if (contains[I](exp, id)) {
      exp match {
        /*
         * ARITHMETIC EXPRESSIONS
         */
        case BinaryArithmeticExpression(left, right, op, typ) => {
          return new BinaryArithmeticExpression(substitute[I](left, id, subExp), substitute[I](right, id, subExp), op, typ)
        }

        case UnaryArithmeticExpression(left, op, typ) => return substitute[I](left, id, subExp);

        case Constant(c, t, pp) => return exp;

        case UnitExpression(t, pp) => return exp;

        case x : AbstractOperator => return exp;

        // I assume that the identifiers are the same if they have the same name.
        case x : Identifier => {
          if (x.getName().equals(id.getName())) {
            return subExp;
          } else {
            return exp;
          }
        }

        case x : HeapIdSetDomain[I] => {
          if (exp.isInstanceOf[I] && id.isInstanceOf[I] && x.value.contains(id.asInstanceOf[I])) {
            return x.remove(id.asInstanceOf[I]).add(exp.asInstanceOf[I]);
          } else {
            return exp;
          }
        }

        /*
         * BOOLEAN EXPRESSIONS
         */

        case BinaryBooleanExpression(left, right, op, typ) => {
          return new BinaryBooleanExpression(substitute[I](left, id, subExp), substitute[I](right, id, subExp), op, typ)
        }

        case NegatedBooleanExpression(x) => return substitute[I](x, id, subExp);

        /*
         * REFERENCE EXPRESSIONS
         */

        case ReferenceComparisonExpression(left, right, op, typ) => {
          return new ReferenceComparisonExpression(substitute[I](left, id, subExp), substitute[I](right, id, subExp), op, typ)
        }

        /*
        * NONDETERMINISTIC EXPRESSIONS
        */

        case BinaryNondeterministicExpression(left, right, op, typ) => {
          return new BinaryNondeterministicExpression(substitute[I](left, id, subExp), substitute[I](right, id, subExp), op, typ)
        }

        case _ => throw new Exception("Can not substitute " + subExp.toString + " for " + id.getName() + " in " + exp.toString);
      }
    } else {
      return exp;
    }
  }

  /**
   * This methods returns all the identifiers (variable identifiers, heapIds, ...) that occur in the given expression exp.
   * E.g.: When exp=3x+4z+2 then getIdsForExpression(exp) will return {x,y}
   *
   * @param exp is the expression from which we want all the identifiers
   * @return a set of identifiers that occur in the given expression
   */
  def getIdsForExpression[I <: HeapIdentifier[I]](exp: Expression): Set[Identifier] = exp match {

    /*
     * ARITHMETIC EXPRESSIONS
     */
    case BinaryArithmeticExpression(left, right, op, typ) => return getIdsForExpression[I](left).union(getIdsForExpression[I](right));

    case UnaryArithmeticExpression(left, op, typ) => return getIdsForExpression[I](left);

    case Constant(c, t, pp) => return Set.empty[Identifier];

    case UnitExpression(t, pp) => return Set.empty[Identifier];

    case x : AbstractOperator => return Set.empty[Identifier];

    // I assume that the identifiers are the same if they have the same name.
    case x : Identifier => return Set.empty[Identifier].+(x);

    case x : HeapIdSetDomain[I] => return x.value.asInstanceOf[Set[Identifier]];

    /*
     * BOOLEAN EXPRESSIONS
     */

    case BinaryBooleanExpression(left, right, op, typ) => return getIdsForExpression[I](left).union(getIdsForExpression[I](right));

    case NegatedBooleanExpression(x) => return getIdsForExpression[I](x);

    /*
     * REFERENCE EXPRESSIONS
     */

    case ReferenceComparisonExpression(left, right, op, typ) => return getIdsForExpression[I](left).union(getIdsForExpression[I](right));

    /*
     * NONDETERMINISTIC EXPRESSIONS
     */

    case BinaryNondeterministicExpression(left,right,op,typ) => return getIdsForExpression[I](left).union(getIdsForExpression[I](right));

    case _ => return Set.empty[Identifier];
  }
}

/**
 * Implements non-deterministic operations. These can be used to represent non-deterministic expressions on the
 * stack of the state, e.g. the result of a math.random call.
 *
 * Note that not every numerical domain has direct support for this.
 *
 * @author Lucas Brutschy
 *
 */
object NondeterministicOperator extends Enumeration {

  /**
   * Represents a nondeterministic choice between two expressions, e.g (a+3) or (b+3) or (invalid)
   */
  val or = Value("or")

  /**
   * Represents a nondeterministic choice in a given range (interval), e.g. 1 to (x+y)
   */
  val to = Value("to")

}

/**
 *
 * Represents an expression with a nondeterministic operator.
 *
 * @author Lucas Brutschy
 *
 */
case class BinaryNondeterministicExpression(left : Expression, right : Expression, op : NondeterministicOperator.Value, returnType : Type) extends Expression(left.getProgramPoint()) {

  override def getType() = returnType

  override def hashCode() : Int = left.hashCode()

  override def equals(o : Any) = o match {
    case BinaryNondeterministicExpression(l, r, op2, ty) => left.equals(l) && right.equals(r) && op.equals(op2)
    case _ => false
  }

  override def toString = left.toString + " " + op.toString + " " + right.toString

}
