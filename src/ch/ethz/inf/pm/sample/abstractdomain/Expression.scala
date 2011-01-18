package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._

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

object BooleanOperator extends Enumeration {
  val && = Value("&&");
  val || = Value("||");
}

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

abstract sealed class Expression {
  def getType() : Type;
}

case class NegatedBooleanExpression(val thisExpr : Expression) extends Expression {
  override def getType() = thisExpr.getType();
  override def hashCode() : Int = thisExpr.hashCode();
  override def equals(o : Any) = o match {
    case NegatedBooleanExpression(l) => thisExpr.equals(l) 
    case _ => false
  }
  override def toString() = "! " + thisExpr.toString()
}

case class AbstractOperator(val thisExpr : Expression, val parameters : Set[List[Expression]], val typeparameters : List[Type], val op : AbstractOperatorIdentifiers.Value, val returntyp : Type) extends Expression {
  override def getType() = returntyp;
  override def hashCode() : Int = thisExpr.hashCode();
  override def equals(o : Any) = o match {
    case AbstractOperator(l, p, t, o, ty) => thisExpr.equals(l) && parameters.equals(p) && typeparameters.equals(t) & op.equals(o) 
    case _ => false
  }
  override def toString() = thisExpr.toString() + "." + op.toString() + ToStringUtilities.parametricTypesToString(typeparameters)+"("+ToStringUtilities.setOfListToString(parameters)+")"
}

case class BinaryBooleanExpression(val left : Expression, val right : Expression, op : BooleanOperator.Value, val returntyp : Type) extends Expression {
  override def getType() = returntyp;
  override def hashCode() : Int = left.hashCode();
  override def equals(o : Any) = o match {
    case BinaryBooleanExpression(l, r, o, ty) => left.equals(l) && right.equals(r) && op.equals(o) 
    case _ => false
  }
  override def toString() = left.toString() + op.toString() + right.toString()
}

case class BinaryArithmeticExpression(val left : Expression, val right : Expression, val op : ArithmeticOperator.Value, returntyp : Type) extends Expression {
  override def getType() = returntyp;
  override def hashCode() : Int = left.hashCode();
  override def equals(o : Any) = o match {
    case BinaryArithmeticExpression(l, r, o, ty) => left.equals(l) && right.equals(r) && op.equals(o) 
    case _ => false
  }
  override def toString() = left.toString() + op.toString() + right.toString()
}

case class UnaryArithmeticExpression(val left : Expression, val op : ArithmeticOperator.Value, val returntyp : Type) extends Expression {
  override def getType() = returntyp;
  override def hashCode() : Int = left.hashCode();
  override def equals(o : Any) = o match {
    case UnaryArithmeticExpression(l, o, ty) => left.equals(l) && op.equals(o)
    case _ => false
  }
  override def toString() = op.toString() + left.toString()
}

case class Constant(val constant : String, val typ : Type) extends Expression {
  override def getType() = typ;
  override def hashCode() : Int = constant.hashCode();
  override def equals(o : Any) = o match {
    case Constant(c, t) => constant.equals(c) && typ.equals(t) 
    case _ => false
  }
  override def toString() = constant
}

abstract class Identifier(var typ : Type) extends Expression {
  def getName() : String;
  def getField() : Option[String];
  override def getType() : Type = typ;
  def representSingleVariable() : Boolean;
}

case class VariableIdentifier(var name : String, typ1 : Type) extends Identifier(typ1) {
  override def getName() = name.toString
  override def toString() = getName();
  override def getField() = None;
  override def hashCode() : Int = name.hashCode();
  override def representSingleVariable()=true;
  override def equals(o : Any) = o match {
    case VariableIdentifier(n, t) => name.equals(n) //&& typ.equals(t) 
    case _ => false
  }
}

abstract case class HeapIdentifier[I <: HeapIdentifier[I]](typ1 : Type) extends Identifier(typ1) 

case class UnitExpression(typ : Type) extends Expression {
  override def hashCode() : Int = 0;
  override def getType() : Type = typ;
  override def equals(o : Any) = o match {
    case UnitExpression(t) => true 
    case _ => false
  }
  override def toString() = "Unit"
}

object Normalizer {
  
  def simplify(monomes : List[(Int, Identifier)]) : List[(Int, Identifier)] = monomes match {
    case Nil => Nil
    case (0, x) :: xs => simplify(xs)
    case (n, x) :: xs => 
      val (index1, monomes1) = simplifyVariable(monomes, x, n)
      return (index1, x) :: simplify(monomes)
  } 
  
  def simplifyVariable(monomes : List[(Int, Identifier)], id : Identifier, index : Int) : (Int, List[(Int, Identifier)]) = monomes match {
    case Nil => (index, monomes)
    case (n, id1) :: xs if(id.equals(id1)) => simplifyVariable(xs, id, n+index)
    case (n, id1) :: xs => 
      val (index1, monomes1) = simplifyVariable(xs, id, index)
      return (index1, (n, id1) :: monomes1)
  } 
  
  //It transform the current expression to \sum a_i x_i + c >= 0
  //It returns None if it cannot reduce the expression to such form)
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
    
  //It transform the current expression to \sum a_i x_i + c
  //It returns None if it cannot reduce the expression to such form)
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
      
    case Constant(c, t) =>  return Some(Nil, Integer.valueOf(c).intValue())
    
    case UnitExpression(t) => return None;
    
    case x : AbstractOperator => return None;
    
    case x : Identifier => return Some(((1, x.asInstanceOf[Identifier])::Nil, 0))
  }
  
  def compactOnTheLeft(left : (List[(Int, Identifier)], Int), right : (List[(Int, Identifier)], Int)) : (List[(Int, Identifier)], Int) = (left._1 ::: transform(right._1, (x : Int)=> -x ), left._2-right._2) 
  
  def transform(monome : List[(Int, Identifier)], f : Int => Int) : List[(Int, Identifier)] = monome match {
    case Nil => Nil;
    case (n, v) :: xs => (f(n), v) :: transform(xs, f)
  }
  
}