package ch.ethz.inf.pm.sample.abstractdomain.arrayanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class LengthArray(val id : Identifier, typ : Type) extends Identifier(typ) {
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

class ArrayAccess(val id : Identifier, val index : Expression, typ : Type) extends Identifier(typ) {
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

class ArrayCreation(val size : Expression, val typ1 : Type) extends Identifier(typ1) {
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

object ArrayNativeMethodSemantics extends NativeMethodSemantics {
	
	def applyForwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] = thisExpr.getType().toString() match {
		case "Array" => operator match {
		      case "this" => parameters match {
		        case x :: Nil =>
		        	if(thisExpr.getExpressions().size != 1) throw new ArrayAnalysisException("This is not yet supported!");
		        	if(! thisExpr.getExpressions().iterator.next.isInstanceOf[Identifier]) throw new ArrayAnalysisException("This is not yet supported!");
		        	//val id : Identifier = thisExpr.getExpressions().iterator.next.asInstanceOf[Identifier];
		        	var result = state.bottom(); 
		        	for(exp <- x.getExpressions) {
		        		val st = x.get(exp);
		        		result=result.lub(result, state.setExpression(new SymbolicAbstractValue(new ArrayCreation(exp, thisExpr.getType()), state)));
		        	}
		        	//val arrayLength = new SymbolicAbstractValue(new LengthArray(), state)
		        	//val tempResult=result.assignVariable(x, x)
		        	return Some(result);
		      }
		      case "update" => parameters match {
		     	case index :: value :: Nil => 
		        	if(thisExpr.getExpressions().size != 1) throw new ArrayAnalysisException("This is not yet supported!");
		        	if(! thisExpr.getExpressions().iterator.next.isInstanceOf[Identifier]) throw new ArrayAnalysisException("This is not yet supported!");
		        	val id : Identifier = thisExpr.getExpressions().iterator.next.asInstanceOf[Identifier];
		        	var result = state.bottom(); 
		        	for(ind <- index.getExpressions) {
		        		for(valu <- value.getExpressions()) {
		        			val st = value.get(valu);
		        			result=result.lub(result, state.assignVariable(new SymbolicAbstractValue(new ArrayAccess(id, ind, thisExpr.getType()), result), value))
		        		}
		        	}
		        	return Some(result);
		      }
		      case "apply" => parameters match {
		     	case index :: Nil => 
		        	if(thisExpr.getExpressions().size != 1) throw new ArrayAnalysisException("This is not yet supported!");
		        	if(! thisExpr.getExpressions().iterator.next.isInstanceOf[Identifier]) throw new ArrayAnalysisException("This is not yet supported!");
		        	val id : Identifier = thisExpr.getExpressions().iterator.next.asInstanceOf[Identifier];
		        	var result = state.bottom(); 
		        	for(exp <- index.getExpressions) {
		        		val st = index.get(exp);
		        		result=result.lub(result, state.setExpression(new SymbolicAbstractValue(new ArrayAccess(id, exp, thisExpr.getType()), state)));
		        	}
		        	return Some(result);
		      }
		      case "length" => parameters match {
		     	  case Nil => 
		        	if(thisExpr.getExpressions().size != 1) throw new ArrayAnalysisException("This is not yet supported!");
		        	if(! thisExpr.getExpressions().iterator.next.isInstanceOf[Identifier]) throw new ArrayAnalysisException("This is not yet supported!");
		        	val id : Identifier = thisExpr.getExpressions().iterator.next.asInstanceOf[Identifier];
		        	return Some(state.setExpression(new SymbolicAbstractValue(new LengthArray(id, returnedtype), state)));
		      }
		      case _ => return None
		}
		case _ => return None;
	}
	
	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] = None
}

class ArrayAnalysisException(s : String) extends Exception(s)