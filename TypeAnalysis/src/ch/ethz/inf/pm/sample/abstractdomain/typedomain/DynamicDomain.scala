package ch.ethz.inf.pm.sample.abstractdomain.typedomain
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class DynamicTypeDomain(typ : Type) extends BoxedDomain[Type, DynamicTypeDomain]() with SemanticDomain[DynamicTypeDomain] {
  //def merge(f : Replacement) = if(f.isEmpty()) this else throw new TypeDomainException("Merging not yet supported");
  final override def factory() = new DynamicTypeDomain(typ);
  override def assume(expr : Expression) : DynamicTypeDomain = expr match { 
    case AbstractOperator(t, parameters, typeparameters, AbstractOperatorIdentifiers.isInstanceOf, typ) =>
      t match {
        case t1 : VariableIdentifier =>
          typeparameters match {
            case x :: Nil => return this.add(t1, x);
            case _ => return this
          }
        case _ => return this;
      }
    case AbstractOperator(left, args, typepars,AbstractOperatorIdentifiers.==, typ) =>
      if(args.size != 1)
        return this;
      for(arg <- args) {
    	  arg match {
    	  	case x :: Nil if (x.isInstanceOf[Identifier]) => 
    	  	  if(left.getType().isStatic==false) return this;
    	  	  else return this.add(x.asInstanceOf[Identifier], left.getType());
    	  	case _ => return this;
      	  }
       }
      return this;
    /*case NotExpression(AbstractOperator(t, parameters, typeparameters, AbstractOperatorIdentifiers.isInstanceOf, typ)) =>
      t match {
        case t1 : VariableIdentifier =>
           typeparameters match {
            case x :: Nil => 
              if(this.get(t1).lessEqual(x)) 
                return this.bottom;
             else return this;
            case _ => return this
          }
        case _ => return this;
      }*/
    case _ => return this;
  }
  
  def get(key : Identifier) : Type = value.get(key) match {
    case None => typ.top()
    case Some(x) => x
  }
                        
  override def assign(variable : Identifier, expr : Expression) : DynamicTypeDomain = expr match {
    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.asInstanceOf, typ) =>
	    if(variable.representSingleVariable)
	    	this.add(variable, typeparameters.apply(0));
	    else this.add(variable, this.get(variable).lub(this.get(variable), typeparameters.apply(0)))
    case _ =>
        if(variable.representSingleVariable)
        	this.add(variable, expr.getType());
        else this.add(variable, this.get(variable).lub(this.get(variable), expr.getType()))
  }
  def getType(variable : Identifier) = this.get(variable);
  override def createVariable(variable : Identifier, typ : Type) : DynamicTypeDomain = this.add(variable, typ);
  
  override def createVariableForParameter(variable : Identifier, typ : Type, path : List[String]) = {
    var result = Map.empty[Identifier, List[String]];
    result=result+((variable, path ::: variable.toString() :: Nil));
    (this.createVariable(variable, typ), result);
  }
  
  override def removeVariable(variable : Identifier) : DynamicTypeDomain = this.remove(variable);
  
  override def access(variable : Identifier) : DynamicTypeDomain = this;
  override def backwardAccess(variable : Identifier) : DynamicTypeDomain = throw new TypeDomainException("Backward analysis not yet supported");;
  override def backwardAssign(variable : Identifier, expr : Expression) : DynamicTypeDomain = throw new TypeDomainException("Backward analysis not yet supported");;
                                                                                                                                                            
  def setToTop(variable : Identifier) : DynamicTypeDomain=this.remove(variable);
  def setParameter(variable : Identifier, expr : Expression) : DynamicTypeDomain=this.assign(variable, expr)

           
}

class TypeDomainException(message : String) extends Exception(message)