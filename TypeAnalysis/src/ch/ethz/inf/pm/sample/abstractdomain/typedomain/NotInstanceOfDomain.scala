package ch.ethz.inf.pm.sample.abstractdomain.typedomain
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._


class SetExcludedTypes extends SetDomain[Type, SetExcludedTypes] {
  override def factory() : SetExcludedTypes = new SetExcludedTypes();

  override def add(el : Type) : SetExcludedTypes = {
    var result = factory();
    result.value=value+el;
    normalize(result);
  }
  
  override def lub(left : SetExcludedTypes, right : SetExcludedTypes) : SetExcludedTypes = {
    if(left.isTop || right.isTop) return top();
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    val result : SetExcludedTypes = this.factory()
    result.value=left.value**right.value;
    normalize(result)
  }
  
  override def glb(left : SetExcludedTypes, right : SetExcludedTypes) : SetExcludedTypes = {
    if(left.isBottom || right.isBottom) return bottom();
    if(left.isTop) return right;
    if(right.isTop) return left;
    val result : SetExcludedTypes = this.factory()
    result.value=left.value++right.value;
    normalize(result)
  }
  
  override def lessEqual(right : SetExcludedTypes) : Boolean = {
    if(this.isBottom) return true;
    if(right.isTop) return true;
    normalize(right).value.subsetOf(normalize(this).value)
  }
  
  private def normalize(domain : SetExcludedTypes) : SetExcludedTypes = {
    var result : Set[Type] = Set.empty[Type];
    for(el1 <- domain.value) {
      var toBeAdded : Boolean = true;
      for(el2 <- domain.value) {
        if(! el1.equals(el2))
          if(el1.lessEqual(el2))
            toBeAdded=false;
      }
      if(toBeAdded)
        result=result+el1;
    }
    val returned = new SetExcludedTypes();
    returned.value=result;
    returned
  }  
  
}

class NotInstanceOfDomain(typ : Type) extends BoxedDomain[SetExcludedTypes, NotInstanceOfDomain]() with SemanticDomain[NotInstanceOfDomain] {

  def merge(f : Replacement) = if(f.isEmpty()) this else throw new TypeDomainException("Merging not yet supported");

  override def factory() : NotInstanceOfDomain = new NotInstanceOfDomain(typ);

  def assume(expr : Expression) : NotInstanceOfDomain = expr match {
    case NegatedBooleanExpression(AbstractOperator(e, pars, typepars,AbstractOperatorIdentifiers.isInstanceOf, typ)) =>
      e match {
        case t : VariableIdentifier =>
          typepars match {
            case x :: Nil => this.value.get(t) match {
              case None => val toBeAdded = this.get(t);
              				return this.add(t, toBeAdded.add(x));
              case Some(c) => return this.add(t, c.add(x));
            }
            case _ => return this
          }
        case _ => return this;
    }
    
    case NegatedBooleanExpression(AbstractOperator(left, args, typepars,AbstractOperatorIdentifiers.==, typ)) =>
      if(args.size != 1)
        return this;
      for(arg <- args) {
    	  arg match {
    	  	case x :: Nil if (x.isInstanceOf[Identifier]) => 
    	  	  if(left.getType().isStatic==false) return this;
    	  	  val toBeAdded = this.get(x.asInstanceOf[Identifier]);
              return this.add(x.asInstanceOf[Identifier], toBeAdded.add(left.getType()));
    	  	case _ => return this;
      	  }
       }
      return this;
    case _ => return this;
  }
  
  def setToTop(variable : Identifier) : NotInstanceOfDomain = this.remove(variable)
  def removeVariable(variable : Identifier) : NotInstanceOfDomain = this.remove(variable)
  
  def get(key : Identifier) : SetExcludedTypes = value.get(key) match {
    case None => new SetExcludedTypes().top()
    case Some(x) => x
  }
  
 def assign(variable : Identifier, expr : Expression) : NotInstanceOfDomain=this; //TODO : I should refine it for assignments like x=y
 def setParameter(variable : Identifier, expr : Expression) : NotInstanceOfDomain=this;
 
 def createVariable(variable : Identifier, typ : Type) : NotInstanceOfDomain=this;
 def createVariableForParameter(variable : Identifier, typ : Type, path : List[String]) ={
    var result = Map.empty[Identifier, List[String]];
    result=result+((variable, path ::: variable.toString() :: Nil));
    (this, result);
  }
 override def access(field : Identifier) : NotInstanceOfDomain=this;
 override def backwardAccess(field : Identifier) : NotInstanceOfDomain=throw new TypeDomainException("Backward analysis not yet supported");
 override def backwardAssign(variable : Identifier, expr : Expression) : NotInstanceOfDomain=throw new TypeDomainException("Backward analysis not yet supported");
                                                                                              
}
