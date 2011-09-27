package ch.ethz.inf.pm.sample.abstractdomain.typedomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class StaticTypeDomain(typ : Type) extends BoxedDomain[Type, StaticTypeDomain]() with SemanticDomain[StaticTypeDomain] {
  //def merge(f : Replacement) = if(f.isEmpty()) this else throw new TypeDomainException("Merging not yet supported");
  final override def factory() = new StaticTypeDomain(typ);
  override def assume(expr : Expression) : StaticTypeDomain = this
  override def assign(variable : Identifier, expr : Expression) : StaticTypeDomain = this
  def getType(variable : Identifier) = this.get(variable);
  override def createVariable(variable : Identifier, typ : Type) : StaticTypeDomain = this.add(variable, typ);
  override def createVariableForArgument(variable : Identifier, typ : Type, path : List[String]) = {
    var result = Map.empty[Identifier, List[String]];
    result=result+((variable, path ::: variable.toString() :: Nil));
    (this.createVariable(variable, typ), result);
  } 
    
  
  def get(key : Identifier) : Type = value.get(key) match {
    case None => typ.top()
    case Some(x) => x
  }
  
  override def removeVariable(variable : Identifier) : StaticTypeDomain = this.remove(variable);
  override def access(variable : Identifier) : StaticTypeDomain = this;
  override def backwardAccess(variable : Identifier) : StaticTypeDomain = throw new TypeDomainException("Backward analysis not yet supported");;
  override def backwardAssign(variable : Identifier, expr : Expression) : StaticTypeDomain= throw new TypeDomainException("Backward analysis not yet supported");;
                                                                                                                                                            
                                                                                                                                                                                                            
  def setToTop(variable : Identifier) : StaticTypeDomain=this;
  override def setArgument(variable : Identifier, expr : Expression) : StaticTypeDomain=this.assign(variable, expr)
  override def top() = this;//Static typing cannot be erased by method calls
}