package ch.ethz.inf.pm.sample.abstractdomain.typedomain
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._


class StaticAndDynamicTypeDomain(d1 : StaticTypeDomain, d2 : DynamicTypeDomain) extends ReducedSemanticProductDomain[StaticTypeDomain, DynamicTypeDomain, StaticAndDynamicTypeDomain](d1, d2) {
  override def factory() : StaticAndDynamicTypeDomain = new StaticAndDynamicTypeDomain(d1.top(), d2.top());  
  
  def getStringOfId(id : Identifier) : String = this.getType(id).toString();
  
  override def top() = super.top().reduce()

  def getType(variable : Identifier) : Type = this._1.getType(variable).glb(this._1.getType(variable), this._2.getType(variable));
  override def toString() : String = {
    var result : String = ""; 
    for(variable <- this._1.value.keySet ) {
      if(this._2.value.keySet.contains(variable))
        result = result+variable.toString() + "-> ("+this._1.get(variable)+", "+this._2.value.apply(variable)+"} )\n";
      else result = result+variable.toString() + "-> "+this._1.get(variable)+"\n";
    }
    result
  }
  def reduce() : StaticAndDynamicTypeDomain = {
    var result = this.factory();
    for(variable <- this._1.value.keySet ) {
      if(! this._2.value.keySet.contains(variable))
        result=new StaticAndDynamicTypeDomain(result._1.add(variable, this._1.get(variable)), result._2.add(variable, this._1.get(variable)))
      else
        result=new StaticAndDynamicTypeDomain(result._1.add(variable, this._1.get(variable)), result._2.add(variable, this._2.get(variable)))
    }
    this;
  }

}

class TypeAndNotInstanceOfDomain(d1 : StaticAndDynamicTypeDomain, d2 : NotInstanceOfDomain) extends ReducedSemanticProductDomain[StaticAndDynamicTypeDomain, NotInstanceOfDomain, TypeAndNotInstanceOfDomain](d1, d2) {
  override def factory() : TypeAndNotInstanceOfDomain = new TypeAndNotInstanceOfDomain(d1.top(), d2.top());  
  
  def getStringOfId(id : Identifier) : String = this.getType(id).toString();
  
  def getType(variable : Identifier) : Type = this._1.getType(variable);
  override def toString() : String = {
    var result : String = ""; 
    for(variable <- this._1._1.value.keySet ) {
      if(this._2.value.keySet.contains(variable))
        result = result+variable.toString() + "-> ("+this._1.getType(variable)+", { "+ToStringUtilities.setToString(this._2.value.apply(variable).value)+"} )\n";
      else result = result+variable.toString() + "-> "+this._1.getType(variable)+"\n";
    }
    for(variable <- this._2.value.keySet ) {
      if(! this._1._1.value.keySet.contains(variable))
        result = result+variable.toString() + "-> ( ! { "+ToStringUtilities.setToString(this._2.value.apply(variable).value)+"} )\n";
    }
    result
  }
  
  def reduce() : TypeAndNotInstanceOfDomain = {
    val result = new TypeAndNotInstanceOfDomain(this._1.reduce(), this._2)
    for(variable <- result._1._1.value.keySet ) {
      if(result._2.value.keySet.contains(variable)) {
        if(this._1.getType(variable).isBottomExcluding(this._2.value.get(variable).get.value))
           return this.bottom();
      	for(notType <- this._2.value.get(variable).get.value)
         if(this._1.getType(variable).lessEqual(notType))
           return this.bottom();
      }
    }
    this;
  }

}