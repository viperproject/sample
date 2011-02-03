package ch.ethz.inf.pm.sample.abstractdomain.waitorderinference;

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type

class Path(val p : List[String]) {
  override def toString() : String = {
    var result : String ="";
    for(el <- p)
      if(result.equals("")) result=el;
      else result=result+"."+el;
    return result;
  }
  override def hashCode() = p.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case x : Path =>
      if(p.size!=x.p.size) return false;
      for(i <- 0 to p.size-1)
        if(! p.apply(i).equals(x.p.apply(i)))
          return false;
      return true;
    case _ => return false;
  } 
}


sealed abstract class SymbolicOrderValue(val from : Path, val to : Path) {
  override def toString() = from.toString + "<<" + to.toString;
  override def equals(a : Any) : Boolean = a match {
    case x : SymbolicOrderValue => from.equals(x.from) && to.equals(x.to);
    case _ => return false;
  }
  def factory() : SymbolicOrderValue; 
}

case class SymbolicOrderPreCondition(val className : String, val methodName : String, f : Path, t : Path) extends SymbolicOrderValue(f, t) { 
  override def toString() = "pre("+className.toString()+"."+methodName.toString()+", "+super.toString()+")";
  override def hashCode() = methodName.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case x : SymbolicOrderPreCondition => super.equals(x) && className.equals(x.className) && methodName.equals(x.methodName);
    case _ => return false;
  }
  override def factory() : SymbolicOrderValue = new SymbolicOrderPreCondition(className, methodName, from, to);
}

case class SymbolicOrderPostCondition(val className : String, val methodName : String, f : Path, t : Path) extends SymbolicOrderValue(f, t) { 
  override def toString() = "post("+className.toString()+"."+methodName.toString()+", "+super.toString()+")";
  override def hashCode() = methodName.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case x : SymbolicOrderPreCondition => super.equals(x) && className.equals(x.className) && methodName.equals(x.methodName) 
    case _ => return false;
  }
  override def factory() : SymbolicOrderValue = new SymbolicOrderPreCondition(className, methodName, from, to);
}


class SetSymbolicOrderValues extends SetDomain[SymbolicOrderValue, SetSymbolicOrderValues] { 
	def factory : SetSymbolicOrderValues = new SetSymbolicOrderValues();
}

abstract class Node

case class AbstractObject[I <: HeapIdentifier[I]](val id : I) {
  override def toString() = id.toString();
  override def hashCode() = id.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case x : AbstractObject[I] => id.equals(x.id) 
    case _ => return false;
  }
}

case class MaxlockLevel(val classe : String, val method : String) {
  override def toString() = "maxlock("+classe.toString+"."+method.toString+")";
  override def hashCode() = classe.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case x : MaxlockLevel => classe.equals(x.classe) && method.equals(x.method) 
    case _ => return false;
  }
}

class WaitOrderDomain[I <: HeapIdentifier[I]] extends FunctionalDomain[(Node, Node), SetSymbolicOrderValues, WaitOrderDomain[I]] with SimplifiedSemanticDomain[WaitOrderDomain[I]]{
	
	def get(key : (Node, Node)) : SetSymbolicOrderValues = this.value.get(key) match {
		case Some(s) => return s;
		case None => return new SetSymbolicOrderValues();
	}
	
	def factory() : WaitOrderDomain[I] = new WaitOrderDomain();
	
	def removeVariable(variable : Identifier) : WaitOrderDomain[I] = variable match {
		case x : I => 
			if(this.isBottom) return this;
			var result : WaitOrderDomain[I] = new WaitOrderDomain[I]();
			for(key <- this.value.keySet)
				if(! key._1 .equals(x) && ! key._2.equals(x))
					result=result.add(key, this.get(key));
			return result;
		case _ => this;
	}
    
    def createVariable(variable : Identifier, typ : Type) : WaitOrderDomain[I] = this; 
	
    def assume(expr : Expression) : WaitOrderDomain[I] = this;

    def assign(variable : Identifier, expr : Expression) : WaitOrderDomain[I] = this;

    def setToTop(variable : Identifier) : WaitOrderDomain[I] = variable match {
		case x : I => 
			if(this.isBottom) return this;
			var result : WaitOrderDomain[I] = new WaitOrderDomain[I]();
			for(key <- this.value.keySet)
				if(! key._1 .equals(x) && ! key._2.equals(x))
					result=result.add(key, this.get(key));
				else result=result.add(key, new SetSymbolicOrderValues().top());
			return result;
		case _ => this;
	}

    def getStringOfId(id : Identifier) : String = {
    	var result = "";
		for(key <- this.value.keySet)
			if(key._1 .equals(id) && key._2.equals(id))
				result=result+" "+key._1 .toString+"<<"+key._2 .toString+":"+this.get(key).toString();
    	if(result.equals("")) return "no constraint"
    	else return result;
    }
}