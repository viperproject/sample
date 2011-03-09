package ch.ethz.inf.pm.sample.abstractdomain.waitorderinference;

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._

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


sealed abstract class SymbolicOrderValue {
  def factory() : SymbolicOrderValue; 
}

case class SymbolicObjectSharing[I <: HeapIdentifier[I]](val heapId : I, val pp : ProgramPoint) extends SymbolicOrderValue { 
  override def toString() = "Share of "+heapId.toString()+" at "+pp.toString();
  override def hashCode() = heapId.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case x : SymbolicObjectSharing[I] => heapId.equals(x.heapId) && pp.equals(x.pp);
    case _ => return false;
  }
  override def factory() : SymbolicOrderValue = new SymbolicObjectSharing(heapId, pp);
}


class SetSymbolicOrderValues extends SetDomain[SymbolicOrderValue, SetSymbolicOrderValues] { 
	def factory : SetSymbolicOrderValues = new SetSymbolicOrderValues();
}

abstract class Node {
	def containsId(id : Identifier) : Boolean;
}

case class AbstractObject[I <: HeapIdentifier[I]](var id : I, val path : Path) extends Node {
	//TODO: that's horrible!
	if(id.isInstanceOf[HeapIdAndSetDomain[ProgramPointHeapIdentifier]]) {
		if(id.asInstanceOf[HeapIdAndSetDomain[ProgramPointHeapIdentifier]].value.size==1)
			id=id.asInstanceOf[HeapIdAndSetDomain[ProgramPointHeapIdentifier]].value.iterator.next.asInstanceOf[I];
		else throw new WaitOrderInferenceException("Not yet supported");
	}
	
	
  override def toString() = id.toString()+" through path "+path.toString();
  override def hashCode() = id.hashCode();
  override def containsId(id : Identifier) : Boolean = this.id.equals(id);
  override def equals(a : Any) : Boolean = a match {
    case x : AbstractObject[I] => id.equals(x.id) && path.equals(x.path);
    case _ => return false;
  }
}

case class InitialLockLevel(val classe : String, val method : String) extends Node {
	if(method==null || classe==null) throw new WaitOrderInferenceException("This is not allowed");
  override def toString() = "maxlock("+classe.toString+"."+method.toString+")";
  override def hashCode() = classe.hashCode();
  override def containsId(id : Identifier) : Boolean = false;
  override def equals(a : Any) : Boolean = a match {
    case x : InitialLockLevel => classe.equals(x.classe) && method.equals(x.method) 
    case _ => return false;
  }
}

class CurrentWaitLevel[I <: HeapIdentifier[I]] extends InverseSetDomain[I, CurrentWaitLevel[I]] {
	override def factory() = new CurrentWaitLevel();
}

class WaitOrderDomain[I <: HeapIdentifier[I]](val rel : SymbolicOrderRelationshipsDomain[I], val waitlevel : CurrentWaitLevel[I]) extends CartesianProductDomain[SymbolicOrderRelationshipsDomain[I], CurrentWaitLevel[I], WaitOrderDomain[I]](rel, waitlevel) with SimplifiedSemanticDomain[WaitOrderDomain[I]] {
	override def removeVariable(variable : Identifier) : WaitOrderDomain[I] = new WaitOrderDomain[I](this._1.removeVariable(variable), this._2);
    
    override def createVariable(variable : Identifier, typ : Type) : WaitOrderDomain[I] =  new WaitOrderDomain(this._1.createVariable(variable, typ), this._2); 
	
    override def assume(expr : Expression) : WaitOrderDomain[I] = new WaitOrderDomain(this._1.assume(expr), this._2);

    override def assign(variable : Identifier, expr : Expression) : WaitOrderDomain[I] = new WaitOrderDomain(this._1.assign(variable, expr), this._2);

    override def setToTop(variable : Identifier) : WaitOrderDomain[I] = new WaitOrderDomain(this._1.setToTop(variable), this._2);

    override def getStringOfId(id : Identifier) : String = this._1.getStringOfId(id);
    
    override def factory() = new WaitOrderDomain[I](new SymbolicOrderRelationshipsDomain[I], new CurrentWaitLevel[I])
    
    def wait(id : I) : WaitOrderDomain[I] = new WaitOrderDomain(this._1, this._2.add(id))
    
    def release(id : I) : WaitOrderDomain[I] = new WaitOrderDomain(this._1, this._2.remove(id))
}

class SymbolicOrderRelationshipsDomain[I <: HeapIdentifier[I]] extends FunctionalDomain[(Node, Node), SetSymbolicOrderValues, SymbolicOrderRelationshipsDomain[I]] with SimplifiedSemanticDomain[SymbolicOrderRelationshipsDomain[I]]{
	
	def get(key : (Node, Node)) : SetSymbolicOrderValues = this.value.get(key) match {
		case Some(s) => return s;
		case None => return new SetSymbolicOrderValues();
	}
	
	def factory() : SymbolicOrderRelationshipsDomain[I] = new SymbolicOrderRelationshipsDomain();
	
	def removeVariable(variable : Identifier) : SymbolicOrderRelationshipsDomain[I] = variable match {
		case x : I => 
			if(this.isBottom) return this;
			var result : SymbolicOrderRelationshipsDomain[I] = new SymbolicOrderRelationshipsDomain[I]();
			for(key <- this.value.keySet)
				if(! key._1 .equals(x) && ! key._2.equals(x))
					result=result.add(key, this.get(key));
			return result;
		case _ => this;
	}
    
    def createVariable(variable : Identifier, typ : Type) : SymbolicOrderRelationshipsDomain[I] = this; 
	
    def assume(expr : Expression) : SymbolicOrderRelationshipsDomain[I] = this;

    def assign(variable : Identifier, expr : Expression) : SymbolicOrderRelationshipsDomain[I] = this;

    def setToTop(variable : Identifier) : SymbolicOrderRelationshipsDomain[I] = variable match {
		case x : I => 
			if(this.isBottom) return this;
			var result : SymbolicOrderRelationshipsDomain[I] = new SymbolicOrderRelationshipsDomain[I]();
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
			if(key._1.containsId(id) || key._2.containsId(id))
				result=result+" "+key._1 .toString+"<<"+key._2 .toString+":"+this.get(key).toString();
    	if(result.equals("")) return "no constraint"
    	else return result;
    }
}

class WaitOrderInferenceException(message : String) extends Exception(message)