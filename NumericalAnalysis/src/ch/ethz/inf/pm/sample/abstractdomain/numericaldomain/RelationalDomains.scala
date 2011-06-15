package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

trait RelationalNumericalDomain[T <: RelationalNumericalDomain[T]] extends NumericalDomain[T] {
	override def getStringOfId (id : Identifier) : String ;
	override def setToTop(variable : Identifier) : T;
	override def assign (variable : Identifier, expr : Expression) : T;
	override def assume(expr : Expression) : T;
	override def createVariable (variable : Identifier, typ : Type) : T = this.asInstanceOf[T]; //.setToTop(variable);
	override def removeVariable(variable : Identifier) : T = this.setToTop(variable);
}

class UpperBoundRightPart extends InverseSetDomain[Identifier, UpperBoundRightPart] {
	override def factory() = new UpperBoundRightPart();
}

class UpperBound extends RelationalNumericalDomain[UpperBound] with BoxedDomain[UpperBoundRightPart, UpperBound] {
  //def getIds : Set[Identifier] = this.value.keySet.asInstanceOf[Set[Identifier]];

  override def merge(r : Replacement) = if(r.isEmpty) this; else throw new SemanticException("Merge not yet implemented");
	
	def this(v : Map[Identifier, UpperBoundRightPart]) {
		this();
		this.value=v;
	}
	
	override def get(key : Identifier) : UpperBoundRightPart = this.value.get(key) match {
		case Some(s) => s;
		case None => new UpperBoundRightPart();
	}
	
	override def factory() = new UpperBound();
		
	override def getStringOfId (id : Identifier) : String  = {
		var result : String = "";
		this.value.get(id) match {
			case Some(s) =>
				for(variable <- s.value)
					result=result+id.toString()+" > "+variable.toString();
			case None =>
		}
		for(key <- this.value.keySet) {
			val vars = this.value.apply(key);
			if(vars.value.contains(id))
				result=result+key.toString()+" > "+id.toString();
		}
		return result;
	}
	override def setToTop(id : Identifier) : UpperBound = {
		var result=this.value;
		result.get(id) match {
			case Some(s) =>
				result=result-id;
			case None =>
		}
		for(key <- result.keySet) {
			val vars = this.value.apply(key);
			if(vars.value.contains(id))
				result=result+((key, vars.remove(id)));
		}
		return new UpperBound(result);
	}
	override def assign (variable : Identifier, expr : Expression) : UpperBound = this.setToTop(variable);
	override def assume(expr : Expression) : UpperBound = this;
}

class Pentagons(l : BoxedNonRelationalNumericalDomain[Interval], r : UpperBound) extends SemanticCartesianProductDomain[BoxedNonRelationalNumericalDomain[Interval], UpperBound, Pentagons](l, r) with RelationalNumericalDomain[Pentagons] {
  override def merge(r : Replacement) = if(r.isEmpty) this; else throw new SemanticException("Merge not yet implemented");
	override def getStringOfId (id : Identifier) : String = "Intervals:"+this._1.getStringOfId(id)+"\n\nUpper bound:\n"+this._2.getStringOfId(id)
	override def setToTop(variable : Identifier) : Pentagons = new Pentagons(this._1.setToTop(variable), this._2.setToTop(variable));
	override def assign (variable : Identifier, expr : Expression) : Pentagons = new Pentagons(this._1.assign(variable,expr), this._2.assign(variable, expr));
	override def assume(expr : Expression) : Pentagons = new Pentagons(this._1.assume(expr), this._2.assume(expr));;
	override def factory() : Pentagons = new Pentagons(l.factory(), r.factory()) ;
}