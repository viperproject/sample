package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

trait RelationalNumericalDomain[
    T <: RelationalNumericalDomain[T]]
  extends NumericalDomain[T]
  with SimplifiedSemanticDomain[T] { this: T =>

	override def getStringOfId (id : Identifier) : String ;
	override def setToTop(variable : Identifier) : T;
	override def assign (variable : Identifier, expr : Expression) : T;
	override def assume(expr : Expression) : T;
	override def createVariable (variable : Identifier, typ : Type) : T = this.asInstanceOf[T]; //.setToTop(variable);
	override def removeVariable(variable : Identifier) : T = this.setToTop(variable);
}

case class UpperBoundRightPart(
    value: Set[Identifier] = Set.empty[Identifier],
    isTop: Boolean = false,
    isBottom: Boolean = false)
  extends InverseSetDomain[Identifier, UpperBoundRightPart] {

  def setFactory(
      value: Set[Identifier] = Set.empty[Identifier],
      isTop: Boolean = false,
      isBottom: Boolean = false) =
    UpperBoundRightPart(value, isTop, isBottom)
}

class UpperBound(val map:Map[Identifier, UpperBoundRightPart] = Map.empty[Identifier, UpperBoundRightPart],
                 val isBottom:Boolean = false,
                 val isTop:Boolean = false)
  extends BoxedDomain[UpperBoundRightPart, UpperBound]
  with RelationalNumericalDomain[UpperBound] {

  def functionalFactory(_value:Map[Identifier, UpperBoundRightPart] = Map.empty[Identifier, UpperBoundRightPart],
                        _isBottom:Boolean = false,
                        _isTop:Boolean = false) : UpperBound =
    new UpperBound(_value,_isBottom,_isTop)

  override def merge(r : Replacement) = if(r.isEmpty) this; else throw new SemanticException("Merge not yet implemented");

	override def get(key : Identifier) : UpperBoundRightPart = this.map.get(key) match {
		case Some(s) => s;
		case None => new UpperBoundRightPart();
	}
	
	override def factory() = new UpperBound();
		
	override def getStringOfId (id : Identifier) : String  = {
		var result : String = "";
		this.map.get(id) match {
			case Some(s) =>
				for(variable <- s.value)
					result=result+id.toString+" > "+variable.toString;
			case None =>
		}
		for(key <- this.map.keySet) {
			val vars = this.map.apply(key);
			if(vars.value.contains(id))
				result=result+key.toString+" > "+id.toString;
		}
		return result;
	}
	override def setToTop(id : Identifier) : UpperBound = {
		var result=this.map;
		result.get(id) match {
			case Some(s) =>
				result=result-id;
			case None =>
		}
		for(key <- result.keySet) {
			val vars = this.map.apply(key);
			if(vars.value.contains(id))
				result=result+((key, vars.remove(id)));
		}
		return new UpperBound(result);
	}
	override def assign (variable : Identifier, expr : Expression) : UpperBound = this.setToTop(variable);
	override def assume(expr : Expression) : UpperBound = this;
}

case class Pentagons(_1: BoxedNonRelationalNumericalDomain[Interval], _2: UpperBound)
  extends SemanticCartesianProductDomain[BoxedNonRelationalNumericalDomain[Interval], UpperBound, Pentagons]
  with RelationalNumericalDomain[Pentagons] {

  override def merge(r: Replacement) =
   if (r.isEmpty) this; else throw new SemanticException("Merge not yet implemented")

  override def getStringOfId(id: Identifier) =
    "Intervals:" + this._1.getStringOfId(id) + "\n\nUpper bound:\n" + this._2.getStringOfId(id)

  override def setToTop(variable: Identifier) =
    new Pentagons(this._1.setToTop(variable), this._2.setToTop(variable))

  override def assign(variable: Identifier, expr: Expression) =
    new Pentagons(this._1.assign(variable, expr), this._2.assign(variable, expr))

  override def assume(expr: Expression) =
    new Pentagons(this._1.assume(expr), this._2.assume(expr))

  override def factory(a: BoxedNonRelationalNumericalDomain[Interval], b: UpperBound) =
    new Pentagons(a, b)
}