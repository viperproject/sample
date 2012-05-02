package project

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type

/**
 * Created by IntelliJ IDEA.
 * User: Pietro
 * Date: 29/03/12
 * Time: 18.03
 * To change this template use File | Settings | File Templates.
 */

abstract class AVPDomain[V <: Lattice[V], T <: AVPDomain[V, T]] extends SimplifiedSemanticDomain[T] with BoxedDomain[V, T]{
  def set(variable : Identifier, value : V) : T = {
    var result = this.factory();
    result.value=this.value+((variable, value));
    result.isBottom=false;
    return result;
  }
  override def add(variable : Identifier, value : V) : T = this.set(variable, value);
  override def setToTop(variable : Identifier) : T = throw new SemanticException("Not supported");
  override def createVariable(variable : Identifier, typ : Type) = this.asInstanceOf[T];
  override def removeVariable(variable : Identifier) : T = throw new SemanticException("Not supported");
  override def getIds() : Set[Identifier] = this.value.keySet.toSet;

  def bottomValue() : V;

  override def get(id : Identifier) : V = value.get(id) match {
    case Some(s) => s;
    case None => this.bottomValue();
  }

  override def setArgument(variable : Identifier, expr : Expression) : T = super.setArgument(variable, expr);
  override def createVariableForArgument(variable : Identifier, typ : Type, path : List[String]) : (T, Map[Identifier, List[String]]) =
    super.createVariableForArgument(variable, typ, path)

  override def access(field : Identifier) : T = super.access(field);
  override def backwardAccess(field : Identifier) : T = super.backwardAccess(field);
  override def backwardAssign(variable : Identifier, expr : Expression) : T = super.backwardAssign(variable, expr);


  override def merge(r : Replacement) : T = super.merge(r);
  override def clone() : T = super.clone();

  override def bottom() : T = super.bottom();
  override def top() : T = super.top();
  override def lub(left : T, right : T) : T = super.lub(left, right);
  override def glb(left : T, right : T) : T = super.glb(left, right);
  override def widening(left : T, right : T) : T = super.widening(left, right);
  override def lessEqual(r : T) : Boolean = super.lessEqual(r);

}