package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

import java.io.StringWriter
import java.lang.Exception

trait SymbolicInt[T <: SymbolicInt[T]] {

  def max(a : T, c : T) : T;
  def min(a : T, c : T) : T;
  def +(a : T, c : T) : T;
  def -(a : T, c : T) : T;
  def <=(a : T, b : T) : Boolean;
  def factory(s : SymbolicValue) : T;
  def factory(s : Int) : T;
}

class Coefficient(d1 : Int, d2 : Int) extends (Int, Int)(d1, d2) with SymbolicInt[Coefficient] {

  def <=(a : Coefficient, b : Coefficient) : Boolean = a._1 == b._1 && a._2==b._2
  def max(a : Coefficient, c : Coefficient) = new Coefficient(a._1.min(c._1), a._2.max(c._2))
  def min(a : Coefficient, c : Coefficient) = new Coefficient(a._1.min(c._1), a._2.max(c._2))
  def +(a : Coefficient, c : Coefficient) = new Coefficient(a._1+c._1, a._2+c._2)
  def -(a : Coefficient, c : Coefficient) = new Coefficient(a._1-c._1, a._2-c._2)
  def factory(s : SymbolicValue) : Coefficient = throw new SymbolicIntException("Not supported");
  def factory(s : Int) : Coefficient = new Coefficient(s, s)

  override def equals(o : Any) = o match {
    case x : Coefficient => x._1==this._1 && x._2==this._2
    case _ => false;
  }

  override def toString() = "("+_1+", "+_2+")"
}

class Summation(val m : Map[SymbolicValue, Coefficient]) extends SymbolicInt[Summation] {


  override def equals(o : Any) = o match {
    case x : Summation => x.m.equals(m)
    case _ => false;
  }

  def factory(s : SymbolicValue) : Summation = new Summation(Map.empty+((s, new Coefficient(0, 1))));
  def factory(s : Int) : Summation= throw new SymbolicIntException("Not supported");

  def get(s : SymbolicValue) : Coefficient = m.get(s) match {
    case Some(c) => c;
    case None => new Coefficient(0, 0);
  }
  def <=(a : Summation, c : Summation) : Boolean = {
    for(val s : SymbolicValue <- a.m.keySet.++(c.m.keySet))
      if(! a.get(s).<=(a.get(s), c.get(s)))
        return false;
    return true;
  }
  def max(a : Summation, c : Summation) : Summation = this.iteratorOverMap(a, c, new Coefficient(0, 0).max(_, _))
  def min(a : Summation, c : Summation) : Summation = this.iteratorOverMap(a, c, new Coefficient(0, 0).min(_, _))
  def +(a : Summation, c : Summation) : Summation = this.iteratorOverMap(a, c, new Coefficient(0, 0).+(_, _))
  def -(a : Summation, c : Summation) : Summation = this.iteratorOverMap(a, c, new Coefficient(0, 0).-(_, _))

  private def iteratorOverMap(a : Summation, c : Summation, f : (Coefficient, Coefficient) => Coefficient ) : Summation = {
    var result : Map[SymbolicValue, Coefficient] = Map.empty;
    for(val s : SymbolicValue <- a.m.keySet.++(c.m.keySet))
      result=result+((s, f(a.get(s), c.get(s))));
    return new Summation(result);
  }
  override def toString() = {
    var result="";
    for(s <- m.keySet)
      result=result+" "+m.apply(s)+"*"+s+" + ";
    result;
  }
}

class LinearSum(val s : Summation, val c : Int) extends SymbolicInt[LinearSum] {
  def <=(a : LinearSum, b : LinearSum) : Boolean = a.s.<=(a.s, b.s) && a.c <= b.c
  def max(a : LinearSum, b : LinearSum) : LinearSum = new LinearSum(a.s.max(a.s, b.s), a.c.max(b.c))
  def min(a : LinearSum, b : LinearSum) : LinearSum = new LinearSum(a.s.min(a.s, b.s), a.c.min(b.c))
  def +(a : LinearSum, b : LinearSum) : LinearSum = new LinearSum(a.s.+(a.s, b.s), a.c + b.c)
  def -(a : LinearSum, b : LinearSum) : LinearSum = new LinearSum(a.s.-(a.s, b.s), a.c - b.c)
  def factory(s1 : SymbolicValue) : LinearSum = new LinearSum(s.factory(s1), 0)
  def factory(i : Int) : LinearSum = new LinearSum(new Summation(Map.empty), i)

  override def equals(o : Any) = o match {
    case x : LinearSum => x.s.equals(s) && c == x.c;
    case _ => false;
  }

  override def toString() = s.toString+" "+c;

}

class SymbolicIntException(s : String) extends Exception(s)