package ch.ethz.inf.pm.sample.abstractdomain.clientsideinference

trait SymbolicInt[T <: SymbolicInt[T, S], S <: SymbolicValue[S]] {

  def max(a : T, c : T) : T;
  def min(a : T, c : T) : T;
  def +(a : T, c : T) : T;
  def -(a : T, c : T) : T;
  def <=(a : T, b : T) : Boolean;
  def factory(s : S) : T;
  def factory(s : Int) : T;
}

class Coefficient[T <: SymbolicInt[T, S], S <: SymbolicValue[S]](d1 : Int, d2 : Int) extends (Int, Int)(d1, d2) with SymbolicInt[Coefficient[T, S], S] {

  def <=(a : Coefficient[T, S], b : Coefficient[T, S]) : Boolean = a._1 == b._1 && a._2==b._2
  def max(a : Coefficient[T, S], c : Coefficient[T, S]) = new Coefficient(a._1.min(c._1), a._2.max(c._2))
  def min(a : Coefficient[T, S], c : Coefficient[T, S]) = new Coefficient(a._1.min(c._1), a._2.max(c._2))
  def +(a : Coefficient[T, S], c : Coefficient[T, S]) = new Coefficient(a._1+c._1, a._2+c._2)
  def -(a : Coefficient[T, S], c : Coefficient[T, S]) = new Coefficient(a._1-c._1, a._2-c._2)
  def factory(s : S) : Coefficient[T, S] = throw new SymbolicIntException("Not supported");
  def factory(s : Int) : Coefficient[T, S] = new Coefficient(s, s)

  def isPositive() = d1>=0 && d2>=0;
  def isNegative() = d1<0 && d2<=0;

  override def equals(o : Any) = o match {
    case x : Coefficient[T, S] => x._1==this._1 && x._2==this._2
    case _ => false;
  }

  override def toString() = "("+_1+", "+_2+")"
}

class Summation[T <: SymbolicInt[T, S], S <: SymbolicValue[S]](val m : Map[S, Coefficient[T, S]]) extends SymbolicInt[Summation[T, S], S] {

  override def equals(o : Any) = o match {
    case x : Summation[T, S] => x.m.equals(m)
    case _ => false;
  }

  def factory(s : S) : Summation[T, S] = {
    val map : Map[S, Coefficient[T, S]] = Map[S, Coefficient[T, S]]((s, new Coefficient(1, 1)))
    new Summation[T, S](map);
  }
  def factory(s : Int) : Summation[T, S]= throw new SymbolicIntException("Not supported");

  def get(s : S) : Coefficient[T, S] = m.get(s) match {
    case Some(c) => c;
    case None => new Coefficient(0, 0);
  }
  def <=(a : Summation[T, S], c : Summation[T, S]) : Boolean = {
    for(val s : S <- a.m.keySet.++(c.m.keySet))
      if(! a.get(s).<=(a.get(s), c.get(s)))
        return false;
    return true;
  }
  def max(a : Summation[T, S], c : Summation[T, S]) : Summation[T, S] = {
    var result : Map[S, Coefficient[T, S]] = Map.empty;
    val coeff = new Coefficient[T, S](0, 0)
    val bothDomains = a.m.keySet.intersect(c.m.keySet);
    val onlyInA = a.m.keySet.--(c.m.keySet);
    val onlyInC = c.m.keySet.--(a.m.keySet);
    var remainingFromC = c.m.keySet.--(a.m.keySet);
    for(val s : S <- bothDomains)
      result=result+((s, coeff.max(a.get(s), c.get(s))));
    for(val el1 <- onlyInA) {
      var found : Boolean=false;
      for(val el2 <- onlyInC)
        if(!found && el1.<=(el1, el2) && a.get(el1).isPositive() && c.get(el2).isPositive()) {
          remainingFromC=remainingFromC.-(el2);
          result=result+((el2, coeff.max(a.get(el1), c.get(el2))));
          found=true;
        }
      if(!found)
        result=result+((el1, coeff.max(a.get(el1), c.get(el1))));
    }
    for(val el1 <- remainingFromC) {
      var found : Boolean=false;
      for(val el2 <- onlyInA)
        if(!found && el1.<=(el1, el2) && a.get(el1).isPositive() && c.get(el2).isPositive()) {
          result=result+((el2, coeff.max(a.get(el1), c.get(el2))));
          found=true;
        }
      if(!found)
        result=result+((el1, coeff.max(a.get(el1), c.get(el1))));
    }
    return new Summation(result);
  }

  def min(a : Summation[T, S], c : Summation[T, S]) : Summation[T, S] = {
    var result : Map[S, Coefficient[T, S]] = Map.empty;
    val coeff = new Coefficient[T, S](0, 0)
    val bothDomains = a.m.keySet.intersect(c.m.keySet);
    val onlyInA = a.m.keySet.--(c.m.keySet);
    val onlyInC = c.m.keySet.--(a.m.keySet);
    var remainingFromC = c.m.keySet.--(a.m.keySet);
    for(val s : S <- bothDomains)
      result=result+((s, coeff.min(a.get(s), c.get(s))));
    for(val el1 <- onlyInA) {
      var found : Boolean=false;
      for(val el2 <- onlyInC)
        if(!found && el1.<=(el1, el2) && a.get(el1).isPositive() && c.get(el2).isPositive()) {
          remainingFromC=remainingFromC.-(el2);
          result=result+((el1, coeff.min(a.get(el1), c.get(el2))));
          found=true;
        }
      if(!found)
        result=result+((el1, coeff.min(a.get(el1), c.get(el1))));
    }
    for(val el1 <- remainingFromC) {
      var found : Boolean=false;
      for(val el2 <- onlyInA)
        if(!found && el1.<=(el1, el2) && a.get(el1).isPositive() && c.get(el2).isPositive()) {
          result=result+((el1, coeff.min(a.get(el1), c.get(el2))));
          found=true;
        }
      if(!found)
        result=result+((el1, coeff.min(a.get(el1), c.get(el1))));
    }
    return new Summation(result);
  }

  def +(a : Summation[T, S], c : Summation[T, S]) : Summation[T, S] = this.iteratorOverMap(a, c, new Coefficient(0, 0).+(_, _))
  def -(a : Summation[T, S], c : Summation[T, S]) : Summation[T, S] = this.iteratorOverMap(a, c, new Coefficient(0, 0).-(_, _))

  private def iteratorOverMap(a : Summation[T, S], c : Summation[T, S], f : (Coefficient[T, S], Coefficient[T, S]) => Coefficient[T, S] ) : Summation[T, S] = {
    var result : Map[S, Coefficient[T, S]] = Map.empty;
    for(val s : S <- a.m.keySet.++(c.m.keySet))
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

class LinearSum[T <: SymbolicInt[T, S], S <: SymbolicValue[S]](val s : Summation[T, S], val c : Int) extends SymbolicInt[LinearSum[T, S], S] {
  def <=(a : LinearSum[T, S], b : LinearSum[T, S]) : Boolean = a.s.<=(a.s, b.s) && a.c <= b.c
  def max(a : LinearSum[T, S], b : LinearSum[T, S]) : LinearSum[T, S] = new LinearSum(a.s.max(a.s, b.s), a.c.max(b.c))
  def min(a : LinearSum[T, S], b : LinearSum[T, S]) : LinearSum[T, S] = new LinearSum(a.s.min(a.s, b.s), a.c.min(b.c))
  def +(a : LinearSum[T, S], b : LinearSum[T, S]) : LinearSum[T, S] = new LinearSum(a.s.+(a.s, b.s), a.c + b.c)
  def -(a : LinearSum[T, S], b : LinearSum[T, S]) : LinearSum[T, S] = new LinearSum(a.s.-(a.s, b.s), a.c - b.c)
  def factory(s1 : S) : LinearSum[T, S] = new LinearSum(s.factory(s1), 0)
  def factory(i : Int) : LinearSum[T, S] = new LinearSum(new Summation(Map.empty), i)

  override def equals(o : Any) = o match {
    case x : LinearSum[T, S] => x.s.equals(s) && c == x.c;
    case _ => false;
  }

  override def toString() = s.toString+" "+c;

}

class SymbolicIntException(s : String) extends Exception(s)