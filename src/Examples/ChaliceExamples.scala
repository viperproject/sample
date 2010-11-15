package Examples;
import Chalice._;


class Pippo {
  var x=0;
  def Foo() : Int = return x;
}

/**
 * The following examples are taken from:
 * K. R. M. Leino and P. Mueller and J. Smans: Verification of Concurrent Programs with Chalice
 * Foundations of Security Analysis and Design V, 2009.
 */

/*
/** Fig. 1 */
class C100 {
	var x : Int=0;
 	var y : Int=0;
 
 	def Main() = {
 	  var c = new C100();
 	  c.x=7;
 	  var tmp = c.y;
 	  c.Inc(this, 2);
 	  c.x = c.y+10;
 	  c.Dispose();
 	}
  
 	//Requires acc(x)
 	//Ensures acc(x)
 	def Inc(a : C100, b: Int) = x=x+1
  
 	//Requires acc(x) && acc(y)
 	def Dispose() = {}
}

/** Fig. 2 */
class Math(var n : Int) {
  
  //Requires acc(n)
  //Ensures acc(n)
  def ISqrt() = {
    var N = n;
    n = 0;
    //Invariant acc(n)
    while((n+1)*(n+1)<=N)
    	n=n+1;
  }
}

/** Fig. 3 */
class RockBand {
  var memberCount : Int = 0;
  //Monitor invariant acc(memberCount)
  
  //Requires acc(memberCount)
  //Ensures acc(memberCount)
  def Init() = memberCount=0;
  
  //Requires acc(memberCount)
  //Ensures acc(memberCount)
  def AddMembers(howMany : Int) = memberCount=memberCount+howMany;
  
}

/** Fig. 4 */
class Program {
  def Main() {
    var acdc = new RockBand();
    acdc.Init();
    acdc.AddMembers(5);
    
    var noDoubt = new RockBand();
    noDoubt.Init();
    noDoubt.AddMembers(4);
  } 
}

/** Fig. 5 */
class ParallelMath {
  def TwoSqrts(x : Int, y : Int) : (Int, Int) = {
    var m0 = new Math(x);
    var m1 = new Math(y);
    Chalice.fork("tk0", m0.ISqrt());
    Chalice.fork("tk1", m1.ISqrt());
    Chalice.join("tk0");
    Chalice.join("tk1");
    return (m0.n, m1.n);
  }
}

/** Fig. 6 */
class VideoRental {
  var customerId : Int = 0;
  var movieId : Int = 0;
  var days : Int = 0;
  
  //Requires rd(customerId) && rd(movieId)
  //Ensures rd(customerId) && rd(movieId)
  def FrequentRentalPoints() : Int = 0;
  
  
  //Requires rd(customerId) && rd(movieId)
  //Ensures rd(customerId) && rd(movieId)
  def Invoice() : Int = 0;
  
}

class VideoStore {
  
  //Requires acc(vr.customerId) && acc(vr.moveiId) && acc(vr.days)
  def Charge(vr : VideoRental) {
    Chalice.fork("tk0", vr.FrequentRentalPoints());
    Chalice.fork("tk1", vr.Invoice);
    var p = Chalice.join("tk0");
    var r = Chalice.join("tk1");
  }
}


/** Fig. 13 */
class Node {
  var value : Int = 0;
  var left : Node = null;
  var right : Node = null;
  
  //Predicate isTree acc(value) && acc(left) && acc(right)
  
  //Requires acc(this.*)
  //Ensures isTree
  def Init() = {
    left = null;
    right = null;
    Chalice.fold(this, "isTree");
  }
  
  //Requires isTree
  //Ensures isTree  
  def Sum() : Int = {
    var tmp : Int = 0;
    Chalice.unfold(this, "isTree");
    var total : Int = value;
    if(left!=null) Chalice.fork("tk", left.Sum());
    if(right!=null) {
      val tmp = right.Sum();
      total=total+tmp;
    }
    if(left!=null) {
      val tmp=Chalice.join("tk").asInstanceOf[Int];
      total=total+tmp; 
    }
    Chalice.fold(this, "isTree");
    return total;
  } 
}
                      
*/