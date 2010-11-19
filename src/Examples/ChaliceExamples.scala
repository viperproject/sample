package Examples;
import Chalice._;

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
 	def Dispose() = {Chalice.free(this)}
}
/** Fig. 2 */
class Math(var n : Int) {
  
  //Requires acc(n)
  //Ensures acc(n)
  def ISqrt() : Unit = {
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
    Chalice.fork(m0.ISqrt(), "tk0");
    Chalice.fork(m1.ISqrt(), "tk1");
    Chalice.join(m0.ISqrt(), "tk0");
    Chalice.join(m1.ISqrt(), "tk1");
    return (m0.n, m1.n);
  }
}

*/
/** Fig. 6 */
class VideoRental {
  var customerId : Int = 0;
  var movieId : Int = 0;
  var days : Int = 0;
  
  //Requires rd(customerId) && rd(movieId)
  //Ensures rd(customerId) && rd(movieId)
  def FrequentRentalPoints() : Int = customerId+movieId;
  
  
  //Requires rd(customerId) && rd(movieId)
  //Ensures rd(customerId) && rd(movieId)
  def Invoice() : Int = customerId+movieId;
  
}

class VideoStore {
  
  //Requires acc(vr.customerId) && acc(vr.moveiId) && acc(vr.days)
  def Charge(vr : VideoRental) {
    Chalice.fork(vr.FrequentRentalPoints(), "tk0");
    Chalice.fork(vr.Invoice, "tk1");
    var p = Chalice.join(vr.FrequentRentalPoints(), "tk0");
    var r = Chalice.join(vr.Invoice, "tk1");
  }
}

/*
/** Fig. 11 */
class RockBand {
  var memberCount : Int = 0;
  //Monitor invariant valid
  
  //predicate valid acc(memberCount) 
  
  //Requires valid
  //Ensures valid
  def getMemberCount() = {
  	Chalice.unfold(this, "valid");
  	val result=this.memberCount;
  	Chalice.fold(this, "valid");
  	result;
  }
  
  //Requires acc(memberCount)
  //Ensures valid
  def Init() = {
  	memberCount=0;
  	Chalice.fold(this, "valid");
  }
    
  //Requires valid
  //Ensures valid
  def addMembers(howMany : Int) = {
  	Chalice.unfold(this, "valid");
  	memberCount=memberCount+howMany;
  	Chalice.fold(this, "valid");
  }
  
}

/** Fig. 12 */
class Stack{
  var contents : List[Any]= Nil;
  
  //predicate valid acc(contents) 
  
  //Requires valid
  //Ensures valid
  def size() = {
  	Chalice.unfold(this, "valid");
  	val result=contents.length;
  	Chalice.fold(this, "valid");
  	result;
  }
  
  //Requires acc(memberCount)
  //Ensures valid
  def Init() = {
  	contents=Nil;
  	Chalice.fold(this, "valid");
  }
    
  //Requires valid
  //Ensures valid
  def Push(x : Int) = {
  	Chalice.unfold(this, "valid");
  	contents=contents ::: x :: Nil;
  	Chalice.fold(this, "valid");
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
    if(left!=null) Chalice.fork(left.Sum(), "tk");
    if(right!=null) {
      val tmp = right.Sum();
      total=total+tmp;
    }
    if(left!=null) {
      val tmp=Chalice.join(left.Sum(), "tk").asInstanceOf[Int];
      total=total+tmp; 
    }
    Chalice.fold(this, "isTree");
    return total;
  } 
}
  
*/