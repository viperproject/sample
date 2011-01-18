package Examples
//import Chalice._;

class B {
  var x : Int = 0;
  //val v = 0;
  /*def Inc() = {
    Chalice.acquire(this)
    val a=x+1;
    Chalice.release(this)
  }*/
  
  def Numerical(i : Int) = {
	  var j = 1;
	  while(j<i)
	 	  j=j*2;
  }
  
  /*def Try() = {
    val b = new B();
    Chalice.fork(b, "Inc");
    Chalice.fork(b, "Inc");
    Chalice.join(b, "Inc");
    Chalice.join(b, "Inc");
  }*/
  
  /*def Try() {
    val b : B = new B();
    //Chalice.acquire(this)
    //Chalice.unfold(this, "predicate1")
    //this.x=5;
    //Chalice.release(this)
    //Chalice.fold(this, "predicate")
    Chalice.fork(b, "Inc");
    Chalice.fork(b, "Inc");
    Chalice.join(b, "Inc");
    Chalice.join(b, "Inc");
  }*/
}

/*class Node() {
  var next : Node = null;
  var value : Int = 0;
  var sum : Int = 0;
}

class List2(var head : Node, var sum : Int) {
  
  def Insert(x : Int) : Unit = {
    Chalice.acquire(this)
  	var p : Node = head;
    Chalice.acquire(p);
    p.sum=x+p.sum;
    this.sum=x+this.sum;
    Chalice.release(this);
    while(p.next!=null && p.next.value<x) {
      var nx : Node = p.next;
      Chalice.acquire(nx);
      nx.sum=x+nx.sum;
      Chalice.release(p);
      p=nx;
    }
    var t : Node = new Node();
    t.next=p.next;
    t.value=x;
    if(t.next==null) 
      t.sum=0;
    else
      t.sum=p.next.sum+p.next.value;
    Chalice.share(t);
    p.next=t;
    Chalice.release(p);
    
  }
}*/