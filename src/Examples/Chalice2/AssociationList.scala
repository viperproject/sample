package Examples.Chalice2

import Chalice._

class Data

class Node {
  var key : Int = 0
  var value : Data = new Data()
  var next : Node = new Node()
}

class AssociationList {
  var head : Node = new Node
  
  /*
  def Add(key : Int, value : Data) = {
    Chalice.acquire(this);
    var p : Node = head;
    Chalice.release(this);
    
    Chalice.acquire(p);
    var n = new Node;
    n.key=key;
    n.value=value;
    n.next=p.next;
    p.next=n;
    Chalice.release(p); 
  }
*/
  def Get(key : Int) : Unit = {
    var d : Data = null;
    //Chalice.acquire(this);
    var p : Node = head;
    //Chalice.release(this);
    while(/*p!=null &&*/ d==null) {
      //val temp=p;
      //Chalice.acquire(temp);
      if(p.key==key)
        d=p.value;
      else
        p=p.next;
      //Chalice.release(temp);
    }
    //return d;
  }
}
