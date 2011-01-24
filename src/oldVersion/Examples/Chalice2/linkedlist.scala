package Examples.Chalice2

import Chalice._;


class NodeCell {
  var next: NodeCell = null;
  var value: Int = 0;

  def init(v: Int)
    //requires acc(next) && acc(value);
    //ensures valid
  {
    next = null;
    value = v;
    Chalice.fold(this, "valid");
  }

  def add(x: Int) : Unit =
    //requires valid;
    //ensures valid;
  {
    Chalice.unfold(this, "valid");
    if(next==null) {
      var n : NodeCell = new NodeCell;
      n.init(x);
      next = n;
    } else {
      next.add(x);
    }
    Chalice.fold(this, "valid");
  }

  def addFirst(x: Int) : NodeCell =
    //requires valid;
    //result.valid;
  {
    var n : NodeCell = new NodeCell;
    n.value = x;
    n.next = this; 
    Chalice.fold(n, "valid");
    return n;
  }

  def at(i: Int): Int = 
    //requires valid
  {
    Chalice.unfold(this, "valid")
    val result = if(i==0) value else next.at(i-1)
    Chalice.fold(this, "valid")
    return result;
  }

  def size(): Int =
    //requires valid;
  {
    Chalice.unfold(this, "valid") 
    val result = if(next!=null) 1+ next.size() else 1
    Chalice.fold(this, "valid")
    return result;
  }

  //predicate valid {
  //  acc(next) && acc(value)
  //}
}