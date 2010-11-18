package Examples.Chalice2

import Chalice._

class Cell {
  var x: Int = 0;

  def init(v: Int)
    //requires acc(this.x)
    //ensures valid
  {
    x = v;
    Chalice.fold(this, "valid");
  }

  def set(v: Int)
    //requires valid
    //ensures valid
  {
    Chalice.unfold(this, "valid");
    x = v;
    Chalice.fold(this, "valid");
  }
  
  def increment()
    //requires valid;
    //ensures valid;
  {
    Chalice.unfold(this, "valid");
    x = x + 1;
    Chalice.fold(this, "valid");
  }

  def dispose()
    //requires valid
  {
    Chalice.unfold(this, "valid");
    Chalice.free(this);
  }

  def get(): Int = 
    //requires valid;
  	//ensures valid;
  {
    Chalice.unfold(this, "valid") 
    val result = x;
    Chalice.fold(this, "valid")
    return result;
  }

  //predicate valid {
  //  acc(this.x)
  //}

  //invariant valid;
}

class Interval {
  var left: Cell = null;
  var right: Cell = null;

  def init(l: Int, r: Int)
    //requires acc(left) && acc(right);
    //ensures valid;
  {
    left = new Cell;
    left.init(l);
    right = new Cell;
    right.init(r);
    Chalice.fold(this, "valid");
  }

  def setLeft(l: Int)
    //requires valid;
    //ensures valid;
  {
    Chalice.unfold(this, "valid");
    left.set(l);
    Chalice.fold(this, "valid");
  }

  def setRight(r: Int)
    //requires valid;
    //ensures valid;
  {
    Chalice.unfold(this, "valid");
    right.set(r);
    Chalice.fold(this, "valid");
  }

  def shift(v: Int)
    //requires valid;
    //ensures valid;
  {
    Chalice.unfold(this, "valid");
    left.set(left.get()+v);
    right.set(right.get()+v);
    Chalice.fold(this, "valid")
  }

  def getLeft() : Int = 
    //requires valid;
  {
    Chalice.unfold(this, "valid");
    val result=left.get()
    Chalice.fold(this, "valid");
    return result;
  }

  def getRight() : Int = 
    //requires valid;
  {
    Chalice.unfold(this, "valid");
    val result=right.get()
    Chalice.fold(this, "valid");
    return result;
  }
  
  //predicate valid
  //{
  //  acc(left) && acc(right) && left.valid && right.valid
  //}
}

class Program {
  def main(){
    var c1 = new Cell;
    c1.init(5);
    c1.set(6);

    var c2 = new Cell;
    c2.init(10);
    c2.set(11);

    //assert c1.get() == 6;
  }

  def main2(){
    var c: Cell = new Cell;
    c.init(0);
    c.dispose();

    //assert c.valid; // should fail
  }

  def main3() : Cell = 
    //ensures rt.valid
  {
    var rt = new Cell;
    rt.init(0);
    return rt;
  }

  def main4() {
    var c: Cell = new Cell;
    c.init(0);
    Chalice.share(c);

    Chalice.acquire(c);
    c.set(1);
    Chalice.release(c);
  }
}