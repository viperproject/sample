import Chalice._;

object Chalice {
  def acquire(obj : Any) : Unit = {}
  def release(obj : Any) : Unit = {}
  def share(obj : Any) : Unit = {}
  def unshare(obj : Any) : Unit = {}
  def free(obj : Any) : Unit = {}
  def fold(objThis : Any, pred : String) : Unit = {}
  def unfold(objThis : Any, pred : String) : Unit = {}
  def fork(obj : Any, method : String) : Unit = {}
  def join(obj : Any, method : String) : Unit = {}
}


class Cell {
  var x : Int = 0;

  //def get() : Int = return x;
  //def set(v : Int) = x=v;
  def set(x : Int) = {
    Chalice.acquire(this);
    //this.set(x);
    this.x=x;
    Chalice.release(this);
  }

  /*def twoParallelSets(x1 : Int, x2 : Int) = {
    Chalice.fork(this, "set")
	  Chalice.fork(this, "set")
	  Chalice.join(this, "set")
	  Chalice.join(this, "set")
  } */
}