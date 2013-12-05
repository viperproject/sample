package ChaliceExamples;

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


class TwoThreads {
  var x : Int = 0;
  def Inc() = {
    Chalice.acquire(this)
    x=x+1;
    Chalice.release(this)
  }
  
  def Try() {
    val b : TwoThreads = new TwoThreads();
    Chalice.fork(this, "Inc");
    Chalice.fork(this, "Inc");
    Chalice.join(this, "Inc");
    Chalice.join(this, "Inc");
    if(this.x!=2)
    	throw new Exception("Error");
  }
}
