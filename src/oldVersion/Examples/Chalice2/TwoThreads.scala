package Examples.Chalice2
import Chalice._;

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
