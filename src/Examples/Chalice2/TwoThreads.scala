package Examples.Chalice2
import Chalice._;
import Examples._;

class TwoThreads {
  var x : Int = 0;
  def Inc() = {
    Chalice.acquire(this)
    x=x+1;
    Chalice.release(this)
  }
  
  def Try() {
    val b : B = new B();
    Chalice.fork(b, "Inc");
    Chalice.fork(b, "Inc");
    Chalice.join(b, "Inc");
    Chalice.join(b, "Inc");
  }
}
