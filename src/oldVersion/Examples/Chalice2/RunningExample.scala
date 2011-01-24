package Examples.Chalice2
import Chalice._

class Cell {
	var x : Int = 0;
	var c1 : Int = 0;
	var c2 : Int = 0;
}

class Worker1(var c : Cell) {
	def Inc() {
		Chalice.acquire(c);
		c.x = c.x+1;
		c.c1 = c.c1+1;
		Chalice.release(c)
		c.c1;//it simulates the ensure
	}
}

class Worker2(var c : Cell) {
	def Inc() {
		Chalice.acquire(c);
		c.x = c.x+1;
		c.c2 = c.c2+1;
		Chalice.release(c)
		c.c2;//it simulates the ensure
	}
}

object RunningExample {
  def main() : Unit = {
	  val c : Cell = new Cell();
	  Chalice.share(c);
	  val w1 : Worker1 = new Worker1(c);
	  w1.c = c;
	  val w2 : Worker2 = new Worker2(c);
	  w2.c = c;
	  Chalice.fork(w1, "Inc")
	  Chalice.fork(w2, "Inc")
	  Chalice.join(w1, "Inc")
	  Chalice.join(w2, "Inc")
	  Chalice.acquire(c);
	  c.x ;//it simulates the assert
  }
}
