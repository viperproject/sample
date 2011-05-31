package Verifast

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

class Counter(var value : Int, var c1 : Int, var c2 : Int) {
  def this()=this(0, 0, 0);

  def init(v : Int) = {
    this.value=v;
  }

  def increment() = {
    this.value=this.value+1;
  }

}

class Semaphore

class Session(var c : Counter, var lock : Semaphore, var first : Boolean) {
  def this() = this(null, null, true);
  def init(c : Counter, lock : Semaphore, first : Boolean) = {
    this.c=c;
    this.lock=lock;
    this.first=first;
  }

  def run() = {
    Chalice.unfold(this, "Session");
    Chalice.acquire(lock);
    this.c.increment();
    if(this.first)
      c.c1=c.c1+1;
    else c.c2=c.c2+1;
    Chalice.fold(c, "Counter");
    Chalice.release(lock);
    Chalice.fold(this, "Session");
  }

  def main() = {
    val c : Counter = new Counter();
    c.init(0);
    Chalice.fold(c, "Counter");
    val lock : Semaphore = new Semaphore();
    val session1 : Session = new Session();
    session1.init(c, lock, true);
    Chalice.fold(session1, "Session");
    Chalice.fork(session1, "run");
    val session2 : Session = new Session();
    session1.init(c, lock, false);
    Chalice.fold(session2, "Session");
    Chalice.fork(session2, "run");
    Chalice.join(session1, "run");
    Chalice.join(session2, "run");
    Chalice.unfold(session1, "Session");
    Chalice.unfold(session2, "Session");
    Chalice.unfold(c, "Counter");
    c.value;
  }

}