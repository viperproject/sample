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

class Test1 {
  var x : Int=0;
  var y : Int = 0;
  def setX(i : Int) = {
    this.x=i;
  }
  def getX() : Int= {
    this.x;
  }

  def swap() = {
    val temp = this.x;
    this.x = this.y;
    this.y = temp;
  }

  def concurrentSetX(i : Int) = {
    Chalice.acquire(this);
    this.x=i;
    Chalice.release(this);
  }

  def concurrentSetYWithPredicates(i : Int) = {
    Chalice.unfold(this, "p");
    Chalice.acquire(this);
    this.y=i;
    Chalice.release(this);
    Chalice.fold(this, "p");
  }

  def concurrentSetXWithPredicates(i : Int) = {
    Chalice.unfold(this, "p");
    Chalice.acquire(this);
    this.y=i;
    Chalice.release(this);
    Chalice.fold(this, "p");
  }

  def loopSetX( i : Int) = {
     while(this.y < i)
       this.x=i;
  }

}