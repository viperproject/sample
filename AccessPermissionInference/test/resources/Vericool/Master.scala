package Vericool

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

class Master(var time : Int) {
  def tick() {
    Chalice.unfold(this, "inv");
    time=time+1;
    Chalice.fold(this, "inv");
  }
  def getTime() : Int = {
    Chalice.unfold(this, "inv");
    val result=time;
    Chalice.fold(this, "inv");
    return result;
  }
}


class Clock(var m : Master, var t : Int) {
  def init(m : Master) = {
    Chalice.unfold(this, "inv");
    this.m=m;
    this.Sync();
    Chalice.fold(this, "inv");
  }
  def Sync() = {
    Chalice.unfold(this, "inv");
    t = m.getTime();
    Chalice.fold(this, "inv");
  }
  def getMaster() : Master = {
    Chalice.unfold(this, "inv");
    val result=m;
    Chalice.fold(this, "inv");
    return result;
  }

  def getT() : Int = {
    Chalice.unfold(this, "inv");
    val result=t
    Chalice.fold(this, "inv");
    return result;
  };

  def isSync() : Boolean = {
    Chalice.unfold(this, "inv");
    val result= t <= m.getTime()
    Chalice.fold(this, "inv");
    return result;
  };


}