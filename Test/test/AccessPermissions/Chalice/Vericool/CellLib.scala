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

class Cell(var x : Int) {
  def setX(v : Int) {
    Chalice.unfold(this, "inv");
    this.x=v;
    Chalice.fold(this, "inv");
  }
  def swap(c : Cell) {
    Chalice.unfold(this, "inv");
    Chalice.unfold(c, "inv");
    val t : Int = this.getX();
    val t2 : Int = c.getX();
    this.setX(t2);
    c.setX(t);
    Chalice.fold(c, "inv");
    Chalice.fold(this, "inv");
  }
  def getX() : Int = {
    Chalice.unfold(this, "inv");
    val result=this.x;
    Chalice.fold(this, "inv");
    return result;
  }
}
class ReCell(var bak : Int, var base : Cell) {
  def getX(v : Int) {
    Chalice.unfold(this, "inv");
    bak=base.getX();
    base.setX(v);
    Chalice.fold(this, "inv");
  }
  def swap(c : Cell) {
    Chalice.unfold(this, "inv");
    base.swap(c);
    Chalice.fold(this, "inv");
  }
  def getX() : Int = {
    Chalice.unfold(this, "inv");
    val result=base.getX();
    Chalice.fold(this, "inv");
    return result;
  }
  def getBackup() : Int = {
    Chalice.unfold(this, "inv");
    val result=bak;
    Chalice.fold(this, "inv");
    return result;
  }
  def undo() : Int = {
    Chalice.unfold(this, "inv");
    base.setX(bak);
    val result : Int = bak;
    Chalice.fold(this, "inv");
    return result;
  }
}

class TCell(var val2 : Int, var base : Cell) {
  def setX(v : Int) {
    Chalice.unfold(this, "inv");
    val2=v;
    base.setX(v);
    Chalice.fold(this, "inv");
  }
  def swap(c : Cell) {
    Chalice.unfold(this, "inv");
    base.swap(c);
    Chalice.fold(this, "inv");
  }
  def getX() : Int = {
    Chalice.unfold(this, "inv");
    val result=base.getX();
    Chalice.fold(this, "inv");
    return result;
  }
  def check() {
    Chalice.unfold(this, "inv");
    val temp : Int = base.getX();
    if(val2!=temp)
      throw new Exception();
    Chalice.fold(this, "inv");
  }
}

class DCell(var base : Cell) {
  def setX(v : Int) {
    Chalice.unfold(this, "inv");
    base.setX(v*2);
    Chalice.fold(this, "inv");
  }
  def swap(c : Cell) {
    Chalice.unfold(this, "inv");
    base.swap(c);
    Chalice.fold(this, "inv");
  }
  def getX() : Int = {
    Chalice.unfold(this, "inv");
    val result=base.getX();
    Chalice.fold(this, "inv");
    return result;
  }
}