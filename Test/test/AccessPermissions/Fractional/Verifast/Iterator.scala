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

trait Iterator {
  def hasNext() : Boolean;
  def next() : Object;
}

class SingletonIterator(var value : Object, var done : Boolean) extends Iterator {
  def this() = this(null, true);

  def init(value : Object) = {
    this.value=value;
    done=false;
    Chalice.fold(this, "valid");
  }

  def hasNext() : Boolean = {
    Chalice.unfold(this, "valid");
    val result : Boolean = this.done;
    Chalice.fold(this, "valid");
    return result;
  }

  def next() : Object = {
    Chalice.unfold(this, "valid");
    this.done=true;
    val result : Object = this.value;
    Chalice.fold(this, "valid");
    return result;
  }

}

class IteratorUtil {
  def getLast(iterator : Iterator) : Object = {
    var value : Object = null;
    var more : Boolean = iterator.hasNext();
    while(more) {
      value = iterator.next();
      more = iterator.hasNext();
    }
    return value;
  }
}