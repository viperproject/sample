package ChaliceExamples

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


class C {
  var F : Any = null;
  var G : Any = null;
  def n()
    //requires acc(F) && acc(this.G);
    //ensures acc(F) && acc(G);
  {
    var tmp = F;
    F = G;
    G = tmp;
  }
}
