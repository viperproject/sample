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

class Node(var value : Int, var next : Node) {
  def this()=this(0, null);
}

class Stack(var head : Node) {
  //predicate valid
  //acc(head)

  //requires acc(head)
  //ensures valid
  def init() = {
    Chalice.fold(this, "valid");
  }

  //requires valid
  //ensures valid
  def push(element : Int) = {
    Chalice.unfold(this, "valid");
    val n : Node = new Node();
    n.value=element;
    n.next=head;
    head=n;
    Chalice.fold(this, "nodes");
    Chalice.fold(this, "valid");
  }

  //requires valid
  //ensures valid
  def pop() : Int = {
    Chalice.unfold(this, "valid");
    Chalice.unfold(this, "nodes");
    val result : Int = head.value;
    head=head.next;
    Chalice.fold(this, "valid");
    return result;
  }
}