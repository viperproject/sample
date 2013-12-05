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

class Node(var total : Int, var parent : Node, var left : Node, var right : Node, var myComp : Set[Node]) {
  def init() {
    this.total=1;
    Chalice.fold(this, "valid");
  }
  def getTotal() : Int = {
    Chalice.unfold(this, "valid");
    val result=this.total;
    Chalice.fold(this, "valid");
    return result;
  }
  def getParent() : Node = {
    Chalice.unfold(this, "valid");
    val result=this.parent;
    Chalice.fold(this, "valid");
    return result;
  }
  def getLeft() : Node = {
    Chalice.unfold(this, "valid");
    val result=this.left;
    Chalice.fold(this, "valid");
    return result;
  }
  def getRight() : Node = {
    Chalice.unfold(this, "valid");
    val result=this.right;
    Chalice.fold(this, "valid");
    return result;
  }
  def add(other : Node) = {
    Chalice.unfold(this, "isComp");
    Chalice.unfold(other, "isComp");
    addCore(this.myComp, other, other.myComp)
    Chalice.fold(this, "isComp");
  }

  def addCore(thisNodes : Set[Node], other : Node, otherNodes : Set[Node]) = {
    Chalice.unfold(thisNodes, "comp");
    Chalice.unfold(otherNodes, "comp");
    other.parent=this;
    this.left=other;
    fixTotal(thisNodes, otherNodes, other.total)
  }

  def fixTotal(nodes : Set[Node], nodes2 : Set[Node], c : Int) : Unit = {
    Chalice.unfold(nodes, "compExcept");
    total = total+c;
    if(parent!=null)
      parent.fixTotal(nodes, nodes2, c);
    Chalice.fold(nodes, "comp");
  }
}