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

class List {
  var sum: Int = 0
  var head: Node = null
  //invariant acc(head) && rd(head.val) acc(sum,20) && acc(head.sum, 50)

  def Main()
  {
    var list = new List
    list.Init()
    //list.Insert(8)
    //list.Insert(12)
    //list.Insert(4)
    //assert list.sum == 24
  }

  def Init()
    //requires acc(head) && acc(sum)
    //ensures acc(sum,80)
  {
    var t = new Node
    t.value = -1
    t.next = null
    t.sum = 0
    Chalice.share(t)
    head = t
    sum = 0
    Chalice.share(this)
  }
 
  def Insert(x: Int)
    //requires acc(sum,80)
    //ensures acc(sum,80)
  {
    Chalice.acquire(this)
    sum = sum + x
    var p : Node = head
    Chalice.acquire(p)
    p.sum = p.sum + x
    Chalice.release(this)

    while (p.next != null && p.next.value < x)
      //invariant acc(p.next) && rd(p.val)
      //invariant rd(p.next.value)
      //invariant acc(p.sum, 50)
      //acc(p.next.sum, 50)
    {
      var nx: Node = p.next
      Chalice.acquire(nx)
      nx.sum = nx.sum + x
      Chalice.release(p)
      p = nx
    }
    var t = new Node;
    t.value = x
    t.next = p.next
    if (t.next == null) { t.sum = 0 } else { t.sum = p.next.value + p.next.sum }
    p.next = t
    Chalice.release(p)
  }


  def Delete(x: Int)
    //requires acc(sum,80)
    //ensures acc(sum,80)
  {
    var c = 10; //It was a ghost const
    var wasPresent : Boolean = true;//the returned value
    
    Chalice.acquire(this)
    sum = sum - c
    var p: Node = head
    Chalice.acquire(p)
    p.sum = p.sum - c
    Chalice.release(this)

    while (p.next != null && p.next.value < x)
      //invariant acc(p.next) && rd(p.val)
      //invariant rd(p.next.val)
      //invariant acc(p.sum, 50)
      //invariant acc(p.next.sum, 50)
    {
      var nx: Node = p.next
      Chalice.acquire(nx)
      nx.sum = nx.sum - c
      Chalice.release(p)
      p = nx
    }
    if (p.next != null && p.next.value == x) {
      wasPresent = true
      c = x
      var nx: Node = p.next
      Chalice.acquire(nx)
      p.next = nx.next
      Chalice.unshare(nx)
    } else {
      wasPresent = false
      c = 0
    }
    Chalice.release(p)
    return wasPresent
  }
}

class Node {
  var sum: Int = 0
  var value: Int = 0
  var next: Node = null
  //invariant acc(next) && rd(value)
  //invariant rd(next.value)
  //invariant acc(sum, 50)
  //invariant acc(next.sum, 50)
}
