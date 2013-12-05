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

/*
class Client {
  def Main(d: Data)
  {
    var a = new AssociationList
    a.Init()
    a.Add(5, d)
    a.Add(10, d)
    var t: Data = a.Get(10)
  }
}
*/
class AssociationList {
  var head: NodeAssociationList = null  // sentinel
  //invariant rd(head)

  def Init() = 
    //requires acc(head)
  {
    head = new NodeAssociationList
    head.next = null
    Chalice.share(head)
    Chalice.share(this)
  }

  def Add(key: Int, value: Data)
  {
    Chalice.acquire(this)
    var p: NodeAssociationList = head
    Chalice.acquire(p)
    Chalice.release(this)

    var n = new NodeAssociationList
    n.key = key
    n.value = value
    n.next = p.next
    p.next = n
    Chalice.share(n)
    Chalice.release(p)
  }
  
  def Get(key: Int) : Data = 
  {
    var d : Data = null
    Chalice.acquire(this)
    var p: NodeAssociationList = head
    Chalice.acquire(p)
    Chalice.release(this)

    if (p.next != null) {
      Chalice.acquire(p.next)
      if (p.next.key == key) {
        d = p.next.value
      } else {
        var done = false
        while (!done)
          //invariant rd(p.key) && rd(p.value) && acc(p.next)
          //invariant rd(p.next.key) && rd(p.next.value) && acc(p.next.next)
        {
          if (p.next.next == null) {
            done = true  // key not present
          } else {
            Chalice.acquire(p.next.next)
            if (p.next.next.key == key) {
              done = true  // key is present
              d = p.next.next.value
              // move p.next.next closer to the head by one step

              var t: NodeAssociationList = p.next
              p.next = t.next
              t.next = p.next.next
              p.next.next = t
              Chalice.release(t)
            } else {
              var t: NodeAssociationList = p
              p = p.next
              Chalice.release(t)
            }
          }
        }
      }
      Chalice.release(p.next)
    }
    Chalice.release(p)
    return d
  }
}

class Data { }

class NodeAssociationList
{
  var key: Int = 0
  var value: Data = null
  var next: NodeAssociationList = null
  //invariant rd(key) && rd(value) && acc(next)
}
