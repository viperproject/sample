package test.files

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 10/21/13
 * Time: 6:02 PM
 * To change this template use File | Settings | File Templates.
 */
class DLNode {
  var next : DLNode = null
  var prev : DLNode = null
  var value : Int = 0
}

class SortedDLL {
  var head : DLNode = null

  def insertKey(key : Int) {
    var p: DLNode = null
    var c: DLNode = head
    var t: DLNode = null

    while (c != null && c.value < key) {
      p = c
      c = c.next
    }

    t = new DLNode()
    t.value = key
    t.next = c
    t.prev = p

    if (p == null)
      head = t
    else
      p.next = t

    if (c != null)
      c.prev = t
  }

  def deleteKey(key : Int) {
    var p: DLNode = null
    var c: DLNode = head

    while (c != null && c.value < key) {
      p = c
      c = c.next
    }

    if (c != null && c.value == key) {
      if (p == null)
        head = c.next
      else
        p.next = c.next
      if (c.next != null)
        c.next.prev = p
    }
  }

  def deepCopy() : SortedDLL = {
    var p: DLNode = null
    var c : DLNode = head
    var l : DLNode = null
    val result : SortedDLL = new SortedDLL()
    var key : Int = 0

    while (c != null) {
      key = c.value
      if (result.head == null) {
        result.head = new DLNode()
        l = result.head
      } else {
        p = l
        l.next = new DLNode()
        l = l.next
      }
      l.prev = p
      l.value = key
      c = c.next
    }

    result
  }

  def printDLL() {
    var curr: DLNode = head

    while (curr != null) {
      print(curr.value + " ")
      curr = curr.next
    }
  }
}

object TestSortedDLL {

  def main(args: Array[String]) {
    val list : SortedDLL = new SortedDLL()
    list.insertKey(1)
    list.insertKey(3)
    list.insertKey(2)
    list.deleteKey(2)
    list.printDLL()
    println()
    val copyL = list.deepCopy()
    copyL.printDLL()
  }

}
