package test.files

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 10/21/13
 * Time: 6:29 PM
 * To change this template use File | Settings | File Templates.
 */

class SLLNode {
  var next : SLLNode = null
  var value : Int = 0
}

object TestSLLInsertionSort {

  def insertionSort(l: SLLNode) : SLLNode = {
    var currL : SLLNode = l
    var result : SLLNode = null
    var currRes : SLLNode = null
    var prev : SLLNode = null
    var temp : SLLNode = null
    var key: Int = 0

    while (currL != null) {
      key = currL.value
      prev = null
      currRes = result
      while (currRes != null && currRes.value < key) {
        prev = currRes
        currRes = currRes.next
      }

      temp = new SLLNode()
      temp.value = key
      temp.next = currRes

      if (prev == null)
        result = temp
      else
        prev.next = temp

      currL = currL.next
    }

    result
  }

  def main(args: Array[String]) {
    val l = new SLLNode()
    l.value = 5
    l.next = new SLLNode()
    l.next.value = 3
    l.next.next = new SLLNode()
    l.next.next.value = 6
    l.next.next.next = new SLLNode()
    l.next.next.next.value = 1
    val r = insertionSort(l)
    var c = r
    while (c != null) {
      print(c.value + " ")
      c = c.next
    }

  }

}