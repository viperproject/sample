package test.files

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 10/21/13
 * Time: 6:58 PM
 * To change this template use File | Settings | File Templates.
 */
object InsertionSort {

  def insertionSort(l: SLNode) : SLNode = {
    var currL : SLNode = l
    var result : SLNode = null
    var currRes : SLNode = null
    var prev : SLNode = null
    var temp : SLNode = null
    var key: Int = 0

    while (currL != null) {
      key = currL.value
      prev = null
      currRes = result
      while (currRes != null && currRes.value < key) {
        prev = currRes
        currRes = currRes.next
      }

      temp = new SLNode()
      temp.value = key
      temp.next = currRes

      if (prev == null)
        result = temp
      else
        prev.next = temp

      currL = currL.next
    }

    return result
  }

  def main(args: Array[String]) {
    val l = new SLNode()
    l.value = 5
    l.next = new SLNode()
    l.next.value = 3
    l.next.next = new SLNode()
    l.next.next.value = 6
    l.next.next.next = new SLNode()
    l.next.next.next.value = 1
    val r = insertionSort(l)
    var c = r
    while (c != null) {
      print(c.value + " ")
      c = c.next
    }

  }
}
