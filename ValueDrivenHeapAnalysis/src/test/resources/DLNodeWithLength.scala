package test.files

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 10/24/13
 * Time: 9:27 AM
 * To change this template use File | Settings | File Templates.
 */
class DLNodeWithLength {
  var next : DLNodeWithLength = null
  var prev : DLNodeWithLength = null
  var length : Int = 0
}

object CreateList {

  def createListOfLength(k : Int) : DLNodeWithLength = {
    var result : DLNodeWithLength = null
    var temp : DLNodeWithLength = null
    var i : Int = 1
    while (i <= k) {
      temp = new DLNodeWithLength()
      temp.next = result
      temp.prev = null
      temp.length = i
      if (result != null)
        result.prev = temp
      result = temp
      i = i + 1
      temp = null
    }
    result
  }

  def printList(l : DLNodeWithLength) {
    var curr : DLNodeWithLength = l
    while (curr != null) {
      print(curr.length + " ")
      curr = curr.next
    }
    println()
  }

  def main(args: Array[String]) {
    val l = createListOfLength(0)
    printList(l)
  }

}
